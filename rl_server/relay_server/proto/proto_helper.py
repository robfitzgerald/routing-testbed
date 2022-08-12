from abc import abstractmethod

from google.protobuf.message import DecodeError
import relay_server.serve.server_io_ops as ops
import relay_server.proto.external_assignment_v1_pb2 as proto
import logging

log = logging.getLogger(__name__)


class ProtoHelper:

    def __init__(self,
                 action_request_constructor,
                 action_response_constructor,
                 reward_request_constructor,
                 reward_response_constructor):
        self.act_req_c = action_request_constructor
        self.act_res_c = action_response_constructor
        self.rew_req_c = reward_request_constructor
        self.rew_res_c = reward_response_constructor

    @abstractmethod
    def perform_operation(self, req):
        """
        perform the action - generating actions, or, updating the model with rewards
        :param req: the parsed, protobuf-generated request class
        :return: the response object - an ActionResponse or RewardResponse generated protobuf class
        :raises: Exception
        """
        pass

    def handle_request(self, handler):
        try:
            content_len = int(handler.headers.get('Content-Length'))
            body = handler.rfile.read(content_len)
        except Exception as e1:
            ops.send_failure(self, e1, f"failed to read post body due to: {e1}")
            body = None

        if body is not None:
            try:
                req = self.parse_to_post_request_type(body)
                res = self.perform_operation(req)
                ops.send_success(handler, res)

            except Exception as e2:
                ops.send_failure(handler, e2, f"failed to read post body due to: {e2}")

    def parse_to_post_request_type(self, body):
        """
        parse the provided body into either an ActionRequest or RewardRequest object
        :param body:
        :return:
        :raises: IOError when the provided body does not parse into the expected types
        """
        try:
            req = self.act_req_c()
            req.ParseFromString(body)
            log.debug(f"action request received: {req}")
            return req

        except DecodeError as e1:

            # may be a reward request
            try:
                req = self.rew_req_c()
                req.ParseFromString(body)
                log.debug(f"action request received: {req}")
                return req
            except DecodeError as e2:
                msg = f"could not parse request as ActionRequest ({e1}) or RewardRequest ({e2})"
                raise IOError(msg) from e2

