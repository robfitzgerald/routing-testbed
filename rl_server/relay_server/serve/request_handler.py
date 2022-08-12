from http.server import BaseHTTPRequestHandler

import relay_server.proto.external_assignment_v1_pb2 as proto
import relay_server.serve.server_io_ops as ops
from relay_server.proto.proto_helper import ProtoHelper


class RequestHandler(BaseHTTPRequestHandler):

    def __init__(self,
                 proto_helper: ProtoHelper,
                 *args,
                 **kwargs):
        self.proto_helper = proto_helper
        super(RequestHandler, self).__init__(*args, **kwargs)

    def do_GET(self):

        self.send_response(200)
        self.send_header('Content-type', 'application/x-protobuf')
        self.end_headers()

        res = proto.ActionResponse()
        a1 = proto.ActionSelection()
        a1.agent = "bob"
        a1.action = 3
        a2 = proto.ActionSelection()
        a2.agent = "alice"
        a2.action = 5
        res.actions = [a1, a2]

        message = res.SerializeToString()
        self.wfile.write(message)

    def do_POST(self):

        self.proto_helper.handle_request(self)

        # try:
        #     content_len = int(self.headers.get('Content-Length'))
        #     body = self.rfile.read(content_len)
        # except Exception as e:
        #     ops.send_failure(self, e, f"failed to read post body due to: {e}")
        #
        # # attempt to parse using one of the supported proto message types
        #
        # try:
        #
        #     req = proto.ModelRequest()
        #     req.ParseFromString(body)
        #
        #     print("ModelRequest received:")
        #     print(req)
        #
        #     self.send_response(200)
        #     self.send_header('Content-type', 'application/x-protobuf')
        #     self.end_headers()
        #
        #     res = proto.ModelResponse()
        #     res.a = req.y + 1
        #     res.b = req.z + 1
        #
        #     print("sending ModelResponse:")
        #     print(res)
        #
        #     message = res.SerializeToString()
        #
        #     print("serialized response:")
        #     print(message)
        #
        #     self.wfile.write(message)
        #
        # except Exception as e:
        #     print(e)
        #
        #     self.send_response(400)
        #     self.send_header('Content-type', 'text/html')
        #     self.end_headers()
        #
        #     message = f"invalid request failed due to: {e}"
        #     self.wfile.write(bytes(message, "utf8"))

