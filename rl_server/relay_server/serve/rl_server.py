import os
from http.server import HTTPServer, CGIHTTPRequestHandler
from functools import partial

# Make sure the server is created at current directory

from relay_server.proto.proto_helper import ProtoHelper
from request_handler import RequestHandler
import relay_server.proto.external_assignment_v1_pb2 as proto

if __name__ == "__main__":
    # os.chdir('')
    # Create server object listening the port 80
    helper = ProtoHelper(
        action_request_constructor=proto.ActionRequest,
        action_response_constructor=proto.ActionResponse,
        reward_request_constructor=proto.RewardRequest,
        reward_response_constructor=proto.RewardResponse
    )
    handler = partial(RequestHandler, helper)
    server_object = HTTPServer(server_address=('', 46368), RequestHandlerClass=handler)
    # Start the web server
    server_object.serve_forever()
