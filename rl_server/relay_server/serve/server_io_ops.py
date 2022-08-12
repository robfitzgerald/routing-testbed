import logging

log = logging.getLogger(__name__)


def send_success(handler, proto_obj):
    log.debug(f"sending response with object {proto_obj}")

    handler.send_response(200)
    handler.send_header('Content-type', 'application/x-protobuf')
    handler.end_headers()

    # proto_obj should be a Google protobuf object
    # https://developers.google.com/protocol-buffers/docs/proto3
    body = proto_obj.SerializeToString()

    handler.wfile.write(body)


def send_failure(handler, exception, message):
    log.error(exception)

    handler.send_response(400)
    handler.send_header('Content-type', 'text/html')
    handler.end_headers()

    handler.wfile.write(bytes(message, "utf8"))

