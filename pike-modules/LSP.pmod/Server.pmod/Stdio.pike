#include "server.h"

inherit .Base : base;

private Thread.Thread handler_thread;
private Stdio.File input;
private Stdio.File output;

public void start() {
  handler_thread = thread_create(start_handler, Stdio.stdin, Stdio.stdout);
}

public void stop(int code) {
  handler_thread?->kill();
  exit(code);
}

public variant void stop() {
  stop(0);
}

public void send_response(mapping message, void|JsonRpc.Id id) {
  string encoded_message =
    base::encode_response_message_with_header(message, id);
  DEBUG("send_response() -> %O\n", encoded_message);
  output->write(encoded_message);
}

public void send_error(JsonRpc.Error err) {
  string encoded_error = base::encode_response_error_with_header(err);
  DEBUG("send_error() -> %O\n", encoded_error);
  output->write(encoded_error);
}

protected void start_handler(Stdio.File input, Stdio.File output) {
  this::input = input;
  this::output = output;

  while (true) {
    .Request req = .parse_raw_request(input);

    DEBUG("Raw Request: %O\n", req);

    if (req) {
      if (mixed err = catch(base::handle_request(req))) {
        DEBUG("An error occured in handle_request: %O\n", err);
        if (arrayp(err)) {
          err = JsonRpc.Error(JsonRpc.INTERNAL_ERROR, describe_backtrace(err));
        }


        send_error(err);
      }
    } else {
      werror("Undhandled request!\n");
      break;
    }
  }

  stop();
}
