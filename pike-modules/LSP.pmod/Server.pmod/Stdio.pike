inherit .Base : base;

private Thread.Thread handler_thread;
private Stdio.File input;
private Stdio.File output;

public void start() {
  handler_thread = thread_create(start_handler, Stdio.stdin, Stdio.stdout);
}

public void stop(int code) {
  handler_thread && handler_thread->kill();
  exit(code);
}

public variant void stop() {
  stop(0);
}

protected void send_response(mapping message, void|JsonRpc.Id id) {
  string encoded_message =
    base::encode_response_message_with_header(message, id);
  werror("send_response() -> %O\n", encoded_message);
  output->write(encoded_message);
}

protected void send_error(JsonRpc.JsonRpcError err) {
  string encoded_error = base::encode_response_error_with_header(err);
  werror("send_error() -> %O\n", encoded_error);
  output->write(encoded_error);
}

protected void start_handler(Stdio.File input, Stdio.File output) {
  this::input = input;
  this::output = output;

  while (true) {
    .Request req = .parse_raw_request(input);

    if (req) {
      if (mixed err = catch(base::handle_request(req))) {
        werror("An error occured, send error response: %O\n", err);
        send_error(err);
      }
    } else {
      werror("Undhandled request!\n");
      break;
    }
  }

  stop();
}
