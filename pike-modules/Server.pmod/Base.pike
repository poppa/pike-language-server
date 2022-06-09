public void start();
public void stop();

protected string encode_response_message(mapping message, void|JsonRpc.Id id) {
  string data = Standards.JSON.encode(
    (mapping)JsonRpc.make_response_message(message, id)
  );
  return sprintf("Content-Length: %d\r\n\r\n%s", sizeof(data), data);
}

protected string encode_response_error(JsonRpc.JsonRpcError err) {
  string data = Standards.JSON.encode(
    (mapping)JsonRpc.make_response_error(err)
  );
  return sprintf("Content-Length: %d\r\n\r\n%s", sizeof(data), data);
}
