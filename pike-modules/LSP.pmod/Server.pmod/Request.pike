public mapping(string:string) headers;
public mapping body;

protected void create(mapping(string:string) headers, mapping|string body) {
  this::headers = headers;
  this::body = stringp(body) ? Standards.JSON.decode(body) : body;
}

public JsonRpc.Message `rpc_message() {
  return JsonRpc.request_to_instance(body);
}
