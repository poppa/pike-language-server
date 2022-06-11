inherit LSP.EventEmitter;

private LSP.Server.Base connection;
private mapping(.DocumentUri:object(.Document)) _documents = ([]);

protected void create(LSP.Server.Base connection) {
  this::connection = connection;
  this::connection.on("textDocument/didOpen", on_did_open);
}

public array(object(.Document)) `documents() {
  return values(_documents) + ({});
}

public bool has_document(.DocumentUri uri) {
  return has_index(_documents, uri);
}

public variant bool has_document(.Document doc) {
  return has_document(doc->uri);
}

public variant bool has_document(mapping in) {
  if (has_index(in, "uri")) {
    return has_document(in->uri);
  }

  if (string uri = LSP.Tools.deep_find_property(in, "uri")) {
    return has_document(uri);
  }

  return false;
}

private void on_did_open(mixed arg) {
  werror("Got on_did_open callback: %O\n", arg);
}
