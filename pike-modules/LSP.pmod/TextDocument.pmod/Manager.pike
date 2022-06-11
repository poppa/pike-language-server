inherit LSP.EventEmitter;

private LSP.Server.Base connection;
private mapping(.DocumentUri:object(.Document)) _documents = ([]);

protected void create(LSP.Server.Base connection) {
  this::connection = connection;
  this::connection
    .on("textDocument/didOpen", on_did_open)
    .on("textDocument/didChange", on_did_change)
    .on("textDocument/didClose", on_did_close);
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

public .Document get_document(.DocumentUri uri) {
  return _documents[uri];
}

public variant .Document get_document(mapping in) {
  string uri = LSP.Tools.deep_find_property(in, "uri");
  return _documents[uri];
}

protected void on_did_open(mapping params, void|JsonRpc.Id id) {
  .Document doc = .create_document(params->textDocument);

  if (has_document(doc)) {
    error("Document already handled by manager\n");
  }

  _documents[doc->uri] = doc;
}

protected void on_did_close(mapping params) {
  string uri = LSP.Tools.deep_find_property(params, "uri");

  if (has_document(uri)) {
    m_delete(_documents, uri);
  } else {
    error("Trying to close an already closed document: %O\n", uri);
  }
}

public void on_did_change(mapping message) {
  werror("Got on change event: %O\n", message);

  mapping td = message->textDocument;
  object doc = get_document(td->uri);

  if (!doc) {
    werror("Document not found!!!\n");
  }

  doc->update(td->contentChanges, td->version);
}
