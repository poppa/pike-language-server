//! @url{https://microsoft.github.io/language-server-protocol/specifications/\
//!lsp/3.17/specification/#initializeParams@}
protected mapping init_params;
protected bool is_initialized = false;

protected mapping(string:object(LSP.TextDocument)) text_documents = ([]);

protected void create() {}

public void start();
public void stop();
protected void send_response(mapping message, void|JsonRpc.Id id);

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

public void handle_request(.Request request) {
  JsonRpc.Message rpc = request->rpc_message;

  werror("handle_request(%O:%O)\n", rpc->id, rpc->method);

  if (!is_initialized && !(< "initialize", "initialized" >)[rpc->method]) {
    throw(JsonRpc.JsonRpcError(
      .ERR_SERVER_NOT_INITIALIZED,
      "Received request before initialize",
      rpc,
    ));
  }

  string method_name = "on_" + rpc->method;
  mixed cb = this[method_name];

  if (callablep(cb)) {
    cb(rpc);
    return;
  }

  switch (rpc->method) {
    case "textDocument/didChange":
      on_text_document_did_change(rpc);
      return;
    case "textDocument/didOpen":
      on_text_document_did_open(rpc);
      return;
    case "textDocument/didClose":
      on_text_document_did_close(rpc);
      return;

  }

  throw(JsonRpc.JsonRpcError(
    JsonRpc.METHOD_NOT_FOUND,
    sprintf("The method %O does not exist or is not available.", rpc->method),
    rpc,
  ));
}

public void on_initialize(JsonRpc.NotificationMessage message) {
  init_params = message->params;
  mapping pcaps = init_params->capabilities;
  mapping caps = ([
    "textDocumentSync": 2,
    // Well, not yet, but do activate some communication...
    "completionProvider": ([
      "resolveProvider": true,
    ])
  ]);

  if (has_workspace_folder_capability) {
    caps->workspace = ([
      "workspaceFolders": ([
        "supported": true
      ])
    ]);
  }

  send_response(
    ([
      "capabilities": caps,
      "serverInfo": ([
        "name": .NAME,
        "version": .VERSION,
      ])
    ]),
    message->id
  );
}

public void on_initialized(JsonRpc.NotificationMessage message) {
  is_initialized = true;

  if (has_configuration_capability) {
    werror("Client has config capabilities\n");
  }

  if (has_workspace_folder_capability) {
    werror("Clients has workspace folder capabilities\n");
  }
}

public void on_shutdown(JsonRpc.NotificationMessage message) {
  werror("Handle shutdown request\n");
}

public void on_exit(JsonRpc.NotificationMessage message) {
  werror("Handle exit request\n");
}

public void on_text_document_did_change(JsonRpc.NotificationMessage message) {
  object doc = text_documents[message->params->textDocument->uri];

  if (!doc) {
    werror("Document not found!!!\n");
  }

  doc->update(message->params);

  werror("Now document is: %s\n", doc->text);
}

public void on_text_document_did_open(JsonRpc.NotificationMessage message) {
  LSP.TextDocument doc = LSP.TextDocument(message->params->textDocument);
  text_documents[doc->uri] = doc;
}

public void on_text_document_did_close(JsonRpc.NotificationMessage message) {
  string uri = message->params->textDocument->uri;

  if (text_documents[uri]) {
    werror("Zonk document with URI: %O\n", uri);
    m_delete(text_documents, uri);
  } else {
    werror("Document %O no longer in cache\n", uri);
  }
}

public bool `has_configuration_capability() {
  return !!init_params?->capabilities->workspace?->configuration;
}

public bool `has_workspace_folder_capability() {
  return !!init_params?->capabilities->workspace?->workspaceFolders;
}

public bool `has_diagnostics_related_information_capability() {
  return !!init_params?->capabilities
    ->textDocument?->publishDiagnostics?->relatedInformation;
}
