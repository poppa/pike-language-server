inherit LSP.EventEmitter;

//! @url{https://microsoft.github.io/language-server-protocol/specifications/\
//!lsp/3.17/specification/#initializeParams@}
protected mapping init_params;
protected bool is_initialized = false;

protected LSP.TextDocument.Manager text_documents =
  LSP.TextDocument.Manager(this);

protected void create() {
  this
    .once("initialize", on_initialize)
    .once("initialized", on_initialized);
}

public void start();
public void stop();
protected void send_response(mapping message, void|JsonRpc.Id id);

public LSP.TextDocument.Manager `manager() {
  return text_documents;
}

protected string encode_response_message_with_header(
  mapping message,
  void|JsonRpc.Id id
) {
  string data = Standards.JSON.encode(
    (mapping)JsonRpc.make_response_message(message, id)
  );
  return sprintf("Content-Length: %d\r\n\r\n%s", sizeof(data), data);
}

protected string encode_response_error_with_header(JsonRpc.Error err) {
  string data = Standards.JSON.encode(
    (mapping)JsonRpc.make_response_error(err)
  );
  return sprintf("Content-Length: %d\r\n\r\n%s", sizeof(data), data);
}

public void handle_request(.Request request) {
  JsonRpc.Message rpc = request->rpc_message;

  werror("handle_request(%O:%O) -> %O\n", rpc->id, rpc->method, rpc->params);

  if (!is_initialized && !(< "initialize", "initialized" >)[rpc->method]) {
    throw(JsonRpc.Error(
      .ERR_SERVER_NOT_INITIALIZED,
      "Received request before initialize",
      rpc,
    ));
  }


  emit(rpc->method, rpc->params, rpc->id);
}

public void on_initialize(mapping params, void|JsonRpc.Id id) {
  init_params = params;
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
    id
  );
}

public void on_initialized(mapping params) {
  is_initialized = true;

  if (has_configuration_capability) {
    werror("Client has config capabilities\n");
  }

  if (has_workspace_folder_capability) {
    werror("Clients has workspace folder capabilities\n");
  }
}

public void on_shutdown(mapping message) {
  werror("Handle shutdown request\n");
}

public void on_exit(mapping message) {
  werror("Handle exit request\n");
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
