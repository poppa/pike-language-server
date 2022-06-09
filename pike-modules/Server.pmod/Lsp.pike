
//! @url{https://microsoft.github.io/language-server-protocol/specifications/\
//!lsp/3.17/specification/#initializeParams@}
protected mapping init_params;
protected bool is_initialized = false;

protected void create() {}

protected void send_response(mapping message, void|JsonRpc.Id id);

public JsonRpc.Message|void handle_request(.Request request) {
  JsonRpc.Message rpc = request->rpc_message;

  werror("handle_request(%O:%O) <- %O\n", rpc->id, rpc->method, rpc->params);

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
    return cb(rpc);
  }

  throw(JsonRpc.JsonRpcError(
    JsonRpc.METHOD_NOT_FOUND,
    sprintf("The method %O does not exist or is not available.", rpc->method),
    rpc,
  ));
}

public void on_initialize(JsonRpc.NotificationMessage message) {
  init_params = message->params;

  mapping fileops = ([
    "filters": ({
      ([ "pattern": ([ "glob": "*.pike" ]) ]),
      ([ "pattern": ([ "glob": "*.pmod" ]) ]),
    })
  ]);

  send_response(
    ([
      "capabilities": ([
        "selectionRangeProvider": true,
        "workspace": ([
          "fileOperations": ([
            "didCreate": fileops,
            "willCreate": fileops,
            "didRename": fileops,
            "willRename": fileops,
            "didDelete": fileops,
            "willDelete": fileops,
          ])
        ]),
      ]),
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
}

public void on_shutdown(JsonRpc.NotificationMessage message) {
  werror("Handle shutdown request\n");
}

public void on_exit(JsonRpc.NotificationMessage message) {
  werror("Handle exit request\n");
}
