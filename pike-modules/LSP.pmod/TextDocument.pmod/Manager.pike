#include "textdocument.h"

inherit LSP.EventEmitter;

private LSP.Server.Base connection;
private mapping(.DocumentUri:object(.Document)) _documents = ([]);
private Pucko.Compiler pucko_compiler;

protected void create(LSP.Server.Base connection) {
  this::connection = connection;
  this::connection
    .on("textDocument/didOpen", on_did_open)
    .on("textDocument/didChange", on_did_change)
    .on("textDocument/didClose", on_did_close)
    .on("textDocument/didSave", on_did_save);

  this::pucko_compiler = Pucko.Compiler();
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

protected void on_did_change(mapping message) {
  DEBUG("Got on change event: %O\n", message);

  mapping td = message->textDocument;
  object doc = get_document(td->uri);

  if (!doc) {
    DEBUG("Document not found!!!\n");
    return;
  }

  doc->update(td->contentChanges, td->version);
}

protected void on_did_save(mapping message, JsonRpc.Id id) {
  .Document doc = get_document(message);
  DEBUG("Got on_did_save event for doc: %O <> %O : %O\n", doc, message, id);

  array(LSP.Diagnostic.Diagnostic) diagnostics;

  float time = gauge {
    diagnostics = pucko_compile(doc);
  };

  if (diagnostics) {
    DEBUG("Do send Diagnostics\n");
    send_diagnostics(
      doc,
      diagnostics,
      id
        // || Standards.UUID.make_version4()->str()
    );
  }

  DEBUG("Fugly compilation took: %O\n", time);
}

protected void send_diagnostics(
  .Document doc,
  array(LSP.Diagnostic.Diagnostic) diagnostics,
  void|JsonRpc.Id message_id)
{
  mapping m = LSP.Diagnostic.make_message(doc->uri, diagnostics);
  DEBUG("Send Diagnostics message: %O\n", m);
  connection->send_response(m, message_id);
}

protected array(LSP.Diagnostic.Diagnostic)|void pucko_compile(.Document d) {
  string uri = d->uri;

  if (has_prefix(uri, "file://")) {
    uri -= "file://";
  }

  if (!Stdio.exist(uri)) {
    return;
  }

  array|void res = pucko_compiler->compile(uri);

  if (!res) {
    return;
  }

  array(LSP.Diagnostic.Diagnostic) diagnostics = ({});

  if (sizeof(res[0]->errors)) {
    diagnostics += map(
      res[0]->errors,
      lambda (mapping m) {
        return pucko_message_to_diagnostic(
          d->uri,
          m,
          LSP.Diagnostic.SeverityError
        );
      }
    );
  }

  if (sizeof(res[0]->warnings)) {
    diagnostics += map(
      res[0]->warnings,
      lambda (mapping m) {
        return pucko_message_to_diagnostic(
          d->uri,
          m,
          LSP.Diagnostic.SeverityWarning
        );
      }
    );
  }

  if (sizeof(diagnostics)) {
    return diagnostics - ({ 0 });
  }
}

protected LSP.Diagnostic.Diagnostic pucko_message_to_diagnostic(
  .DocumentUri uri,
  mapping m,
  LSP.Diagnostic.Severity severity
) {
  DEBUG("Make Diagnostic: %O\n", m);
  .Position start = .Position(m->line, 1);
  .Position end = .Position(m->line, 20);
  .Range range = .Range(start, end);

  LSP.Diagnostic.Diagnostic d = LSP.Diagnostic.Diagnostic(range, m->msg);
  d->severity = severity;

  return d;
}
