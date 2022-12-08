#pragma strict_types

constant VERSION = "2.0";

typedef string|int|Val.Null Id;
typedef array|mapping Params;
typedef string|float|int|bool|mapping|Val.Null ResultType;
typedef string|float|int|bool|array|mapping|Val.Null ErrorData;

public enum error_code {
  //! Invalid JSON was received by the server.
  //! An error occurred on the server while parsing the JSON text.
  PARSE_ERROR = -32700,

  //! The JSON sent is not a valid Request object.
  INVALID_REQUEST = -32600,

  //! The method does not exist / is not available.
  METHOD_NOT_FOUND = -32601,

  //! Invalid method parameter(s).
  INVALID_PARAMS = -32602,

  //! Internal JSON-RPC error.
  INTERNAL_ERROR = -32603,
}

public constant IMPL_MAX_ERROR_CODE = -32000;
public constant IMPL_MIN_ERROR_CODE = -32009;

protected mapping(error_code:string) error_messages = ([
  PARSE_ERROR: "Invalid JSON was received by the server.",
  INVALID_REQUEST: "The JSON sent is not a valid Request object.",
  METHOD_NOT_FOUND: "The method does not exist or is not available.",
  INVALID_PARAMS: "Invalid method parameter(s).",
  INTERNAL_ERROR: "Internal JSON-RPC error.",
]);

public class Error {
  inherit __builtin.GenericError;

  protected int _code;
  protected Message _request_message;

  protected void create(error_code code) {
    this::_code = code;
    string message = error_messages[code];

    if (!message) {
      error("Unknown error code \"%O\"\n", code);
    }

    this::create(code, message);
  }

  protected variant void create(int code, string message) {
    if (!is_valid_error_code(code)) {
      error("Invalid error code\n");
    }

    if (!has_suffix(message, "\n")) {
      message += "\n";
    }

    this::_code = code;
    ::create(message, backtrace());
  }

  protected variant void create(int code, Message request_message) {
    this::_request_message = request_message;
    create(code);
  }

  protected variant void create(
    int code,
    string message,
    Message request_message
  ) {
    this::_request_message = request_message;
    create(code, message);
  }

  public int `code() {
    return _code;
  }

  public Message `request_message() {
    return _request_message;
  }
}

protected class BaseMessage {
  public string jsonrpc;

  protected mixed cast(string how) {
    mapping out = ([]);

    foreach (indices(this), string|mixed key) {
      mixed thing = this[key];

      // Don't ask me why, but arrays are treated as "callable"?
      if (stringp(key) && (!callablep(thing) || arrayp(thing))) {
        out[key] = thing;
      }
    }

    return out;
  }
}

protected class BaseIdMessage {
  inherit BaseMessage;
  public Id id;
}

public class NotificationMessage {
  inherit BaseMessage;
  public string method;
  public mapping|array params;
}

public class RequestMessage {
  inherit BaseIdMessage;
  inherit NotificationMessage;
}

public class ResponseMessage {
  inherit BaseIdMessage;
  public ResultType result;
}

public class ResponseErrorMessage {
  inherit BaseIdMessage;
  public ResponseError error;
}

public class ResponseError {
  public int code;
  public string message;
  public mixed|void data;
}

public typedef RequestMessage|NotificationMessage Message;
public typedef ResponseErrorMessage|ResponseMessage Response;

protected BaseMessage construct_instance(
  program(BaseMessage) prog,
  mapping args
) {
  BaseMessage instance = prog();

  foreach (indices(instance), string|mixed key) {
    if (stringp(key) && !undefinedp(args[key])) {
      instance[key] = args[key];
    }
  }

  return instance;
}

public ResponseMessage make_response_message(ResultType res, void|Id id) {
  return [object(ResponseMessage)] construct_instance(ResponseMessage, ([
    "jsonrpc": VERSION,
    "id": undefinedp(id) ? Val.null : id,
    "result": res,
  ]));
}

public ResponseErrorMessage make_response_error(
  Error err,
  void|ErrorData data,
  void|Id id
) {
  Id _id = Val.null;

  if (id) {
    _id = id;
  } else if (err->request_message && has_index(err->request_message, "id")) {
    _id = ([object(RequestMessage)]err->request_message)->id || Val.null;
  }

  return [object(ResponseErrorMessage)] construct_instance(
    ResponseErrorMessage,
    ([
      "jsonrpc": VERSION,
      "id": _id,
      "error": ([
        "code": err->code,
        "message": err->message(),
        "data": data,
      ])
    ])
  );
}

public RequestMessage make_request_message(
  string method,
  void|Params params,
  void|Id id
) {
  if (undefinedp(id)) {
    id = Standards.UUID.make_version4()->str();
  }

  return [object(RequestMessage)] construct_instance(RequestMessage, ([
    "jsonrpc": VERSION,
    "id": id,
    "method": method,
    "params": params,
  ]));
}

public NotificationMessage make_notification_message(
  string method,
  void|Params params
) {
  return [object(NotificationMessage)] construct_instance(
    NotificationMessage,
    ([
      "jsonrpc": VERSION,
      "method": method,
      "params": params,
    ])
  );
}

public bool is_valid_error_code(int code) {
  switch (code) {
    case PARSE_ERROR:
    case INTERNAL_ERROR..INVALID_REQUEST:
    case IMPL_MIN_ERROR_CODE..IMPL_MAX_ERROR_CODE:
      return true;
  }

  return false;
}

public bool is_valid_request(RequestMessage|mapping message) {
  return !!(message->jsonrpc && message->jsonrpc == VERSION && message->method);
}

public RequestMessage|NotificationMessage request_to_instance(mapping message) {
  if (!is_valid_request(message)) {
    throw(Error(INVALID_REQUEST));
  }

  if (undefinedp(message->id)) {
    return [object(NotificationMessage)] construct_instance(
      NotificationMessage, message
    );
  }

  return [object(RequestMessage)] construct_instance(RequestMessage, message);
}
