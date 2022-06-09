import Pest;
import JsonRpc;

int main() {
  test("Version should be 2.0", lambda () {
    expect(VERSION)->to_equal("2.0");
  });

  test("is_valid_error_code() should to its thing", lambda () {
    expect(is_valid_error_code(PARSE_ERROR))->to_equal(true);
    expect(is_valid_error_code(INVALID_REQUEST))->to_equal(true);
    expect(is_valid_error_code(METHOD_NOT_FOUND))->to_equal(true);
    expect(is_valid_error_code(INVALID_PARAMS))->to_equal(true);
    expect(is_valid_error_code(INTERNAL_ERROR))->to_equal(true);
    // Implementation specific MAX error code
    expect(is_valid_error_code(-32000))->to_equal(true);
    expect(is_valid_error_code(-32001))->to_equal(true);
    expect(is_valid_error_code(-32002))->to_equal(true);
    expect(is_valid_error_code(-32003))->to_equal(true);
    expect(is_valid_error_code(-32004))->to_equal(true);
    expect(is_valid_error_code(-32005))->to_equal(true);
    expect(is_valid_error_code(-32006))->to_equal(true);
    expect(is_valid_error_code(-32007))->to_equal(true);
    expect(is_valid_error_code(-32008))->to_equal(true);
    // Implementation specific MIN error code
    expect(is_valid_error_code(-32009))->to_equal(true);

    expect(is_valid_error_code(-32010))->to_equal(false);
    expect(is_valid_error_code(1))->to_equal(false);
    expect(is_valid_error_code(0))->to_equal(false);
    expect(is_valid_error_code(10))->to_equal(false);
  });

  test(
    "make_request_message() with only method argument should return a " +
    "valid message",
    lambda () {
      RequestMessage m = make_request_message("query");
      expect(sprintf("%O", object_program(m)))
        ->to_equal("JsonRpc.RequestMessage");
      expect(m->jsonrpc)->to_equal("2.0");
      expect(m->method)->to_equal("query");
      expect(m->params)->to_equal(UNDEFINED);
      expect(m->id)->to_be_truthy();
      expect(is_valid_request(m))->to_equal(true);
    }
  );

  test(
    "make_request_message() with method and params arguments should return a " +
    "valid message",
    lambda () {
      RequestMessage m = make_request_message(
        "query",
        ([ "name": "json", "version": 2.0])
      );
      expect(sprintf("%O", object_program(m)))
        ->to_equal("JsonRpc.RequestMessage");
      expect(m->jsonrpc)->to_equal("2.0");
      expect(m->method)->to_equal("query");
      expect(m->params)->to_equal(([ "name": "json", "version": 2.0 ]));
      expect(m->id)->to_be_truthy();
      expect(is_valid_request(m))->to_equal(true);
    }
  );

  test(
    "make_request_message() with method, params and id arguments should " +
    "return a valid message with custom ID",
    lambda () {
      RequestMessage m = make_request_message(
        "query",
        ([ "name": "json", "version": 2.0 ]),
        "my_id_1"
      );
      expect(sprintf("%O", object_program(m)))
        ->to_equal("JsonRpc.RequestMessage");
      expect(m->jsonrpc)->to_equal("2.0");
      expect(m->method)->to_equal("query");
      expect(m->params)->to_equal(([ "name": "json", "version": 2.0 ]));
      expect(m->id)->to_equal("my_id_1");
      expect(is_valid_request(m))->to_equal(true);
    }
  );

  test("Casting a request message to a mapping should work", lambda () {
    RequestMessage m = make_request_message(
      "query",
      ([ "name": "json", "version": 2.0 ]),
      "my_id_1"
    );

    expect((mapping)m)->to_equal(([
      "jsonrpc": "2.0",
      "method": "query",
      "params": ([ "name": "json", "version": 2.0 ]),
      "id": "my_id_1",
    ]));
  });

  test(
    "make_notification_message() should return a valid notification message",
    lambda () {
      NotificationMessage n = make_notification_message("query",  ([ "a": 1 ]));
      expect(sprintf("%O", object_program(n)))
        ->to_equal("JsonRpc.NotificationMessage");
      expect(n->id)->to_equal(UNDEFINED);
      expect(n->method)->to_equal("query");
      expect(n->params)->to_equal(([ "a": 1 ]));
      expect(is_valid_request(n))->to_equal(true);
    }
  );

  test(
    "request_to_instance should return a RequestMessage instance" +
    "when given a valid request mapping",
    lambda () {
      mapping req = ([
        "method": "query",
        "id": "1",
        "params": ([ "a": 1 ]),
        "jsonrpc": "2.0"
      ]);

      RequestMessage m = request_to_instance(req);
      expect(sprintf("%O", object_program(m)))
        ->to_equal("JsonRpc.RequestMessage");
      expect(m->id)->to_equal("1");
      expect(m->jsonrpc)->to_equal("2.0");
      expect(m->params)->to_equal(([ "a": 1 ]));
      expect(m->method)->to_equal("query");
    }
  );

  test(
    "request_to_instance should return a NotificationMessage instance" +
    "when given a valid notification mapping",
    lambda () {
      mapping req = ([
        "method": "query",
        "params": ([ "a": 1 ]),
        "jsonrpc": "2.0"
      ]);

      RequestMessage m = request_to_instance(req);
      expect(sprintf("%O", object_program(m)))
        ->to_equal("JsonRpc.NotificationMessage");
      expect(m->jsonrpc)->to_equal("2.0");
      expect(m->params)->to_equal(([ "a": 1 ]));
      expect(m->method)->to_equal("query");
    }
  );

  test("request_to_instance should throw given an invalid message", lambda () {
    mapping m = ([ "method": "query" ]);
    expect(lambda() { request_to_instance(m); })->to_throw();
  });

  test("make_response_message should do its thing", lambda () {
    object x = make_response_message("yes");
    expect(object_program(x))->to_be(JsonRpc.ResponseMessage);
    expect(x->result)->to_equal("yes");
  });

  test("make_response_message with id param should do its thing", lambda () {
    object x = make_response_message("yes", 12);
    expect(object_program(x))->to_be(JsonRpc.ResponseMessage);
    expect(x->result)->to_equal("yes");
    expect(x->id)->to_equal(12);
  });

  test("make_response_error should do its thing", lambda () {
    object err = JsonRpc.JsonRpcError(JsonRpc.PARSE_ERROR);
    object x = make_response_error(err);
    expect(object_program(x))->to_be(JsonRpc.ResponseErrorMessage);
    expect(mappingp(x->error))->to_equal(true);
    expect(x->jsonrpc)->to_equal("2.0");
    expect(x->error->code)->to_equal(JsonRpc.PARSE_ERROR);
  });

  test(
    "make_response_error with id and data params should do its thing",
    lambda () {
      object err = JsonRpc.JsonRpcError(JsonRpc.PARSE_ERROR);
      object x = make_response_error(err, ({ "one", 1 }), 12);
      expect(object_program(x))->to_be(JsonRpc.ResponseErrorMessage);
      expect(mappingp(x->error))->to_equal(true);
      expect(x->jsonrpc)->to_equal("2.0");
      expect(x->error->code)->to_equal(JsonRpc.PARSE_ERROR);
      expect(x->id)->to_equal(12);
      expect(x->error->data)->to_equal(({ "one", 1 }));
    }
  );
}
