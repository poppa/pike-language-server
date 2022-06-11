import Pest;

class MyLsp {
  inherit LSP.Server.Base;
  protected mapping init_params = ([]);
  bool is_initialized = true;
}

int main() {
  test("parse_raw_request should return a Request object", lambda () {
    string body = "{\"key\": 12}\r\n";
    string raw = sprintf("Content-Length: %d\r\n"
      "User-Agent: Stdio\r\n\r\n", sizeof(body)) + body;

    Stdio.File f = Stdio.FakeFile(raw);
    mixed res = LSP.Server.parse_raw_request(f);

    expect(object_program(res))->to_equal(LSP.Server.Request);
  });

  test("Lsp server should do stuff", lambda () {
    string body = Standards.JSON.encode(([
      "jsonrpc": "2.0",
      "method": "test",
      "params": ([ "capabilities": ([]) ])
    ]));

    string raw = sprintf(
      "Content-Length: %d\r\n"
      "User-Agent: Stdio\r\n\r\n",
      sizeof(body)
    ) + body;

    Stdio.File f = Stdio.FakeFile(raw);
    mixed res = LSP.Server.parse_raw_request(f);

    MyLsp lsp = MyLsp();
    function on_test = fn(lambda (mapping params) {
      expect(params->capabilities)->to_equal(([]));
    });
    lsp->on("test", on_test);
    lsp->handle_request(res);

    expect(on_test)->to_have_been_called_n_times(1);
  });
}
