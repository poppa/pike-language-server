import Pest;

class MyLsp {
  inherit Server.Lsp;
  protected mapping init_params = ([]);
  public function on_test;
}

int main() {
  test("parse_raw_request should return a Request object", lambda () {
    string body = "{\"key\": 12}\r\n";
    string raw = sprintf("Content-Length: %d\r\n"
      "User-Agent: Stdio\r\n\r\n", sizeof(body)) + body;

    Stdio.File f = Stdio.FakeFile(raw);
    mixed res = Server.parse_raw_request(f);

    expect(object_program(res))->to_equal(Server.Request);
  });

  test("Lsp server should do stuff", lambda () {
    string body = Standards.JSON.encode(([
      "jsonrpc": "2.0",
      "method": "test",
      "params": ([ "capabilities": ({ "one", "two" }) ])
    ]));

    string raw = sprintf(
      "Content-Length: %d\r\n"
      "User-Agent: Stdio\r\n\r\n",
      sizeof(body)
    ) + body;

    Stdio.File f = Stdio.FakeFile(raw);
    mixed res = Server.parse_raw_request(f);

    MyLsp lsp = MyLsp();
    lsp->on_test = fn(lambda (JsonRpc.NotificationMessage mess) {});
    lsp->handle_request(res);

    expect(lsp->on_test)->to_have_been_called_n_times(1);
  });
}
