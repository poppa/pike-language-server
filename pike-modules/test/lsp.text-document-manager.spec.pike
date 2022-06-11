import Pest;
import LSP.TextDocument;

int main() {

test(
  "It should create and add a document when receiving a didOpen event",
  lambda () {
    #include "test-sources/lsp/textDocument.pike"

    .Helpers.FakeBaseConnection connection = .Helpers.FakeBaseConnection();
    connection->emit("textDocument/didOpen", init, 0);

    expect(sizeof(connection->manager->documents))->to_equal(1);
  }
);

test("It should throw when trying to add the same document twice", lambda () {
  #include "test-sources/lsp/textDocument.pike"

  .Helpers.FakeBaseConnection connection = .Helpers.FakeBaseConnection();
  connection->emit("textDocument/didOpen", init, 0);

  expect(lambda() { connection->emit("textDocument/didOpen", init, 0); })
    ->to_throw();
});

test("It should remove the document when receiving a close event", lambda () {
  #include "test-sources/lsp/textDocument.pike"

  .Helpers.FakeBaseConnection connection = .Helpers.FakeBaseConnection();

  connection->emit("textDocument/didOpen", init, 0);
  expect(sizeof(connection->manager->documents))->to_equal(1);

  connection->emit("textDocument/didClose", close_params, 0);
  expect(sizeof(connection->manager->documents))->to_equal(0);
});
}
