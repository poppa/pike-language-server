import Pest;
import LSP;

int main() {
describe("TextDocument module", lambda () {
  test("It should create an instance from an RPC message", lambda () {
    #include "test-sources/lsp/textDocument.pike"
    object doc = TextDocument.create_document(init->textDocument);
    expect(object_program(doc))->to_be(TextDocument.Document);
    expect(doc->version)->to_equal(1);
    expect(doc->language_id)->to_equal("pike");
    expect(doc->uri)
      ->to_equal("file:///Users/pontus/Desktop/Pike-WS/dump2.pike");
    expect(doc->line_count)->to_equal(5);

    werror("Doc: %O\n", doc);
  });
});
}
