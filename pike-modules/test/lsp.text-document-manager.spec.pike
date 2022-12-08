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

test("Dummy compiler...WIP", lambda () {
  .Helpers.FakeBaseConnection connection = .Helpers.FakeBaseConnection();

  string uri = "file://" +
    combine_path(__DIR__, "test-sources", "missing-semi.pike");

  connection->emit(
    "textDocument/didOpen",
    ([ /* 1 element */
      "textDocument": ([ /* 4 elements */
        "languageId": "pike",
        "text": "\n"
          "int age = 12;\n"
          "string name = \"Pontus\"\n"
          "array(string) interests = ({});\n",
        "uri": uri,
        "version": 1
      ])
    ]),
    0
  );

  connection->emit(
    "textDocument/didChange",
    ([ /* 2 elements */
      "contentChanges": ({ /* 1 element */
        ([ /* 3 elements */
          "range": ([ /* 2 elements */
              "end": ([ /* 2 elements */
                  "character": 13,
                  "line": 1
                ]),
              "start": ([ /* 2 elements */
                  "character": 10,
                  "line": 1
                ])
            ]),
          "rangeLength": 3,
          "text": "i"
        ])
      }),
      "textDocument": ([ /* 2 elements */
          "uri": uri,
          "version": 2
        ])
    ])
  );
});

}
