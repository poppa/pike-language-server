import Pest;
import AST.Lexer;
import AST.Token;

int main() {
test("is_modifier() should do its thing", lambda () {
  Lexer l = Lexer(
    "__unused__ "
    "__weak__ "
    "continue "
    "extern "
    "final "
    "inline "
    "local "
    "optional "
    "private "
    "protected "
    "public "
    "static "
    "variant"
  );

  while (Token t = l->lex()) {
    expect(is_modifier(t))->to_equal(true);
  }
});

test("EofToken should be falsy", lambda () {
  Token t = EofToken(
    Location("stdin", Position(0, 0, 0), Position(0, 0, 0)),
    0,
  );

  expect(t == false)->to_equal(true);
  expect(t)->to_be_falsy();
  expect(!t)->to_equal(true);
  expect(!!t)->to_equal(false);
  expect(t->type)->to_equal(EOF);
});
}
