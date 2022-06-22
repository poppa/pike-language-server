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
}
