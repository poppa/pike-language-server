import Pest;
import AST.Token;
import AST.Parser;
import AST.Lexer;

int main() {
describe("Just starting dude", lambda () {
  test("It should do shit", lambda () {
    Lexer l = Lexer(#"import Pest.Test");
    PikeParser p = PikeParser(l);
    p->parse();
  });

  test("It should be peekable", lambda () {
    Lexer l = Lexer("import AST.Token;");
    PikeParser p = PikeParser(l);

    Token t = p->next_token();
    expect(t->type)->to_equal(IMPORT);

    Token peeked = p->peek_token();
    expect(peeked->type)->to_equal(IDENTIFIER);
    expect(peeked->value)->to_equal("AST");

    Token peeked2 = p->peek_token();
    expect(peeked2)->to_be(peeked);

    t = p->next_token();
    expect(peeked)->to_be(t);

    t = p->next_token();
    expect(t->type)->to_equal(DOT);
  });
});
}
