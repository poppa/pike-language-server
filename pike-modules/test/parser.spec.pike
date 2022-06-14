import Pest;
import AST.Parser;
import AST.Lexer;

int main() {
describe("Just starting dude", lambda () {
  test("It should do shit", lambda () {
    Lexer l = Lexer(#"public int main() { return 0; }");
    PikeParser p = PikeParser(l);
    p->parse();
  });
});
}
