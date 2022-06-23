import Pest;
import AST.Token;
import AST.Parser;
import AST.Lexer;
import AST.Node;

int main() {
describe("Just starting dude", lambda () {
  test("It should parse import of identifers", lambda () {
    Lexer l = Lexer(#"import Pest.Test;");
    PikeParser p = PikeParser(l);
    Program ast_p = p->parse();
    expect(object_program(ast_p))->to_be(Program);
    expect(sizeof(ast_p->body))->to_equal(1);

    ImportStatement imp = ast_p->body[0];
    expect(sizeof(imp->identifiers))->to_equal(2);
    expect(undefinedp(imp->path))->to_equal(true);
    expect(object_program(imp->identifiers[0]))->to_be(Identifier);
    expect(object_program(imp->identifiers[1]))->to_be(Identifier);
    expect(imp->identifiers[0]->name)->to_equal("Pest");
    expect(imp->identifiers[1]->name)->to_equal("Test");
  });

  test("Import statement with no ending semicolon should throw", lambda () {
    Lexer l = Lexer(#"import Pest.Test");
    expect(lambda () { PikeParser(l)->parse(); })->to_throw();
  });

  test("It should parse import of relative string import", lambda () {
    Lexer l = Lexer(#"import \".\";");
    PikeParser p = PikeParser(l);
    Program ast_p = p->parse();
    ImportStatement imp = ast_p->body[0];
    expect(imp->identifiers)->to_equal(({}));
    expect(imp->path)->to_equal(".");
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
