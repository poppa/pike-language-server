import Pest;
import AST.Token;
import AST.Lexer;

int main() {

describe("Delimiters", lambda () {
  test("It should lex left paren", lambda() {
    Token tok = Lexer("(")->lex();
    expect(tok->type)->to_equal(PAREN_LEFT);
    expect(tok->value)->to_equal("(");
  });

  test("It should lex opening array", lambda () {
    Token tok = Lexer("({")->lex();
    expect(tok->type)->to_equal(ARRAY_START);
    expect(tok->value)->to_equal("({");
  });

  test("It should lex opening mapping", lambda () {
    Token tok = Lexer("([")->lex();
    expect(tok->type)->to_equal(MAPPING_START);
    expect(tok->value)->to_equal("([");
  });

  test("It should lex opening multiset", lambda () {
    Token tok = Lexer("(<")->lex();
    expect(tok->type)->to_equal(MULTISET_START);
    expect(tok->value)->to_equal("(<");
  });

  test("It should lex closing mapping", lambda () {
    Token tok = Lexer("])")->lex();
    expect(tok->type)->to_equal(MAPPING_END);
    expect(tok->value)->to_equal("])");
  });

  test("It should lex closing array", lambda () {
    Token tok = Lexer("})")->lex();
    expect(tok->type)->to_equal(ARRAY_END);
    expect(tok->value)->to_equal("})");
  });

  test("It should lex closing multiset", lambda () {
    Token tok = Lexer(">)")->lex();
    expect(tok->type)->to_equal(MULTISET_END);
    expect(tok->value)->to_equal(">)");
  });

  test("It should lex right paren", lambda () {
    Token tok = Lexer(")")->lex();
    expect(tok->type)->to_equal(PAREN_RIGHT);
    expect(tok->value)->to_equal(")");
  });

  test("It should lex left bracket", lambda () {
    Token tok = Lexer("[")->lex();
    expect(tok->type)->to_equal(BRACKET_LEFT);
    expect(tok->value)->to_equal("[");
  });

  test("It should lex right bracket", lambda () {
    Token tok = Lexer("]")->lex();
    expect(tok->type)->to_equal(BRACKET_RIGHT);
    expect(tok->value)->to_equal("]");
  });

  test("It should lex left curly brace", lambda () {
    Token tok = Lexer("{")->lex();
    expect(tok->type)->to_equal(CURLY_LEFT);
    expect(tok->value)->to_equal("{");
  });

  test("It should lex right curly brace", lambda () {
    Token tok = Lexer("}")->lex();
    expect(tok->type)->to_equal(CURLY_RIGHT);
    expect(tok->value)->to_equal("}");
  });

  test("It should lex left angle bracket (less than)", lambda () {
    Token tok = Lexer("<")->lex();
    expect(tok->type)->to_equal(LESS_THAN);
    expect(tok->value)->to_equal("<");
  });

  test("It should lex right angle bracket (greater than)", lambda () {
    Token tok = Lexer(">")->lex();
    expect(tok->type)->to_equal(GREATER_THAN);
    expect(tok->value)->to_equal(">");
  });

  test("It should lex shift right (double greater than)", lambda () {
    Token tok = Lexer(">>")->lex();
    expect(tok->type)->to_equal(RSH);
    expect(tok->value)->to_equal(">>");
  });

  test("It should lex shift right assignment", lambda () {
    Token tok = Lexer(">>=")->lex();
    expect(tok->type)->to_equal(RSH_EQ);
    expect(tok->value)->to_equal(">>=");
  });

  test("It should lex greater or equal than", lambda () {
    Token tok = Lexer(">=")->lex();
    expect(tok->type)->to_equal(GE);
    expect(tok->value)->to_equal(">=");
  });

  test("It should lex shift left (double less than)", lambda () {
    Token tok = Lexer("<<")->lex();
    expect(tok->type)->to_equal(LSH);
    expect(tok->value)->to_equal("<<");
  });

  test("It should lex shift left assignment", lambda () {
    Token tok = Lexer("<<=")->lex();
    expect(tok->type)->to_equal(LSH_EQ);
    expect(tok->value)->to_equal("<<=");
  });

  test("It should lex less or equal than", lambda () {
    Token tok = Lexer("<=")->lex();
    expect(tok->type)->to_equal(LE);
    expect(tok->value)->to_equal("<=");
  });

  test("It should lex exclamation (logical NOT)", lambda () {
    Token tok = Lexer("!")->lex();
    expect(tok->type)->to_equal(NOT);
    expect(tok->value)->to_equal("!");
  });

  test("It should lex not equal", lambda () {
    Token tok = Lexer("!=")->lex();
    expect(tok->type)->to_equal(NE);
    expect(tok->value)->to_equal("!=");
  });

  test("It should lex equal", lambda () {
    Token tok = Lexer("=")->lex();
    expect(tok->type)->to_equal(ASSIGN);
    expect(tok->value)->to_equal("=");
  });

  test("It should lex double equal", lambda () {
    Token tok = Lexer("==")->lex();
    expect(tok->type)->to_equal(EQ);
    expect(tok->value)->to_equal("==");
  });

  test("It should lex minus", lambda () {
    Token tok = Lexer("-")->lex();
    expect(tok->type)->to_equal(MINUS);
    expect(tok->value)->to_equal("-");
  });

  test("It should lex double minus", lambda () {
    Token tok = Lexer("--")->lex();
    expect(tok->type)->to_equal(DEC);
    expect(tok->value)->to_equal("--");
  });

  test("It should lex minus equal", lambda () {
    Token tok = Lexer("-=")->lex();
    expect(tok->type)->to_equal(SUB_EQ);
    expect(tok->value)->to_equal("-=");
  });

  test("It should lex arrow", lambda () {
    Token tok = Lexer("->")->lex();
    expect(tok->type)->to_equal(ARROW);
    expect(tok->value)->to_equal("->");
  });

  test("It should lex plus", lambda () {
    Token tok = Lexer("+")->lex();
    expect(tok->type)->to_equal(PLUS);
    expect(tok->value)->to_equal("+");
  });

  test("It should lex double plus", lambda () {
    Token tok = Lexer("++")->lex();
    expect(tok->type)->to_equal(INC);
    expect(tok->value)->to_equal("++");
  });

  test("It should lex plus equal", lambda () {
    Token tok = Lexer("+=")->lex();
    expect(tok->type)->to_equal(ADD_EQ);
    expect(tok->value)->to_equal("+=");
  });

  test("It should lex ampersand (binary AND)", lambda () {
    Token tok = Lexer("&")->lex();
    expect(tok->type)->to_equal(AMP);
    expect(tok->value)->to_equal("&");
  });

  test("It should lex double ampersand (logical AND)", lambda () {
    Token tok = Lexer("&&")->lex();
    expect(tok->type)->to_equal(LAND);
    expect(tok->value)->to_equal("&&");
  });

  test("It should lex ampersand equal (binary AND assignment)", lambda () {
    Token tok = Lexer("&=")->lex();
    expect(tok->type)->to_equal(AND_EQ);
    expect(tok->value)->to_equal("&=");
  });

  test("It should lex pipe (binary OR)", lambda () {
    Token tok = Lexer("|")->lex();
    expect(tok->type)->to_equal(PIPE);
    expect(tok->value)->to_equal("|");
  });

  test("It should lex pipe equal (binary OR assignment)", lambda () {
    Token tok = Lexer("|=")->lex();
    expect(tok->type)->to_equal(OR_EQ);
    expect(tok->value)->to_equal("|=");
  });

  test("It should lex double pipe (logical OR)", lambda () {
    Token tok = Lexer("||")->lex();
    expect(tok->type)->to_equal(LOR);
    expect(tok->value)->to_equal("||");
  });

  test("It should lex tripple dots", lambda () {
    Token tok = Lexer("...")->lex();
    expect(tok->type)->to_equal(DOT_DOT_DOT);
    expect(tok->value)->to_equal("...");
  });

  test("It should lex double dots", lambda () {
    Token tok = Lexer("..")->lex();
    expect(tok->type)->to_equal(DOT_DOT);
    expect(tok->value)->to_equal("..");
  });

  test("It should lex dot", lambda () {
    Token tok = Lexer(".")->lex();
    expect(tok->type)->to_equal(DOT);
    expect(tok->value)->to_equal(".");
  });

  test("It should lex double colon", lambda () {
    Token tok = Lexer("::")->lex();
    expect(tok->type)->to_equal(COLON_COLON);
    expect(tok->value)->to_equal("::");
  });

  test("It should lex colon", lambda () {
    Token tok = Lexer(":")->lex();
    expect(tok->type)->to_equal(COLON);
    expect(tok->value)->to_equal(":");
  });

  test("It should lex XOR", lambda () {
    Token tok = Lexer("^")->lex();
    expect(tok->type)->to_equal(XOR);
    expect(tok->value)->to_equal("^");
  });

  test("It should lex XOR equal", lambda () {
    Token tok = Lexer("^=")->lex();
    expect(tok->type)->to_equal(XOR_EQ);
    expect(tok->value)->to_equal("^=");
  });
});

// end main
}
