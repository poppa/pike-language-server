import Pest;
import AST.Token;
import AST.Lexer;

int main() {
/*******************************************************************************

  Lex delimiters and simple

*******************************************************************************/
describe("Delimiters and simple tokens", lambda () {
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

  test("It should lex pow", lambda () {
    Token tok = Lexer("**")->lex();
    expect(tok->type)->to_equal(POW);
    expect(tok->value)->to_equal("**");
  });

  test("It should lex pow equal", lambda () {
    Token tok = Lexer("**=")->lex();
    expect(tok->type)->to_equal(POW_EQ);
    expect(tok->value)->to_equal("**=");
  });

  test("It should lex multiply", lambda () {
    Token tok = Lexer("*")->lex();
    expect(tok->type)->to_equal(MULT);
    expect(tok->value)->to_equal("*");
  });

  test("It should lex multiply equal", lambda () {
    Token tok = Lexer("*=")->lex();
    expect(tok->type)->to_equal(MULT_EQ);
    expect(tok->value)->to_equal("*=");
  });

  test("It should lex safe apply", lambda () {
    Token tok = Lexer("(?")->lex();
    expect(tok->type)->to_equal(SAFE_APPLY);
    expect(tok->value)->to_equal("(?");
  });

  test("It should lex safe start index", lambda () {
    Token tok = Lexer("[?")->lex();
    expect(tok->type)->to_equal(SAFE_START_INDEX);
    expect(tok->value)->to_equal("[?");
  });

  test("It should lex safe index", lambda () {
    Token tok = Lexer("->?")->lex();
    expect(tok->type)->to_equal(SAFE_INDEX);
    expect(tok->value)->to_equal("->?");
  });

  test("It should lex divide equal", lambda () {
    Token tok = Lexer("/=")->lex();
    expect(tok->type)->to_equal(DIV_EQ);
    expect(tok->value)->to_equal("/=");
  });

  test("It should lex divide", lambda () {
    Token tok = Lexer("/")->lex();
    expect(tok->type)->to_equal(DIV);
    expect(tok->value)->to_equal("/");
  });

  test("It should lex question mark", lambda () {
    Token tok = Lexer("?")->lex();
    expect(tok->type)->to_equal(QUESTION);
    expect(tok->value)->to_equal("?");
  });

  test("It should lex atomic get/set", lambda () {
    Token tok = Lexer("?=")->lex();
    expect(tok->type)->to_equal(ATOMIC_GET_SET);
    expect(tok->value)->to_equal("?=");
  });

  test("It should lex comma", lambda () {
    Token tok = Lexer(",")->lex();
    expect(tok->type)->to_equal(COMMA);
    expect(tok->value)->to_equal(",");
  });

  test("It should lex semicolon", lambda () {
    Token tok = Lexer(";")->lex();
    expect(tok->type)->to_equal(SEMICOLON);
    expect(tok->value)->to_equal(";");
  });

  test("It should lex tilde", lambda () {
    Token tok = Lexer("~")->lex();
    expect(tok->type)->to_equal(TILDE);
    expect(tok->value)->to_equal("~");
  });

  test("It should lex at", lambda () {
    Token tok = Lexer("@")->lex();
    expect(tok->type)->to_equal(AT);
    expect(tok->value)->to_equal("@");
  });

  test("It should lex percent (modulus)", lambda () {
    Token tok = Lexer("%")->lex();
    expect(tok->type)->to_equal(MOD);
    expect(tok->value)->to_equal("%");
  });

  test("It should lex MOD_EQ", lambda () {
    Token tok = Lexer("%=")->lex();
    expect(tok->type)->to_equal(MOD_EQ);
    expect(tok->value)->to_equal("%=");
  });
});

/*******************************************************************************

  Lex magic underscore identifiers

*******************************************************************************/
describe("Magic underscore identifiers", lambda () {
  test("It should lex __attribute__", lambda () {
    Token tok = Lexer("__attribute__")->lex();
    expect(tok->type)->to_equal(ATTRIBUTE_ID);
    expect(tok->value)->to_equal("__attribute__");
  });

  test("It should lex __deprecated__", lambda () {
    Token tok = Lexer("__deprecated__")->lex();
    expect(tok->type)->to_equal(DEPRECATED_ID);
    expect(tok->value)->to_equal("__deprecated__");
  });

  test("It should lex __func__", lambda () {
    Token tok = Lexer("__func__")->lex();
    expect(tok->type)->to_equal(FUNCTION_NAME);
    expect(tok->value)->to_equal("__func__");
  });

  test("It should lex __unknown__", lambda () {
    Token tok = Lexer("__unknown__")->lex();
    expect(tok->type)->to_equal(UNKNOWN);
    expect(tok->value)->to_equal("__unknown__");
  });

  test("It should lex __unused__", lambda () {
    Token tok = Lexer("__unused__")->lex();
    expect(tok->type)->to_equal(UNUSED);
    expect(tok->value)->to_equal("__unused__");
  });

  test("It should lex __weak__", lambda () {
    Token tok = Lexer("__weak__")->lex();
    expect(tok->type)->to_equal(WEAK);
    expect(tok->value)->to_equal("__weak__");
  });

  test("It should lex _Static_assert", lambda () {
    Token tok = Lexer("__weak__")->lex();
    expect(tok->type)->to_equal(WEAK);
    expect(tok->value)->to_equal("__weak__");
  });

  test(
    "It should allow user defined dunderscore symbols with non-us-ascii chars",
    lambda () {
      Token t = Lexer("__OK__")->lex();
      expect(t->type)->to_equal(DUNDERSCORE);
      expect(t->value)->to_equal("__OK__");

      t = Lexer("__OK_OK2__")->lex();
      expect(t->type)->to_equal(DUNDERSCORE);
      expect(t->value)->to_equal("__OK_OK2__");
    }
  );

  test(
    "It should throw on user defined dunderscore symbol with only "
    "us-ascii chars",
    lambda () {
      Token t = Lexer("__nogood__");
      expect(lambda() { t->lex(); })->to_throw();
    }
  );
});

/*******************************************************************************

  Lex identifiers/keywords

*******************************************************************************/
describe("Identifiers and keywords", lambda () {
  test("It should lex \"array\"", lambda () {
    Token tok = Lexer("array")->lex();
    expect(tok->type)->to_equal(ARRAY_ID);
    expect(tok->value)->to_equal("array");
  });

  test("It should lex \"auto\"", lambda () {
    Token tok = Lexer("auto")->lex();
    expect(tok->type)->to_equal(AUTO_ID);
    expect(tok->value)->to_equal("auto");
  });

  test("It should lex \"bits\"", lambda () {
    Token tok = Lexer("bits")->lex();
    expect(tok->type)->to_equal(BITS);
    expect(tok->value)->to_equal("bits");
  });

  test("It should lex \"break\"", lambda () {
    Token tok = Lexer("break")->lex();
    expect(tok->type)->to_equal(BREAK);
    expect(tok->value)->to_equal("break");
  });

  test("It should lex \"case\"", lambda () {
    Token tok = Lexer("case")->lex();
    expect(tok->type)->to_equal(CASE);
    expect(tok->value)->to_equal("case");
  });

  test("It should lex \"catch\"", lambda () {
    Token tok = Lexer("catch")->lex();
    expect(tok->type)->to_equal(CATCH);
    expect(tok->value)->to_equal("catch");
  });

  test("It should lex \"class\"", lambda () {
    Token tok = Lexer("class")->lex();
    expect(tok->type)->to_equal(CLASS);
    expect(tok->value)->to_equal("class");
  });

  test("It should lex \"constant\"", lambda () {
    Token tok = Lexer("constant")->lex();
    expect(tok->type)->to_equal(CONSTANT);
    expect(tok->value)->to_equal("constant");
  });

  test("It should lex \"continue\"", lambda () {
    Token tok = Lexer("continue")->lex();
    expect(tok->type)->to_equal(CONTINUE);
    expect(tok->value)->to_equal("continue");
  });

  test("It should lex \"default\"", lambda () {
    Token tok = Lexer("default")->lex();
    expect(tok->type)->to_equal(DEFAULT);
    expect(tok->value)->to_equal("default");
  });

  test("It should lex \"do\"", lambda () {
    Token tok = Lexer("do")->lex();
    expect(tok->type)->to_equal(DO);
    expect(tok->value)->to_equal("do");
  });

  test("It should lex \"else\"", lambda () {
    Token tok = Lexer("else")->lex();
    expect(tok->type)->to_equal(ELSE);
    expect(tok->value)->to_equal("else");
  });

  test("It should lex \"enum\"", lambda () {
    Token tok = Lexer("enum")->lex();
    expect(tok->type)->to_equal(ENUM);
    expect(tok->value)->to_equal("enum");
  });

  test("It should lex \"extern\"", lambda () {
    Token tok = Lexer("extern")->lex();
    expect(tok->type)->to_equal(EXTERN);
    expect(tok->value)->to_equal("extern");
  });

  test("It should lex \"final\"", lambda () {
    Token tok = Lexer("final")->lex();
    expect(tok->type)->to_equal(FINAL_ID);
    expect(tok->value)->to_equal("final");
  });

  test("It should lex \"float\"", lambda () {
    Token tok = Lexer("float")->lex();
    expect(tok->type)->to_equal(FLOAT_ID);
    expect(tok->value)->to_equal("float");
  });

  test("It should lex \"for\"", lambda () {
    Token tok = Lexer("for")->lex();
    expect(tok->type)->to_equal(FOR);
    expect(tok->value)->to_equal("for");
  });

  test("It should lex \"foreach\"", lambda () {
    Token tok = Lexer("foreach")->lex();
    expect(tok->type)->to_equal(FOREACH);
    expect(tok->value)->to_equal("foreach");
  });

  test("It should lex \"function\"", lambda () {
    Token tok = Lexer("function")->lex();
    expect(tok->type)->to_equal(FUNCTION_ID);
    expect(tok->value)->to_equal("function");
  });

  test("It should lex \"gauge\"", lambda () {
    Token tok = Lexer("gauge")->lex();
    expect(tok->type)->to_equal(GAUGE);
    expect(tok->value)->to_equal("gauge");
  });

  test("It should lex \"global\"", lambda () {
    Token tok = Lexer("global")->lex();
    expect(tok->type)->to_equal(GLOBAL);
    expect(tok->value)->to_equal("global");
  });

  test("It should lex \"if\"", lambda () {
    Token tok = Lexer("if")->lex();
    expect(tok->type)->to_equal(IF);
    expect(tok->value)->to_equal("if");
  });

  test("It should lex \"import\"", lambda () {
    Token tok = Lexer("import")->lex();
    expect(tok->type)->to_equal(IMPORT);
    expect(tok->value)->to_equal("import");
  });

  test("It should lex \"inherit\"", lambda () {
    Token tok = Lexer("inherit")->lex();
    expect(tok->type)->to_equal(INHERIT);
    expect(tok->value)->to_equal("inherit");
  });

  test("It should lex \"inline\"", lambda () {
    Token tok = Lexer("inline")->lex();
    expect(tok->type)->to_equal(INLINE);
    expect(tok->value)->to_equal("inline");
  });

  test("It should lex \"int\"", lambda () {
    Token tok = Lexer("int")->lex();
    expect(tok->type)->to_equal(INT_ID);
    expect(tok->value)->to_equal("int");
  });

  test("It should lex \"lambda\"", lambda () {
    Token tok = Lexer("lambda")->lex();
    expect(tok->type)->to_equal(LAMBDA);
    expect(tok->value)->to_equal("lambda");
  });

  test("It should lex \"local\"", lambda () {
    Token tok = Lexer("local")->lex();
    expect(tok->type)->to_equal(LOCAL_ID);
    expect(tok->value)->to_equal("local");
  });

  test("It should lex \"mixed\"", lambda () {
    Token tok = Lexer("mixed")->lex();
    expect(tok->type)->to_equal(MIXED_ID);
    expect(tok->value)->to_equal("mixed");
  });

  test("It should lex \"multiset\"", lambda () {
    Token tok = Lexer("multiset")->lex();
    expect(tok->type)->to_equal(MULTISET_ID);
    expect(tok->value)->to_equal("multiset");
  });

  test("It should lex \"object\"", lambda () {
    Token tok = Lexer("object")->lex();
    expect(tok->type)->to_equal(OBJECT_ID);
    expect(tok->value)->to_equal("object");
  });

  test("It should lex \"optional\"", lambda () {
    Token tok = Lexer("optional")->lex();
    expect(tok->type)->to_equal(OPTIONAL);
    expect(tok->value)->to_equal("optional");
  });

  test("It should lex \"predef\"", lambda () {
    Token tok = Lexer("predef")->lex();
    expect(tok->type)->to_equal(PREDEF);
    expect(tok->value)->to_equal("predef");
  });

  test("It should lex \"private\"", lambda () {
    Token tok = Lexer("private")->lex();
    expect(tok->type)->to_equal(PRIVATE);
    expect(tok->value)->to_equal("private");
  });

  test("It should lex \"protected\"", lambda () {
    Token tok = Lexer("protected")->lex();
    expect(tok->type)->to_equal(PROTECTED);
    expect(tok->value)->to_equal("protected");
  });

  test("It should lex \"public\"", lambda () {
    Token tok = Lexer("public")->lex();
    expect(tok->type)->to_equal(PUBLIC);
    expect(tok->value)->to_equal("public");
  });

  test("It should lex \"return\"", lambda () {
    Token tok = Lexer("return")->lex();
    expect(tok->type)->to_equal(RETURN);
    expect(tok->value)->to_equal("return");
  });

  test("It should lex \"sscanf\"", lambda () {
    Token tok = Lexer("sscanf")->lex();
    expect(tok->type)->to_equal(SSCANF);
    expect(tok->value)->to_equal("sscanf");
  });

  test("It should lex \"static\"", lambda () {
    Token tok = Lexer("static")->lex();
    expect(tok->type)->to_equal(STATIC);
    expect(tok->value)->to_equal("static");
  });

  test("It should lex \"string\"", lambda () {
    Token tok = Lexer("string")->lex();
    expect(tok->type)->to_equal(STRING_ID);
    expect(tok->value)->to_equal("string");
  });

  test("It should lex \"switch\"", lambda () {
    Token tok = Lexer("switch")->lex();
    expect(tok->type)->to_equal(SWITCH);
    expect(tok->value)->to_equal("switch");
  });

  test("It should lex \"typedef\"", lambda () {
    Token tok = Lexer("typedef")->lex();
    expect(tok->type)->to_equal(TYPEDEF);
    expect(tok->value)->to_equal("typedef");
  });

  test("It should lex \"typeof\"", lambda () {
    Token tok = Lexer("typeof")->lex();
    expect(tok->type)->to_equal(TYPEOF);
    expect(tok->value)->to_equal("typeof");
  });

  test("It should lex \"variant\"", lambda () {
    Token tok = Lexer("variant")->lex();
    expect(tok->type)->to_equal(VARIANT);
    expect(tok->value)->to_equal("variant");
  });

  test("It should lex \"void\"", lambda () {
    Token tok = Lexer("void")->lex();
    expect(tok->type)->to_equal(VOID_ID);
    expect(tok->value)->to_equal("void");
  });

  test("It should lex \"while\"", lambda () {
    Token tok = Lexer("while")->lex();
    expect(tok->type)->to_equal(WHILE);
    expect(tok->value)->to_equal("while");
  });

  test("It should lex \"zero\"", lambda () {
    Token tok = Lexer("zero")->lex();
    expect(tok->type)->to_equal(ZERO_ID);
    expect(tok->value)->to_equal("zero");
  });

  test("It should be at next position after lexing keyword", lambda () {
    Lexer lexer = Lexer("array()");
    Token tok = lexer->lex();
    expect(tok->type)->to_equal(ARRAY_ID);
    expect(tok->value)->to_equal("array");

    tok = lexer->lex();
    expect(tok->value)->to_equal("(");
    expect(tok->type)->to_equal(PAREN_LEFT);

    tok = lexer->lex();
    expect(tok->type)->to_equal(PAREN_RIGHT);
    expect(tok->value)->to_equal(")");
  });
});

/*******************************************************************************

  Lex comments

*******************************************************************************/
describe("Comments", lambda () {
  test("It should lex a line comment with trailing newline", lambda () {
    Token tok = Lexer("// a comment\n")->lex();
    expect(tok->type)->to_equal(COMMENT);
    expect(tok->value)->to_equal(" a comment");
  });

  test("It should lex a line comment at end of input", lambda () {
    Token tok = Lexer("// a comment")->lex();
    expect(tok->type)->to_equal(COMMENT);
    expect(tok->value)->to_equal(" a comment");
  });

  test("It should lex a doc comment with trailing newline", lambda () {
    Token tok = Lexer("//! a doc comment\n")->lex();
    expect(tok->type)->to_equal(DOC_COMMENT);
    expect(tok->value)->to_equal(" a doc comment");
  });

  test("It should lex a doc comment at end of input", lambda () {
    Token tok = Lexer("//! a doc comment")->lex();
    expect(tok->type)->to_equal(DOC_COMMENT);
    expect(tok->value)->to_equal(" a doc comment");
  });

  test("It should lex a block comment", lambda () {
    string comment = #"/* A block comment */";
    Token tok = Lexer(comment)->lex();
    expect(tok->type)->to_equal(BLOCK_COMMENT);
    expect(tok->value)->to_equal(" A block comment ");
  });

  test("It should lex a multiline block comment", lambda () {
    string comment = #"/*
    A block comment
    over multiple lines
    */";
    Token tok = Lexer(comment)->lex();
    expect(tok->type)->to_equal(BLOCK_COMMENT);
    expect(tok->value)->to_equal(
      "\n    A block comment\n    over multiple lines\n    "
    );
  });

  test("It should throw on an unterminated block comment", lambda () {
    Lexer lexer = Lexer("/* Comment");
    expect(lambda () { lexer->lex(); })->to_throw();
  });

  test("It should add next line on line continuation", lambda () {
    Lexer l = Lexer("// This continues \
    on the next line");
    Token t = l->lex();
    expect(t->value)->to_equal(" This continues     on the next line");
  });
});

/*******************************************************************************

  Start and end positions

*******************************************************************************/
describe("Start and end positions", lambda () {
  test("Simple token", lambda () {
    Token tok = Lexer("(")->lex();
    expect((mapping)tok->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 1, "column": 2, "line": 1 ]),
      "file": "stdin",
    ]));

    tok = Lexer("({")->lex();
    expect((mapping)tok->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 2, "column": 3, "line": 1 ]),
      "file": "stdin",
    ]));

    tok = Lexer("__attribute__")->lex();
    expect((mapping)tok->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 13, "column": 14, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("One line block comment", lambda () {
    Token tok = Lexer("/* Block comment */ ")->lex();
    expect((mapping)tok->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 19, "column": 20, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("Multiline block comment", lambda () {
    Token tok = Lexer(#"
/* Block comment
 * over multiple lines
 */ "
    )->lex();

    expect((mapping)tok->location)->to_equal(([
      "start": ([ "byte": 1, "column": 1, "line": 2 ]),
      "end": ([ "byte": 44, "column": 4, "line": 4 ]),
      "file": "stdin",
    ]));
  });

  test("Multiple consecutive tokens", lambda () {
    Lexer lexer = Lexer("({ })   ->\t// comment");

    Token t = lexer->lex();
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1]),
      "end": ([ "byte": 2, "column": 3, "line": 1]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 3, "column": 4, "line": 1]),
      "end": ([ "byte": 5, "column": 6, "line": 1]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 8, "column": 9, "line": 1]),
      "end": ([ "byte": 10, "column": 11, "line": 1]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 11, "column": 12, "line": 1]),
      "end": ([ "byte": 21, "column": 22, "line": 1]),
      "file": "stdin",
    ]));
  });
});

/*******************************************************************************

  Strings

*******************************************************************************/
describe("Strings", lambda () {
  test("It should lex a simple string", lambda () {
    Token t = Lexer("\"A simple string\"")->lex();
    expect(t->type)->to_equal(STRING);
    expect(t->value)->to_equal("A simple string");
  });

  test("It should throw on unterminated string literal", lambda () {
    string expmsg =
      "Unterminated string literal\n"
      "    at byte range 0..15, column 1..16 on line 1\n";
    expect(lambda() { Lexer("\"Unte\\\"rminated")->lex(); })->to_throw(expmsg);
  });

  test("Token after string should have correct position", lambda () {
    Lexer lexer = Lexer("\"a string\"({");

    Token t = lexer->lex();
    expect(t->value)->to_equal("a string");

    t = lexer->lex();
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 10, "column": 11, "line": 1 ]),
      "end": ([ "byte": 12, "column": 13, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("It should handle newline continuations", lambda () {
    Lexer l = Lexer("\"This is a \
    string\"");
    Token t = l->lex();
    expect(t->value)->to_equal("This is a     string");
  });

  test("It should lex a character literal", lambda () {
    Lexer lexer = Lexer("'\t'&");
    Token tok = lexer->lex();
    expect(tok->type)->to_equal(CHAR);
    expect(tok->value)->to_equal("\t");
    tok = lexer->lex();
    expect(tok->type)->to_equal(AMP);
  });

  test("It should lex a multiline string", lambda () {
    Lexer l = Lexer(#"#\"
      multiline string
      goes here
    \";
    int post_string = 12;
    ");

    Token tok = l->lex();
    expect(tok->type)->to_equal(STRING);
    expect(tok->value)->to_equal(#"
      multiline string
      goes here
    ");
    expect((mapping)tok->location->start)->to_equal(([
      "byte": 0,
      "column": 1,
      "line": 1,
    ]));
    expect((mapping)tok->location->end)->to_equal(([
      "byte": 47,
      "column": 6,
      "line": 4,
    ]));

    tok = l->lex();
    expect(tok->type)->to_equal(SEMICOLON);
    expect(tok->value)->to_equal(";");
    expect(tok->location->start->byte)->to_equal(47);
    expect(tok->location->start->column)->to_equal(6);
    expect(tok->location->start->line)->to_equal(4);
  });
});

/*******************************************************************************

  Numbers

*******************************************************************************/
describe("Numbers", lambda () {
  test("It should lex integers", lambda () {
    Lexer lexer = Lexer("190 1");
    Token t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("190");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 3, "column": 4, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("1");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 4, "column": 5, "line": 1 ]),
      "end": ([ "byte": 5, "column": 6, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test(
    "It should NOT throw on an integer longer than 1 char starting with 0",
    lambda () {
      Lexer lexer = Lexer("0755");
      expect(lexer->lex()->value)->to_equal("0755");
    }
  );

  test("It should lex negative integers", lambda () {
    Lexer lexer = Lexer("-1 -44");
    Token t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("-1");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 2, "column": 3, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("-44");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 3, "column": 4, "line": 1 ]),
      "end": ([ "byte": 6, "column": 7, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("It should lex floating point numbers", lambda () {
    Lexer lexer = Lexer("190.01 1.1");
    Token t = lexer->lex();
    expect(t->type)->to_equal(FLOAT);
    expect(t->value)->to_equal("190.01");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 6, "column": 7, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(FLOAT);
    expect(t->value)->to_equal("1.1");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 7, "column": 8, "line": 1 ]),
      "end": ([ "byte": 10, "column": 11, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("It should lex floating point numbers with a leading \".\"", lambda () {
    Lexer lexer = Lexer(".01");
    Token t = lexer->lex();
    expect(t->type)->to_equal(FLOAT);
    expect(t->value)->to_equal(".01");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 3, "column": 4, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test(
    "It should lex negative floating point numbers with a leading \".\"",
    lambda () {
      Lexer lexer = Lexer("-.01");
      Token t = lexer->lex();
      expect(t->type)->to_equal(FLOAT);
      expect(t->value)->to_equal("-.01");
      expect((mapping)t->location)->to_equal(([
        "start": ([ "byte": 0, "column": 1, "line": 1 ]),
        "end": ([ "byte": 4, "column": 5, "line": 1 ]),
        "file": "stdin",
      ]));
    }
  );

  test("It should lex hexadecimal numbers", lambda () {
    Lexer lexer = Lexer("0xFF 0X0 0x0123456789ABCDEFabcdefGHI");
    Token t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("0xFF");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 4, "column": 5, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("0X0");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 5, "column": 6, "line": 1 ]),
      "end": ([ "byte": 8, "column": 9, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("0x0123456789ABCDEFabcdef");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 9, "column": 10, "line": 1 ]),
      "end": ([ "byte": 33, "column": 34, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("It should lex negative hexadecimal numbers", lambda () {
    Lexer lexer = Lexer("-0xFF");
    Token t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("-0xFF");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 5, "column": 6, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("It should lex exponential integers", lambda () {
    Lexer lexer = Lexer("10e21 10E21 10e+10 1e-5");
    Token t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("10e21");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 5, "column": 6, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("10E21");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 6, "column": 7, "line": 1 ]),
      "end": ([ "byte": 11, "column": 12, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("10e+10");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 12, "column": 13, "line": 1 ]),
      "end": ([ "byte": 18, "column": 19, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("1e-5");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 19, "column": 20, "line": 1 ]),
      "end": ([ "byte": 23, "column": 24, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("It should lex exponential floating point numbers", lambda () {
    Lexer lexer = Lexer("1.234e21 10.23E21 10.0e+10 1.150e-5");

    Token t = lexer->lex();
    expect(t->type)->to_equal(FLOAT);
    expect(t->value)->to_equal("1.234e21");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 8, "column": 9, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(FLOAT);
    expect(t->value)->to_equal("10.23E21");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 9, "column": 10, "line": 1 ]),
      "end": ([ "byte": 17, "column": 18, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(FLOAT);
    expect(t->value)->to_equal("10.0e+10");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 18, "column": 19, "line": 1 ]),
      "end": ([ "byte": 26, "column": 27, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(FLOAT);
    expect(t->value)->to_equal("1.150e-5");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 27, "column": 28, "line": 1 ]),
      "end": ([ "byte": 35, "column": 36, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("It should lex binary numbers", lambda () {
    Lexer lexer = Lexer("0b01 0B110");

    Token t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("0b01");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 4, "column": 5, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("0B110");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 5, "column": 6, "line": 1 ]),
      "end": ([ "byte": 10, "column": 11, "line": 1 ]),
      "file": "stdin",
    ]));
  });

  test("It should lex negative binary numbers", lambda () {
    Lexer lexer = Lexer("-0b01 -0B110");

    Token t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("-0b01");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 0, "column": 1, "line": 1 ]),
      "end": ([ "byte": 5, "column": 6, "line": 1 ]),
      "file": "stdin",
    ]));

    t = lexer->lex();
    expect(t->type)->to_equal(NUMBER);
    expect(t->value)->to_equal("-0B110");
    expect((mapping)t->location)->to_equal(([
      "start": ([ "byte": 6, "column": 7, "line": 1 ]),
      "end": ([ "byte": 12, "column": 13, "line": 1 ]),
      "file": "stdin",
    ]));
  });
});


/*******************************************************************************

  Symbol names

*******************************************************************************/
describe("Symbol names", lambda () {
  test("It should lex a simple symbol", lambda () {
    Lexer lexer = Lexer("my_symbol");
    Token t = lexer->lex();
    expect(t->type)->to_equal(SYMBOL_NAME);
    expect(t->value)->to_equal("my_symbol");
  });

  test("It should lex a simple symbol with leading underscore", lambda () {
    Lexer lexer = Lexer("_my_symbol");
    Token t = lexer->lex();
    expect(t->type)->to_equal(SYMBOL_NAME);
    expect(t->value)->to_equal("_my_symbol");
  });

  test("It should lex a simple symbol with leading dunderscore", lambda () {
    Lexer lexer = Lexer("__my_symbol");
    Token t = lexer->lex();
    expect(t->type)->to_equal(SYMBOL_NAME);
    expect(t->value)->to_equal("__my_symbol");
  });

  test("It should handle Pike special overload method names", lambda () {
    Lexer l = Lexer("`()(array in){}");
    array(Token) toks = ({});

    while (Token t = l->lex()) {
      toks += ({ t });
    }

    expect(toks[0]->type)->to_equal(SYMBOL_NAME);
    expect(toks[0]->value)->to_equal("`()");
    expect(toks[1]->type)->to_equal(PAREN_LEFT);

    l = Lexer("`^(mixed in){}");
    toks = ({});

    while (Token t = l->lex()) {
      toks += ({ t });
    }

    expect(toks[0]->type)->to_equal(SYMBOL_NAME);
    expect(toks[0]->value)->to_equal("`^");
    expect(toks[1]->type)->to_equal(PAREN_LEFT);
    expect((mapping)toks[1]->location->start)
      ->to_equal(([ "byte": 2, "column": 3, "line": 1 ]));

    Token t;
    t = Lexer("`%(){}")->lex();
    expect(t->value)->to_equal("`%");
    t = Lexer("`&(){}")->lex();
    expect(t->value)->to_equal("`&");
    t = Lexer("`+(){}")->lex();
    expect(t->value)->to_equal("`+");
    t = Lexer("`/(){}")->lex();
    expect(t->value)->to_equal("`/");
    t = Lexer("`|(){}")->lex();
    expect(t->value)->to_equal("`|");
    t = Lexer("`~(){}")->lex();
    expect(t->value)->to_equal("`~");
    t = Lexer("`!(){}")->lex();
    expect(t->value)->to_equal("`!");
    l = Lexer("`!=(){}");
    t = l->lex();
    expect(t->value)->to_equal("`!=");
    t = l->lex();
    expect(t->value)->to_equal("(");
    expect((mapping)t->location->start)
      ->to_equal(([ "byte": 3, "column": 4, "line": 1  ]));
    t = Lexer("`*(){}")->lex();
    expect(t->value)->to_equal("`*");
    t = Lexer("`**(){}")->lex();
    expect(t->value)->to_equal("`**");
    t = Lexer("`-(){}")->lex();
    expect(t->value)->to_equal("`-");
    t = Lexer("`->(){}")->lex();
    expect(t->value)->to_equal("`->");
    t = Lexer("`->=(){}")->lex();
    expect(t->value)->to_equal("`->=");
    t = Lexer("`<<(){}")->lex();
    expect(t->value)->to_equal("`<<");
    t = Lexer("`<=(){}")->lex();
    expect(t->value)->to_equal("`<=");
    t = Lexer("`<(){}")->lex();
    expect(t->value)->to_equal("`<");
    t = Lexer("`==(){}")->lex();
    expect(t->value)->to_equal("`==");
    t = Lexer("`>>(){}")->lex();
    expect(t->value)->to_equal("`>>");
    t = Lexer("`>=(){}")->lex();
    expect(t->value)->to_equal("`>=");
    t = Lexer("`>(){}")->lex();
    expect(t->value)->to_equal("`>");
    t = Lexer("`[](){}")->lex();
    expect(t->value)->to_equal("`[]");
    l = Lexer("`[..](){}");
    t = l->lex();
    expect(t->value)->to_equal("`[..]");
    t = l->lex();
    expect((mapping)t->location->start)
      ->to_equal(([ "byte": 5, "column": 6, "line": 1 ]));
    t = Lexer("`[]=(){}")->lex();
    expect(t->value)->to_equal("`[]=");
  });

  test("It should lex user defined getters and setters", lambda () {
    Token t;
    t = Lexer("`name(){}")->lex();
    expect(t->type)->to_equal(SYMBOL_NAME);
    expect(t->value)->to_equal("`name");
    t = Lexer("`name=(){}")->lex();
    expect(t->value)->to_equal("`name=");
    t = Lexer("`name_name(){}")->lex();
    expect(t->value)->to_equal("`name_name");
    t = Lexer("`name_name=(){}")->lex();
    expect(t->value)->to_equal("`name_name=");
  });
});

/*******************************************************************************

  More complex stuff

*******************************************************************************/
describe("More complex stuff", lambda () {
  test("It should lex array index range", lambda () {
    Lexer l = Lexer("my_array[1..]");
    l->lex();
    l->lex();
    l->lex();
    Token t = l->lex();
    expect(t->type)->to_equal(DOT_DOT);
    expect(t->value)->to_equal("..");
  });

  test("It should lex argument spread", lambda () {
    Lexer l = Lexer("myfn(mixed ... args)");
    l->lex();
    l->lex();
    l->lex();
    Token t = l->lex();
    expect(t->type)->to_equal(DOT_DOT_DOT);
    expect(t->value)->to_equal("...");
  });

  test("It should lex a range in a switch-case", lambda () {
    Lexer l = Lexer("case 'a'..'z'");
    array(Token) ts = ({});
    while (Token t = l->lex()) {
      ts += ({ t });
    }

    expect(sizeof(ts))->to_equal(4);
    expect(ts[2]->type)->to_equal(DOT_DOT);
    expect(ts[2]->value)->to_equal("..");
    expect(ts[3]->type)->to_equal(CHAR);
  });

  test("It should do its one-line complex thing", lambda () {
    string code = #"array(string) name = ({ \"Pike\" });";
    Lexer lexer = Lexer(code);
    array(object(Token)) tokens = ({});

    while (Token t = lexer->lex()) {
      tokens += ({ t });
    }

    expect(sizeof(tokens))->to_equal(10);
    expect(tokens[0]->type)->to_equal(ARRAY_ID);
    expect(tokens[1]->type)->to_equal(PAREN_LEFT);
    expect(tokens[2]->type)->to_equal(STRING_ID);
    expect(tokens[3]->type)->to_equal(PAREN_RIGHT);
    expect(tokens[4]->type)->to_equal(SYMBOL_NAME);
    expect(tokens[5]->type)->to_equal(ASSIGN);
    expect(tokens[6]->type)->to_equal(ARRAY_START);
    expect(tokens[7]->type)->to_equal(STRING);
    expect(tokens[8]->type)->to_equal(ARRAY_END);
    expect(tokens[9]->type)->to_equal(SEMICOLON);
  });

  test("It should do its multi-line complex thing", lambda () {
    string code = #"
      class MyClass {
        inherit Stdio.File;

        protected float query(object in) {
          return 1.0;
        }
      }";

    Lexer lexer = Lexer(code);
    array(object(Token)) tokens = ({});

    while (Token t = lexer->lex()) {
      tokens += ({ t });
    }

    expect(sizeof(tokens))->to_equal(21);

    expect(tokens[0]->type)->to_equal(CLASS);        // class
    expect(tokens[1]->type)->to_equal(SYMBOL_NAME);  // MyClass
    expect(tokens[2]->type)->to_equal(CURLY_LEFT);   // {
    expect(tokens[3]->type)->to_equal(INHERIT);      // inherit
    expect(tokens[4]->type)->to_equal(SYMBOL_NAME);  // Stdion
    expect(tokens[5]->type)->to_equal(DOT);          // .
    expect(tokens[6]->type)->to_equal(SYMBOL_NAME);  // File
    expect(tokens[7]->type)->to_equal(SEMICOLON);    // ;
    expect(tokens[8]->type)->to_equal(PROTECTED);    // protected
    expect(tokens[9]->type)->to_equal(FLOAT_ID);     // float
    expect(tokens[10]->type)->to_equal(SYMBOL_NAME); // query
    expect(tokens[11]->type)->to_equal(PAREN_LEFT);  // (
    expect(tokens[12]->type)->to_equal(OBJECT_ID);   // object
    expect(tokens[13]->type)->to_equal(SYMBOL_NAME); // in
    expect(tokens[14]->type)->to_equal(PAREN_RIGHT); // )
    expect(tokens[15]->type)->to_equal(CURLY_LEFT);  // {
    expect(tokens[16]->type)->to_equal(RETURN);      // return
    expect(tokens[17]->type)->to_equal(FLOAT);       // 1.0
    expect(tokens[18]->type)->to_equal(SEMICOLON);   // ;
    expect(tokens[19]->type)->to_equal(CURLY_RIGHT); // }
    expect(tokens[20]->type)->to_equal(CURLY_RIGHT); // }

    // First curly left
    expect((mapping)tokens[2]->location->start)
      ->to_equal(([ "byte": 21, "column": 21, "line": 2 ]));
    expect((mapping)tokens[2]->location->end)
      ->to_equal(([ "byte": 22, "column": 22, "line": 2 ]));

    // File
    expect((mapping)tokens[6]->location->start)
      ->to_equal(([ "byte": 45, "column": 23, "line": 3 ]));
    expect((mapping)tokens[6]->location->end)
      ->to_equal(([ "byte": 49, "column": 27, "line": 3 ]));

    // Semicolon after 1.0
    expect((mapping)tokens[18]->location->start)
      ->to_equal(([ "byte": 115, "column": 21, "line": 6 ]));
    expect((mapping)tokens[18]->location->end)
      ->to_equal(([ "byte": 116, "column": 22, "line": 6 ]));
  });
});

/*******************************************************************************

  Preprocessor Macros

*******************************************************************************/
describe("Preprocessor Macros", lambda () {
  test("It should lex a #pragma directive", lambda () {
    Lexer l = Lexer("#pragma strict_types\nstring");
    Token t = l->lex();
    expect(t->type)->to_equal(MACRO_DIR);
    expect(t->value)->to_equal("pragma");

    t = l->lex();
    expect(t->type)->to_equal(MACRO_LITERAL);
    expect(t->value)->to_equal("strict_types");
    expect((mapping)t->location->start)
      ->to_equal(([ "byte":  8, "column": 9, "line": 1 ]));
  });

  test("It should manage unquoted preproc values", lambda () {
    Lexer l = Lexer("#include <macro.h>");
    Token t = l->lex() && l->lex();
    expect(t->type)->to_equal(MACRO_LITERAL);
    expect(t->value)->to_equal("<macro.h>");

    l = Lexer("#charset utf-8");
    t = l->lex() && l->lex();
    expect(t->type)->to_equal(MACRO_LITERAL);
    expect(t->value)->to_equal("utf-8");
  });

  test("It should manage a online pound-define alias", lambda () {
    Lexer l = Lexer("#define REGEX Regexp.PCRE.Widestring\nint");
    array(Token) toks = ({});

    while (Token t = l->lex()) {
      toks += ({ t });
    }

    expect(sizeof(toks))->to_equal(8);
    expect(toks[0]->type)->to_equal(MACRO_DIR);
  });

  test("It should manage non-unquoted preproc stuff", lambda () {
    Lexer l = Lexer("#require constant(Pest)");
    array(Token) tokens = ({});
    while (Token t = l->lex()) {
      tokens += ({ t });
    }

    expect(sizeof(tokens))->to_equal(5);
    expect(tokens[0]->type)->to_equal(MACRO_DIR);
    expect(tokens[1]->type)->to_equal(CONSTANT);
    expect(tokens[2]->type)->to_equal(PAREN_LEFT);
    expect(tokens[3]->type)->to_equal(SYMBOL_NAME);
    expect(tokens[4]->type)->to_equal(PAREN_RIGHT);
  });

  test("It should manage a multiline pound-define", lambda () {
    Lexer l = Lexer(#"
    #define READ_NUM(N) do { \
      werror(\"Dude\");      \
    } while (0)

    string key = \"a\";
    ");
    array(Token) toks = ({});

    while (Token t = l->lex()) {
      toks += ({ t });
    }

    expect(sizeof(toks))->to_equal(22);
    expect(toks[0]->context)->to_equal(LEX_STATE_PREPROC_DEFINE);
    expect(toks[16]->type)->to_equal(PAREN_RIGHT);
    expect(toks[16]->context)->to_equal(LEX_STATE_PREPROC_DEFINE);
    expect(toks[17]->context)->to_equal(LEX_STATE_DEFAULT);
  });
});

// end main
}
