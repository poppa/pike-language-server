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
    expect(lambda() { Lexer("\"Unte\\\"rminated")->lex(); })
      ->to_throw("Unterminated string literal\n");
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

  test("It should lex a character literal", lambda () {
    Lexer lexer = Lexer("'\t'&");
    Token tok = lexer->lex();
    expect(tok->type)->to_equal(CHAR);
    expect(tok->value)->to_equal("\t");
    tok = lexer->lex();
    expect(tok->type)->to_equal(AMP);
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
    "It should throw on an integer longer than 1 char starting with 0",
    lambda () {
      Lexer lexer = Lexer("02");
      expect(lambda() { lexer->lex(); })->to_throw();
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
  });
});

// end main
}