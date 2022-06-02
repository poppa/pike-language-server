import AST.Token;
import AST.Lexer;

int main() {
  object file = Stdio.File("test/test-sources/huge.pike");
  array(Token) toks = ({});

  mixed res = gauge {
    Lexer l = Lexer(file);

    while (Token t = l->lex()) {
      toks += ({ t });
    }
  };

  werror("Tokenized the entire file: %O tokens in %O\n", sizeof(toks), res);

  return 0;
}
