import AST.Token;
import AST.Lexer;

int main() {
  object file = Stdio.File("test/test-sources/huge.pike");
  array(Token) toks = ({});

  mixed res = gauge {
    Lexer l = Lexer(file);

    int i;
    while (Token t = l->lex()) {
      if (t->location->start->byte == 2818) {
        werror("Tok (%d): %O\n", i, t);
      }
      toks += ({ t });
      i++;
    }
  };

  werror("Tokenized the entire file: %O tokens in %O\n", sizeof(toks), res);

  werror("%O\n", toks[397..398]);

  return 0;
}
