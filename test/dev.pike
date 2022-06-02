import AST.Token;
import AST.Lexer;

// class Test {
//   mixed `()() {

//   }

//   mixed `name() {
//     return "Its a name";
//   }
// }

int main() {
  object file = Stdio.File("test/test-sources/test1.pike");
  Lexer l = Lexer(file);
  array(Token) toks = ({});

  while (Token t = l->lex()) {
    toks += ({ t });
  }



  return 0;
}
