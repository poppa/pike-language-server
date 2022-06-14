import .Lexer;
import .Token;

public class BaseParser {
  protected BaseLexer _lexer;
  protected ADT.Queue token_queue = ADT.Queue();
  protected Token current_token;
  protected array(Token) used_tokens = ({});

  protected void create(BaseLexer lexer) {
    this::_lexer = lexer;
  }

  public BaseLexer `lexer() {
    return _lexer;
  }

  protected Token next_token() {
    if (current_token) {
      used_tokens += ({ current_token });
    }

    return current_token = _lexer->lex();
  }

  public void parse();
}

public class PikeParser {
  inherit BaseParser;

  public void parse() {
    do_block();
  }

  protected void do_block() {
    Token t = next_token();

    werror("Next token: %O\n", t);
  }
}
