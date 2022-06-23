#charset utf-8
#pike __REAL_VERSION__

import .Lexer;
import .Token;
import .Node;

#include "parser.h"

public class BaseParser {
  protected BaseLexer _lexer;
  protected ADT.Queue token_queue = ADT.Queue();
  protected Token current_token;
  protected Token peeked_token;
  protected array(Token) used_tokens = ({});

  protected void create(BaseLexer lexer) {
    this::_lexer = lexer;
  }

  public BaseLexer `lexer() {
    return _lexer;
  }

  public Token next_token()
  //! Fetch the next token
  {
    if (current_token) {
      used_tokens += ({ current_token });
    }

    if (peeked_token) {
      current_token = peeked_token;
      peeked_token = UNDEFINED;

      return current_token;
    }

    return current_token = _lexer->lex();
  }

  public Token accept_next(Type|array(Type) t)
  //! Fetch the next token and expect it to be of type @[t]
  {
    next_token();
    expect(t);
    return current_token;
  }

  public Token peek_token()
  //! Peek the next token
  {
    if (peeked_token) {
      return peeked_token;
    }

    return peeked_token = _lexer->lex();
  }

  public Token prev_token() {
    if (sizeof(used_tokens)) {
      return used_tokens[-1];
    }
  }

  public void expect(Type|array(Type) t)
  //! Expect the @[current_token] to be of type @[t]
  {
    if (!arrayp(t)) {
      t = ({ t });
    }

    if (!has_value(t, current_token->type)) {
      array(string) exp_type = map(t, lambda (Type x) {
        return sprintf("%q", type_to_string(x));
      });

      error(
        "Expected token to be %s but got %q",
        String.implode_nicely(exp_type, "or"),
        type_to_string(current_token)
      );
    }
  }

  public void expect_next(Type|array(Type) t)
  //! Expect the next token to be of type @[t]
  {
    if (!arrayp(t)) {
      t = ({ t });
    }

    if (!has_value(t, peek_token()->type)) {
      array(string) exp_type = map(t, lambda (Type x) {
        return sprintf("%q", type_to_string(x));
      });

      error(
        "Expected next token to be %s but got %q",
        String.implode_nicely(exp_type, "or"),
        type_to_string(peek_token())
      );
    }
  }

  public void parse();
}

public class PikeParser {
  inherit BaseParser;

  public object(Program) parse() {
    object(Program) root = do_program();

    next_token();
    expect(EOF);

    return root;
  }

  protected object(Program) do_program() {
    Token t = next_token();

    object(Program) p = make_node(Program, t->location);

    TRACE("Next token in do_program(): %O\n", t);

    if (t->type == STATIC_ASSERT) {
      TODO("Handle Static_assert()");
    } else if (t->type == IMPORT) {
      Node is = do_import();
      accept_next(SEMICOLON);
      p->body += ({ is });
    } else if (t->type == AT) {
      TODO("Handle annotaton?");
    } else if (is_modifier(t)) {
      TODO("What's next after modifier");
    }

    return p;
  }

  protected void do_block() {
    Token t = next_token();
    TRACE("Next token in do_block(): %O\n", t);
  }

  protected ImportStatement do_import() {
    expect(IMPORT);
    ImportStatement s = make_node(ImportStatement, current_token->location);

    while (peek_token()->type != SEMICOLON) {
      Token t = next_token();

      if (t->type == STRING) {
        s->path = t->value;
        expect_next(SEMICOLON);
        break;
      }

      expect(IDENTIFIER);

      Identifier id = make_node(
        Identifier,
        t->location,
        ([ "name" : t->value ])
      );

      expect_next(({ DOT, SEMICOLON }));

      if (peek_token()->type == DOT) {
        next_token();
      }

      s->identifiers += ({ id });
    }

    return s;
  }
}
