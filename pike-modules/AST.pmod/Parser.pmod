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

  public Token next_token() {
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

  public Token peek_token() {
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

  public void expect(Type t) {
    if (current_token->type != t) {
      error(
        "Expected %q got %q",
        type_to_string(t),
        type_to_string(current_token)
      );
    }
  }

  public void parse();
}

public class PikeParser {
  inherit BaseParser;

  public object(Program) parse() {
    object(Program) root = do_program();

    Token t = next_token();

    if (t != UNDEFINED) {
      TODO("Unexpected token after end\n");
    }

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
      next_token();
      expect(SEMICOLON);
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

        if (peek_token()->type != SEMICOLON) {
          error("Expected SEMICOLN but got %O", type_to_string(peek_token()));
        }

        break;
      }

      expect(IDENTIFIER);

      Identifier id = make_node(
        Identifier,
        t->location,
        ([ "name" : t->value ])
      );

      Token next = peek_token();

      if (next->type != DOT && next->type != SEMICOLON) {
        error("Expected DOT or SEMICOLON but got %O", type_to_string(next));
      }

      if (next->type == DOT) {
        next_token();
      }

      s->identifiers += ({ id });
    }

    return s;
  }
}
