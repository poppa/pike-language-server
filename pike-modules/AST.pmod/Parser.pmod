#charset utf-8
#pike __REAL_VERSION__

import .Lexer;
import .Token;
import .Node;

#include "parser.h"

public typedef Type|array(Type) expect_t;

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

  public Token accept_next(expect_t t)
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

  public Token prev_token()
  //! Get the previous token
  {
    if (sizeof(used_tokens)) {
      return used_tokens[-1];
    }
  }

  public void expect(expect_t t)
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

  public variant void expect(
    function(Type|Token:bool) fn,
    Token|Type t,
    string expected_desc
  ) {
    if (!fn(t)) {
      error(
        "Expected token to be %s but got %q",
        expected_desc,
        type_to_string(t)
      );
    }
  }

  public void expect_next(expect_t t)
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
    expect(({ EOF, SEMICOLON }));

    return root;
  }

  protected object(Program) do_program() {
    next_token();
    object(Program) p = make_node(Program, current_token->location);

    while (current_token->type != EOF) {
      TRACE("Current token in do_program loop?: %O\n", current_token);

      if (current_token->type == STATIC_ASSERT) {
        TODO("Handle Static_assert()");
      } else if (current_token->type == IMPORT) {
        Node is = do_import();
        accept_next(SEMICOLON);
        p->body += ({ is });
      } else if (current_token->type == AT) {
        TODO("Handle annotation?");
      } else if (is_modifier(current_token)) {
        array(Annotation|Modifier) mods = do_modifiers();
        p->body += mods;

        expect(({
          CURLY_LEFT,
          CLASS,
          ENUM,
          TYPEDEF,
          INHERIT,
          IMPORT,
          CONSTANT,
          AT,
          STATIC_ASSERT,
          DEPRECATED_ID,
          ATTRIBUTE_ID,
        }));

        continue;
      } else if (current_token->type == CURLY_LEFT) {
        TODO("Handle block after modifier");
      } else if (current_token->type == CLASS) {
        TODO("Handle class after modifer");
      } else if (current_token->type == ENUM) {
        TODO("Handle enum after modifer");
      } else if (current_token->type == TYPEDEF) {
        TODO("Handle typedef efter modifer");
      } else if (current_token->type == CONSTANT) {
        TODO("Handle constant");
      } else if (current_token->type == AT) {
        TODO("Handle annotation");
      } else if (is_attribute(current_token)) {
        TODO("Handle attribute");
      } else {
        TODO("Uninmplemented or syntax error: %O\n", current_token);
      }

      next_token();
    }


    return p;
  }

  protected void do_block() {
    Token t = next_token();
    TRACE("Next token in do_block(): %O\n", t);
  }

  protected Import do_import() {
    expect(IMPORT);
    Import s = make_node(Import, current_token->location);

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

  protected array(Annotation|Modifier) do_modifiers() {
    array(Annotation) anonlist = do_annotation_list();
    array(Modifier) mods = do_modifier_list();

    array(Annotation|Modifier) out = ({});

    if (sizeof(anonlist || ({}))) {
      out += anonlist;
    }

    if (sizeof(mods)) {
      out += mods;
    }

    return out;
  }

  protected array(Modifier) do_modifier_list() {
    array(Modifier) mods = ({});
    while (is_modifier(current_token)) {
      mods += ({ do_modifier() });
      next_token();
    }

    return mods;
  }

  protected Modifier do_modifier() {
    expect(is_modifier, current_token, "'Modifier'");

    return make_node(Modifier,current_token->location, ([
      "name": current_token->value,
      "type": current_token->type,
    ]));
  }

  protected mixed do_annotation_list() {

  }

  protected mixed do_annotation() {

  }
}
