#charset utf-8
#pike __REAL_VERSION__

import .Lexer;
import .Token;
import .Node;

#include "parser.h"

public typedef Type|multiset(Type) expect_t;

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
    if (!multisetp(t)) {
      t = (< t >);
    }

    if (!t[current_token->type]) {
      array(string) exp_type = map((array(int)) t, lambda (Type x) {
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
    if (!multisetp(t)) {
      t = (< t >);
    }

    if (!t[peek_token()->type]) {
      array(string) exp_type = map((array(int)) t, lambda (Type x) {
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
    expect((< EOF, SEMICOLON >));

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
      }
      // Handle modifier
      else if (is_modifier(current_token)) {
        array(Annotation|Modifier) mods = do_modifiers();

        /*
          TODO: Potential stuff to follow...

                "{" program "}"
                "constant"
                "inherit"
                "typedef"
                "enum"
                "class"
                optional_attributes ... identifier/indentifier list
        */
        if (current_token->type == CURLY_LEFT) {
          TODO("Curly left after modifier -> recurse\n");
        }

        array(Node) attr;
        Node type;
        bool is_const = false;

        if (is_attribute(current_token)) {
          attr = ({});

          while (is_attribute(current_token)) {
            attr += ({ do_attribute() });
          }
        }

        int old_type = current_token->type;
        if (is_builtin_type(current_token)) {
          type = do_basic_type();
          accept_next((< CONSTANT, IDENTIFIER >));
        }

        if (current_token->type == CONSTANT) {
          is_const = true;
          next_token();
        }

        // This is either an identifier like a function/method/variable
        // declaration, or the start of a "list" of variable declarations
        expect(IDENTIFIER);

        Token peeked_token = peek_token();

        // Function declaration
        if (peeked_token->type == PAREN_LEFT) {
          // FIXME: Move to separate function?
          Location loc = current_token->location;
          Node fndecl =
            make_node(FunctionDeclaration, loc, ([
              "name": make_node(Identifier, loc, ([
                "name": current_token->value
              ])),
              "attribute": attr,
              "modifier": mods,
            ]));


          next_token();
          peeked_token = peek_token();

          if (peeked_token->type != PAREN_RIGHT) {
            TODO("Parse argument decl list");
          } else {
            next_token();
          }

          accept_next((< SEMICOLON, CURLY_LEFT >));

          if (current_token->type == SEMICOLON) {
            fndecl->is_prototype = true;
          } else {
            TODO("Function body...\n");
          }

          TODO("Function declaration: %O -> mods: %O, attrs: %O -> next_token: %O\n",
            fndecl,
            fndecl->modifier,
            fndecl->attribute,
            peeked_token,
          );
        } else {
          // Variable declaration
          TODO("Handle variable declaration\n");
        }

        TODO("mods: %O, type: %O, is const: %O -> %O\n",
          mods, type, is_const, current_token);

        continue;
      } else if (current_token->type == CURLY_LEFT) {
        TODO("Handle block -> scoped 'program'");
      } else if (current_token->type == CLASS) {
        TODO("Handle class in program");
      } else if (current_token->type == ENUM) {
        TODO("Handle enum in program");
      } else if (current_token->type == TYPEDEF) {
        TODO("Handle typedef in program");
      } else if (current_token->type == CONSTANT) {
        TODO("Handle constant in program\n");
      } else if (current_token->type == AT) {
        TODO("Handle annotation in program");
      } else if (is_attribute(current_token)) {
        TODO("Handle attribute in program");
      } else if (is_builtin_type(current_token)) {
        TODO("Handle built-in type in program");
      } else {
        TODO(
          "Uninmplemented or syntax error: %O -> body: %O\n",
          current_token,
          p->body
        );
      }

      next_token();
    }


    return p;
  }

  protected void do_block() {
    Token t = next_token();
    TRACE("Next token in do_block(): %O\n", t);
  }

  protected void|IntRangeType resolve_range() {
    next_token();
    Token n = next_token();
    expect((< NUMBER, DOT_DOT >));
    Token peeked = peek_token();
    IntRangeType ret;

    // string(..N)
    if (n->type == DOT_DOT) {
      next_token();
      expect(NUMBER);
      ret = make_node(IntRangeType, n->location, ([
        "range" : ({ UNDEFINED, (int)current_token->value })
      ]));
    }
    // string(N)
    else if (peeked->type == PAREN_RIGHT) {
      ret = make_node(IntRangeType, n->location, ([
        "range": (int)n->value
      ]));
    }
    // string(N..?)
    else if (peeked->type == DOT_DOT) {
      int start_value = (int)n->value;
      next_token();
      peeked = peek_token();

      // string(N..)
      if (peeked->type == PAREN_RIGHT) {
        ret = make_node(IntRangeType, n->location, ([
          "range" : ({ start_value, UNDEFINED })
        ]));
      } else if (peeked->type == NUMBER) {
        next_token();
        ret = make_node(IntRangeType, n->location, ([
          "range" : ({ start_value, (int)current_token->value })
        ]));
      } else {
        error("Expected something other than %O\n", peeked);
      }
    }
    // string(Nbit)
    else if (peeked->type == IDENTIFIER) {
      if (peeked->value != "bit") {
        error("Expexted \"bit\" got %q", peeked->value);
      }

      next_token();
      Location nloc = n->location;
      ret = make_node(IntRangeType, nloc, ([
        "range": make_node(Bits, nloc, ([ "width": (int)n->value ]))
      ]));
    }

    return ret;
  }

  protected mixed do_basic_type() {
    expect(is_builtin_type, current_token, "\"basic type\"");

    Location loc = current_token->location;
    IntrinsicType t;
    Token peeked = peek_token();

    switch (current_token->type) {
      case INT_ID: {
        t = make_node(IntrinsicIntType, loc, ([ "name" : "int" ]));

        if (peeked->type == PAREN_LEFT) {
          t->range = resolve_range();
          accept_next(PAREN_RIGHT);
        }
      } break;

      case STRING_ID: {
        t = make_node(IntrinsicStringType, loc, ([
          "name": "string",
          "width" : UNDEFINED
        ]));

        if (peeked->type == PAREN_LEFT) {
          t->width = resolve_range();
          accept_next(PAREN_RIGHT);
        }

        return t;
      }

      default: TODO("Handle builtin (basic) type: %O\n", current_token);
    }

    return t;
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

      expect_next((< DOT, SEMICOLON >));

      if (peek_token()->type == DOT) {
        next_token();
      }

      s->identifiers += ({ id });
    }

    return s;
  }

  // FIXME: Separate annotations and modifiers
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

    return make_node(Modifier, current_token->location, ([
      "name": current_token->value,
      "type": current_token->type,
    ]));
  }

  protected mixed do_annotation_list() {

  }

  protected mixed do_annotation() {

  }

  protected Attribute do_attribute() {
    Location loc = current_token->location;

    if (current_token->type == ATTRIBUTE_ID) {
      accept_next(PAREN_LEFT);
      accept_next(STRING);

      Attribute a = make_node(Attribute, loc, ([
        "name": "__attribute__",
        "arg": make_node(StringConstant, current_token->location, ([
          "value": current_token->value,
        ]))
      ]));

      next_token();

      if (current_token->type == COMMA) {
        accept_next(PAREN_RIGHT);
      } else {
        expect(PAREN_RIGHT);
        next_token();
      }

      return a;
    } else if (current_token->type == DEPRECATED_ID) {
      if (peek_token()->type == PAREN_LEFT) {
        next_token();
        accept_next(PAREN_RIGHT);
      } else {
        next_token();
      }

      return make_node(Attribute, loc, ([ "name": "__deprecated__" ]));
    } else {
      TODO("Throw syntax error\n");
    }
  }
}
