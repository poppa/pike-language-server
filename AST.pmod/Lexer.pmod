#include "ast.h"

protected class BaseLexer {
  protected Stdio.File source;
  protected int line = 1;
  protected int column = 0;
  protected string current;
  protected .Token.Position position_start;

  protected void create(Stdio.File | string source) {
    if (stringp(source)) {
      source = Stdio.FakeFile(source);
    }

    this::source = source;
  }

  public .Token.Token lex();

  //! Returns the input source. If the input source was a file on disk the
  //! the path is returns, else @code{stdin@} is returned.
  public string `input_source() {
    if (object_program(source) == Stdio.FakeFile) {
      return "stdin";
    } else {
      string o = sprintf("%O", source);
      sscanf(o, "%*s\"%s\"", string filename);
      return filename ? filename : "stdin";
    }
  }

  //! Creates a one character token
  public .Token.Token make_simple_token(.Token.Type type) {
    if (!current) {
      return UNDEFINED;
    }

    .Token.Position end = .Token.Position(source->tell() + 1, line, column + 1);
    .Token.Location loc = .Token.Location(input_source, position_start, end);

    return .Token.Token(type, loc, current);
  }

  protected int `cursor() {
    return source->tell();
  }

  protected BaseLexer set_start_position() {
    position_start = .Token.Position(source->tell(), line, column);
    return this;
  }

  protected variant string advance(int n) {
    string value = source->read(n);
    column += n;

    if (value == "") {
      return current = UNDEFINED;
    }

    if (has_value(value, "\n")) {
      array lines = value / "\n";
      int len = sizeof(lines);
      line += len - 1;


      if (len > 1) {
        column = strlen(lines[-1]);
      } else {
        // FIXME: This feels wroooong
        column = strlen(value);
      }
    }

    return current = value;
  }

  protected variant string advance() {
    return advance(1);
  }

  protected BaseLexer inc_line() {
    line += 1;
    column = 0;
    return this;
  }

  protected BaseLexer eat_whitespace() {
    while ((< " ", "\t", "\v" >)[current]) {
      advance();
    }

    return this;
  }

  protected BaseLexer eat_whitespace_and_newline() {
    while ((< " ", "\t", "\v", "\n" >)[current]) {
      advance();
    }

    return this;
  }

  protected BaseLexer eat_newline() {
    while (current == "\n") {
      inc_line();
      advance();
    }

    return this;
  }

  protected string peek_source(int n) {
    ASSERT_DEBUG(n > 0, "n must be greater than 0\n");

    int pos = source->tell();
    string data = source->read(n);
    source->seek(pos, Stdio.SEEK_SET);

    return data;
  }

  protected variant string peek_source() {
    return peek_source(1);
  }

  protected string concat(int n) {
    ASSERT_DEBUG(n > 0, "n must be greater than 0\n");
    string v = current;

    if (!v || !advance(n)) {
      ASSERT_DEBUG(current, "Trying to concat beyond end\n");
      return UNDEFINED;
    }

    return current = v + current;
  }

  protected variant string concat() {
    return concat(1);
  }
}

class Lexer {
  inherit BaseLexer;

  public .Token.Token lex() {
    if (!advance()) {
      return UNDEFINED;
    }

    eat_whitespace_and_newline();

    if (!current) {
      return UNDEFINED;
    }

    TRACE("Current: %O at (%d:%d)\n", current, line, column);

    set_start_position();

    switch (current) {
      case "(": {
        string next = peek_source();

        if (next == "{") {
          return concat() && make_simple_token(.Token.ARRAY_START);
        } else if (next == "[") {
          return concat() && make_simple_token(.Token.MAPPING_START);
        } else if (next == "<") {
          return concat() && make_simple_token(.Token.MULTISET_START);
        }

        return make_simple_token(.Token.PAREN_LEFT);
      }

      case "]": {
        if (peek_source() == ")") {
          return concat() && make_simple_token(.Token.MAPPING_END);
        }

        return make_simple_token(.Token.BRACKET_RIGHT);
      }

      case ")": {
        return make_simple_token(.Token.PAREN_RIGHT);
      }

      case "}": {
        if (peek_source() == ")") {
          return concat() && make_simple_token(.Token.ARRAY_END);
        }

        return make_simple_token(.Token.CURLY_RIGHT);
      }

      case ">": {
        string next = peek_source();

        if (next == ")") {
          return concat() && make_simple_token(.Token.MULTISET_END);
        } else if (next == ">") {
          if (peek_source(2) == ">=") {
            return concat(2) && make_simple_token(.Token.RSH_EQ);
          }
          return concat() && make_simple_token(.Token.RSH);
        } else if (next == "=") {
          return concat() && make_simple_token(.Token.GE);
        }

        return make_simple_token(.Token.GREATER_THAN);
      }

      case "[": {
        return make_simple_token(.Token.BRACKET_LEFT);
      }

      case "{": {
        return make_simple_token(.Token.CURLY_LEFT);
      }

      case "<": {
        string next = peek_source();

        if (next == "<") {
          if (peek_source(2) == "<=") {
            return concat(2) && make_simple_token(.Token.LSH_EQ);
          }
          return concat() && make_simple_token(.Token.LSH);
        } else if (next == "=") {
          return concat() && make_simple_token(.Token.LE);
        }

        return make_simple_token(.Token.LESS_THAN);
      }

      case "!": {
        if (peek_source() == "=") {
          return concat() && make_simple_token(.Token.NE);
        }
        return make_simple_token(.Token.NOT);
      }

      case "=": {
        if (peek_source() == "=") {
          return concat() && make_simple_token(.Token.EQ);
        }
        return make_simple_token(.Token.ASSIGN);
      }

      case "-": {
        string next = peek_source();

        if (next == "-") {
          return concat() && make_simple_token(.Token.DEC);
        } else if (next == "=") {
          return concat() && make_simple_token(.Token.SUB_EQ);
        } else if (next == ">") {
          return concat() && make_simple_token(.Token.ARROW);
        }

        return make_simple_token(.Token.MINUS);
      }

      case "+":  {
        string next = peek_source();

        if (next == "+") {
          return concat() && make_simple_token(.Token.INC);
        } else if (next == "=") {
          return concat() && make_simple_token(.Token.ADD_EQ);
        }

        return make_simple_token(.Token.PLUS);
      }

      case "&": {
        string next = peek_source();

        if (next == "&") {
          return concat() && make_simple_token(.Token.LAND);
        } else if (next == "=") {
          return concat() && make_simple_token(.Token.AND_EQ);
        }

        return make_simple_token(.Token.AMP);
      }

      case "|": {
        string next = peek_source();

        if (next == "|") {
          return concat() && make_simple_token(.Token.LOR);
        } else if (next == "=") {
          return concat() && make_simple_token(.Token.OR_EQ);
        }

        return make_simple_token(.Token.PIPE);
      }

      case ".": {
        if (peek_source() == ".") {
          if (peek_source(2) == "..") {
            return concat(2) && make_simple_token(.Token.DOT_DOT_DOT);
          }
          return concat() && make_simple_token(.Token.DOT_DOT);
        }
        return make_simple_token(.Token.DOT);
      }

      case ":": {
        if (peek_source() == ":") {
          return concat() && make_simple_token(.Token.COLON_COLON);
        }
        return make_simple_token(.Token.COLON);
      }

      case "^": {
        if (peek_source() == "=") {
          return concat() && make_simple_token(.Token.XOR_EQ);
        }
        return make_simple_token(.Token.XOR);
      }

      case "*": {
        string next = peek_source();

        if (next == "*") {
          if (peek_source(2) == "*=") {
            return concat(2) && make_simple_token(.Token.POW_EQ);
          }
          return concat() && make_simple_token(.Token.POW)
        } else if (next == "=") {
          return concat() && make_simple_token(.Token.MULT_EQ);
        }
        return make_simple_token(.Token.MULT);
      }

      default: {
        error(
          "Unknown token %O at \"%s@%d:%d\"\n",
          current, input_source, position_start->line, position_start->column
        );
      }
    }
  }
}
