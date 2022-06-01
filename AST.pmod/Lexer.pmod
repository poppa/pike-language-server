#charset utf-8
#pike __REAL_VERSION__
#require constant(Regexp.PCRE.Widestring)

#include "ast.h"

#define REGEX Regexp.PCRE.Widestring
#define RegexpOption Regexp.PCRE.OPTION

protected string float_p = "(0?\\.|[1-9][0-9]*\\.)[0-9]+";
protected string int_p = "(0|[1-9][0-9]*)";
protected string num_p = "(" + int_p + "|" + float_p + ")";
protected string exp_p = "(" + num_p + "e[-+]?" + num_p + "+)";
protected string hex_p = "0x[0-9a-f]+";
protected string bin_p = "0b[0-1]+";

protected mapping(string:REGEX) re_cache = ([]);

protected bool is_float(string value) {
  REGEX re = re_cache->float || (re_cache->float = REGEX(float_p));
  return re->match(value);
}

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

  //! Creates a token with @[current] as value, @[position_start] as start
  //! location and the current position as end location.
  public .Token.Token make_simple_token(.Token.Type type) {
    if (!current) {
      return UNDEFINED;
    }

    .Token.Position end = .Token.Position(at_byte + 1, line, column + 1);
    .Token.Location loc = .Token.Location(input_source, position_start, end);

    return .Token.Token(type, loc, current);
  }

  protected inline int `cursor() {
    return source->tell();
  }

  protected int `at_byte() {
    int p = source->tell();
    // If p == 0 no byte has been read yet
    return p > 0 ? p - 1 : p;
  }

  protected void set_start_position() {
    position_start = .Token.Position(at_byte, line, column);
  }

  protected string consume(int n) {
    string value = source->read(n);
    column += sizeof(value);

    if (value == "") {
      return UNDEFINED;
    }

    if (has_value(value, "\n")) {
      array lines = value / "\n";
      int len = sizeof(lines);
      line += len - 1;


      if (len > 1) {
        column = strlen(lines[-1]);
      } else {
        TODO("// FIXME: This feels wroooong\n");
        // FIXME: This feels wroooong
        column = strlen(value);
      }
    }

    return value;
  }

  protected variant string consume() {
    return consume(1);
  }

  protected string advance(int n) {
    return current = consume(n);
  }

  protected variant string advance() {
    return current = consume(1);
  }

  protected string read_non_ws() {
    string value = source->read(1);

    if (value == "") {
      return UNDEFINED;
    }

    if ((<"\n", "\t", "\v", " ">)[value]) {
      source->seek(-1, Stdio.SEEK_CUR);
      return UNDEFINED;
    }

    column += 1;

    return value;
  }

  protected void simple_put_back() {
    column -= 1;
    source->seek(-1, Stdio.SEEK_CUR);
  }

  protected void inc_line() {
    line += 1;
    column = 0;
  }

  protected void eat_whitespace() {
    while ((< " ", "\t", "\v" >)[current]) {
      advance();
    }
  }

  protected void eat_whitespace_and_newline() {
    while ((< " ", "\t", "\v", "\n" >)[current]) {
      advance();
    }
  }

  protected void eat_newline() {
    while (current == "\n") {
      inc_line();
      advance();
    }
  }

  protected string peek_source(int n) {
    ASSERT_DEBUG(n > 0, "n must be greater than 0\n");

    int pos = source->tell();
    string data = source->read(n);
    source->seek(pos, Stdio.SEEK_SET);

    if (data == "") {
      return UNDEFINED;
    }

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

  protected bool read_word(string word) {
    int len = sizeof(word);
    int pos = source->tell();
    int col_in = column;
    simple_put_back();

    String.Buffer buf = String.Buffer();
    function add = buf->add;

    loop: while (string s = consume()) {
      int c = s[0];
      switch (c) {
        case '_':
        case '0'..'9':
        // FIXME: Add support for ISO-8859-* and what not
        case 'a'..'z':
        case 'A'..'Z':
          add(s);
          break;

        default:
          break loop;
      }
    }

    string w = buf->get();

    if (w != word) {
      column = col_in;
      source->seek(pos, Stdio.SEEK_SET);
      return false;
    }

    current = w;

    return true;
  }

  protected array low_read_word() {
    string old_current = current;
    int old_column = column;
    int old_pos = source->tell();
    simple_put_back();

    String.Buffer buf = String.Buffer();
    function add = buf->add;

    loop: while (string s = read_non_ws()) {
      int c = s[0];
      switch (c) {
        case '_':
        // FIXME: Add support for ISO-8859-* and what not
        case 'a'..'z':
        case 'A'..'Z':
        case '0'..'9':
          add(s);
          break;

        default:
          simple_put_back();
          break loop;
      }
    }

    string w = buf->get();

    if (sizeof(w) > 0) {
      current = w;
    }

    return ({ w, lambda () {
      current = old_current;
      column = old_column;
      source->seek(old_pos, Stdio.SEEK_SET);
    }});
  }

  protected string read_line() {
    int pos = source->tell();
    String.Buffer buf = String.Buffer();
    function add = buf->add;

    while (string s = source->read(1)) {
      if (s == "") {
        break;
      } else if (s == "\n") {
        source->seek(-1, Stdio.SEEK_CUR);
        break;
      }

      add(s);
    }

    string w = buf->get();
    column += sizeof(w);

    return w;
  }

  protected string read_number() {
    String.Buffer buf = String.Buffer();
    function add = buf->add;

    int cc = current[0];

    if (!(cc >= '0' && cc <= '9')) {
      if (cc != '-' && cc != '.') {
        error("Expected '-' or '.' but got %O\n", current);
      }
      add(current);
    } else {
      // Put back the current digit, which should be the first in the number,
      // and let the loop below add it
      simple_put_back();
    }

    enum NumState {
      NUM_NONE,
      NUM_INT,
      NUM_FLOAT,
      NUM_EXP,
      NUM_HEX,
      NUM_BIN,
    };

    NumState state = current == "." ? NUM_FLOAT : NUM_NONE;

    #define exit_loop() do {                              \
      column -= 1;                                        \
      source->seek(-1, Stdio.SEEK_CUR);                   \
      break loop;                                         \
    } while (0)

    #define read_digits() do {                            \
      while (string ss = read_non_ws()) {                 \
        switch (ss[0]) {                                  \
          case '0'..'9': add(ss); break;                  \
          default: exit_loop();                           \
        }                                                 \
      }                                                   \
    } while (0)

    #define read_hex_digits() do {                        \
      while (string ss = read_non_ws()) {                 \
        switch (ss[0]) {                                  \
          case '0'..'9': add(ss); break;                  \
          case 'a'..'f': case 'A'..'F': add(ss); break;   \
          default: exit_loop();                           \
        }                                                 \
      }                                                   \
    } while (0)

    #define read_binary_digits() do {                     \
      while (string ss = read_non_ws()) {                 \
        switch (ss[0]) {                                  \
          case '0'..'1': add(ss); break;                  \
          default: exit_loop();                           \
        }                                                 \
      }                                                   \
    } while (0)

    loop: while (string s = read_non_ws()) {
      int c = s[0];

      if (c == '.') {
        if (state != NUM_INT && state != NUM_NONE) {
          error("Malformed number\n");
        }

        state = NUM_FLOAT;
        add(s);
      } else if (c == '0' && state == NUM_NONE) {
        string next = peek_source();

        if ((< "x", "X" >)[next]) {
          state = NUM_HEX;
          add(s);
          add(read_non_ws());
          read_hex_digits();
        } else if ((< "b", "B" >)[next]) {
          state = NUM_BIN;
          add(s);
          add(read_non_ws());
          read_binary_digits();
        } else if (next && next != ".") {
          error("An integer can not start with a 0\n");
        } else {
          state = NUM_INT;
          add(s);
        }
      } else if ((< "e", "E" >)[s]) {
        state = NUM_EXP;
        add(s);
        string next = peek_source();
        if (next == "-" || next == "+") {
          add(consume());
        }
        read_digits();
      } else if (c >= '0' && c <= '9') {
        if (state == NUM_NONE) {
          state = NUM_INT;
        }

        add(s);
      } else {
        exit_loop();
      }
    }

    #undef read_binary_digits
    #undef read_hex_digits
    #undef read_digits
    #undef exit_loop

    return buf->get();
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
        } else if (next == "?") {
          return concat() && make_simple_token(.Token.SAFE_APPLY);
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
        if (peek_source() == "?") {
          return concat() && make_simple_token(.Token.SAFE_START_INDEX);
        }
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
          if (peek_source(2) == ">?") {
            return concat(2) && make_simple_token(.Token.SAFE_INDEX);
          }
          return concat() && make_simple_token(.Token.ARROW);
        }

        int next_c = next && next[0];
        if (next_c) {
          if ((next_c >= '0' && next_c <= '9') || next_c == '.') {
            return lex_number();
          }
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
        string next = peek_source();
        if (next == ".") {
          if (peek_source(2) == "..") {
            return concat(2) && make_simple_token(.Token.DOT_DOT_DOT);
          }
          return concat() && make_simple_token(.Token.DOT_DOT);
        } else if (next && next[0] >= '0' && next[0] <= '9') {
          return lex_number();
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
          return concat() && make_simple_token(.Token.POW);
        } else if (next == "=") {
          return concat() && make_simple_token(.Token.MULT_EQ);
        }
        return make_simple_token(.Token.MULT);
      }

      case "/": {
        string next = peek_source();

        if (next == "=") {
          return concat() && make_simple_token(.Token.DIV_EQ);
        } else if (next == "/") {
          return lex_line_comment();
        } else if (next == "*") {
          return lex_block_comment();
        }
        return make_simple_token(.Token.DIV);
      }

      case "?": {
        if (peek_source() == "=") {
          return concat() && make_simple_token(.Token.ATOMIC_GET_SET);
        }
        return make_simple_token(.Token.QUESTION);
      }

      case ",": {
        return make_simple_token(.Token.COMMA);
      }

      case ";": {
        return make_simple_token(.Token.SEMICOLON);
      }

      case "~": {
        return make_simple_token(.Token.TILDE);
      }

      case "@": {
        return make_simple_token(.Token.AT);
      }

      case "\"": {
        return lex_string();
      }

      case "'": {
        return lex_character_literal();
      }

      case "_": {
        if (peek_source() == "_") {
          if (.Token.Token t = lex_magic_dunderscore()) {
            return t;
          }
        } else if (read_word("_Static_assert")) {
          return make_simple_token(.Token.STATIC_ASSERT);
        }

        return lex_symbol_name();
      }

      case "%": {
        if (peek_source() == "=") {
          return concat() && make_simple_token(.Token.MOD_EQ);
        }
        return make_simple_token(.Token.MOD);
      }

      case "`": {
        TODO("Handle backtick `");
      }

      default: {
        int char = current[0];

        // FIXME: Support wider charset
        if (char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z') {
          if (.Token.Token t = lex_identifier()) {
            return t;
          }

          return lex_symbol_name();
        }

        if (char >= '0' && char <= '9') {
          if (.Token.Token t = lex_number()) {
            return t;
          }
        }

        error(
          "Unknown token %O at \"%s@%d:%d\"\n",
          current, input_source, position_start->line, position_start->column
        );
      }
    }
  }

  private .Token.Token lex_magic_dunderscore() {
    [string word, function reset] = low_read_word();

    switch (word) {
      case "__attribute__": return make_simple_token(.Token.ATTRIBUTE_ID);
      case "__deprecated__": return make_simple_token(.Token.DEPRECATED_ID);
      case "__func__": return make_simple_token(.Token.FUNCTION_NAME);
      case "__unknown__": return make_simple_token(.Token.UNKNOWN);
      case "__unused__": return make_simple_token(.Token.UNUSED);
      case "__weak__": return make_simple_token(.Token.WEAK);
    }

    if (has_suffix(word, "__")) {
      error(
        "Symbols with leading and tailing double underscores are reserved\n"
      );
    }

    reset();

    return UNDEFINED;
  }

  private .Token.Token lex_identifier() {
    [string word, function reset] = low_read_word();

    switch (word) {
      case "array": return make_simple_token(.Token.ARRAY_ID);
      case "auto": return make_simple_token(.Token.AUTO_ID);
      case "bits": return make_simple_token(.Token.BITS);
      case "break": return make_simple_token(.Token.BREAK);
      case "case": return make_simple_token(.Token.CASE);
      case "catch": return make_simple_token(.Token.CATCH);
      case "class": return make_simple_token(.Token.CLASS);
      case "constant": return make_simple_token(.Token.CONSTANT);
      case "continue": return make_simple_token(.Token.CONTINUE);
      case "default": return make_simple_token(.Token.DEFAULT);
      case "do": return make_simple_token(.Token.DO);
      case "else": return make_simple_token(.Token.ELSE);
      case "enum": return make_simple_token(.Token.ENUM);
      case "extern": return make_simple_token(.Token.EXTERN);
      case "final": return make_simple_token(.Token.FINAL_ID);
      case "float": return make_simple_token(.Token.FLOAT_ID);
      case "for": return make_simple_token(.Token.FOR);
      case "foreach": return make_simple_token(.Token.FOREACH);
      case "function": return make_simple_token(.Token.FUNCTION_ID);
      case "gauge": return make_simple_token(.Token.GAUGE);
      case "global": return make_simple_token(.Token.GLOBAL);
      case "if": return make_simple_token(.Token.IF);
      case "import": return make_simple_token(.Token.IMPORT);
      case "inherit": return make_simple_token(.Token.INHERIT);
      case "inline": return make_simple_token(.Token.INLINE);
      case "int": return make_simple_token(.Token.INT_ID);
      case "lambda": return make_simple_token(.Token.LAMBDA);
      case "local": return make_simple_token(.Token.LOCAL_ID);
      case "mixed": return make_simple_token(.Token.MIXED_ID);
      case "multiset": return make_simple_token(.Token.MULTISET_ID);
      case "object": return make_simple_token(.Token.OBJECT_ID);
      case "optional": return make_simple_token(.Token.OPTIONAL);
      case "predef": return make_simple_token(.Token.PREDEF);
      case "private": return make_simple_token(.Token.PRIVATE);
      case "program": return make_simple_token(.Token.PROGRAM_ID);
      case "protected": return make_simple_token(.Token.PROTECTED);
      case "public": return make_simple_token(.Token.PUBLIC);
      case "return": return make_simple_token(.Token.RETURN);
      case "sscanf": return make_simple_token(.Token.SSCANF);
      case "static": return make_simple_token(.Token.STATIC);
      case "string": return make_simple_token(.Token.STRING_ID);
      case "switch": return make_simple_token(.Token.SWITCH);
      case "typedef": return make_simple_token(.Token.TYPEDEF);
      case "typeof": return make_simple_token(.Token.TYPEOF);
      case "variant": return make_simple_token(.Token.VARIANT);
      case "void": return make_simple_token(.Token.VOID_ID);
      case "while": return make_simple_token(.Token.WHILE);
    }

    reset();

    return UNDEFINED;
  }

  private .Token.Token lex_line_comment() {
    string line = read_line();

    if (has_prefix(line, "/!")) {
      current = line[2..];
      return make_simple_token(.Token.DOC_COMMENT);
    } else if (has_prefix(line, "/", )) {
      current = line[1..];
      return make_simple_token(.Token.COMMENT);
    }

    TODO("Return token\n");
  }

  private .Token.Token lex_block_comment() {
    int pos = source->tell();

    String.Buffer buf = String.Buffer();
    function add = buf->add;

    while (string s = source->read(1)) {
      if (s == "") {
        error("Unterminated block comment\n");
      }

      if (s == "*" && peek_source() == "/") {
        column += 2;
        source->read(1);
        break;
      }

      if (s == "\n") {
        inc_line();
      } else {
        column += 1;
      }

      add(s);
    }

    string v = buf->get();
    current = v[1..];

    return make_simple_token(.Token.BLOCK_COMMENT);
  }

  // FIXME: Escape sequences
  private .Token.Token lex_string() {
    int pos = cursor;
    String.Buffer buf = String.Buffer();
    function add = buf->add;
    string prev = "";

    while (string s = source->read(1)) {
      column += 1;
      if (s == "\n") {
        TODO("Handle Newline in string\n");
      } else if (s == "" ) {
        error("Unterminated string literal\n");
      }

      if (s == "\"" && prev != "\\") {
        break;
      }

      add(s);
      prev = s;
    }

    current = buf->get();

    return make_simple_token(.Token.STRING);
  }

  // FIXME: Escape sequences
  private .Token.Token lex_character_literal() {
    advance();

    if (peek_source() != "'") {
      error("Unterminated character literal\n");
    }

    string v = current;
    advance();
    current = v;

    return make_simple_token(.Token.CHAR);
  }

  private .Token.Token lex_number() {
    current = read_number();
    .Token.Type t = is_float(current) ? .Token.FLOAT : .Token.NUMBER;
    return make_simple_token(t);
  }

  private .Token.Token lex_symbol_name() {
    [string word, function reset] = low_read_word();

    if (!word || sizeof(word) < 1) {
      error("Unresolved symbol\n");
    }

    current = word;

    return make_simple_token(.Token.SYMBOL_NAME);
  }
}
