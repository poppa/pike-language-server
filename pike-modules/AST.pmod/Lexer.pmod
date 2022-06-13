#charset utf-8
#pike __REAL_VERSION__
#require constant(Regexp.PCRE.Widestring)

#include "ast.h"

#define REGEX Regexp.PCRE.Widestring
#define RegexpOption Regexp.PCRE.OPTION

// NOTE: Many of these regexp aren't used yet, but will serve a purpose when we
//       get to the point that we want do differentiate different number types.
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

#ifdef AST_CALL_COUNT
  private mapping(string:int) _call_count = ([]);
  public mapping(string:int) get_call_count() {
    return _call_count;
  }
  #define ADD_CALL_COUNT() {                                                \
    string cls = sprintf("%O", object_program(this));                       \
    string key = cls + "->" __func__ "()";                                  \
    _call_count[key] += 1;                                                  \
  }
#else
  #define ADD_CALL_COUNT() 0
#endif // AST_CALL_COUNT

public enum State {
  LEX_STATE_DEFAULT,
  LEX_STATE_PREPROC_UNQUOTED,
  LEX_STATE_PREPROC_DEFINE,
}

#define IS_STATE_DEFAULT (lex_state == LEX_STATE_DEFAULT)
#define IS_STATE_UNQUOTED (lex_state == LEX_STATE_PREPROC_UNQUOTED)
#define IS_STATE_DEFINE (lex_state == LEX_STATE_PREPROC_DEFINE)

#define SYNTAX_ERROR(ARGS...) {                                           \
  string msg = sprintf(ARGS);                                             \
  if (!has_suffix(msg, "\n")) {                                           \
    msg += "\n";                                                          \
  }                                                                       \
  msg += position_info_message;                                           \
  error(msg);                                                             \
}

public multiset(int) whitepsaces = (<
  0x9, 0xa, 0xb, 0xc, 0xd, 0x20, 0x85, 0xa0, 0x1680, 0x2000,
  0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008,
  0x2009, 0x200a, 0x202f, 0x205f, 0x3000, 0xfeff
>);

#define is_whitespace(CHAR) whitepsaces[(CHAR)]
#define is_digit(CHAR) ((CHAR) >= '0' && (CHAR) <= '9')
// FIXME: Support wider charset
#define is_alpha(CH) ((CH) >= 'a' && (CH) <= 'z' || (CH) >= 'A' && (CH) <= 'Z')

protected class BaseLexer {
  protected string source;
  protected int length;
  protected int line = 1;
  protected int column = 0;
  protected int cursor = 0;
  protected string current_string;
  protected int char;
  protected string _filename;
  protected .Token.Position position_start;
  protected State lex_state = LEX_STATE_DEFAULT;

  protected void create(string source, string filename) {
    if (has_prefix(source, "\357\273\277")) {
      TRACE("Convert input to internal string\n");
      source = utf8_to_string(source);
    }

    this::length = sizeof(source);
    // Add sentinels so we can't peek beyond the end
    this::source = source + "\0\0\0";
    this::_filename = filename;
  }

  protected variant void create(string source) {
    create(source, "stdin");
  }

  protected variant void create(Stdio.File source, string filename) {
    create(source->read(), filename);
  }

  protected variant void create(Stdio.File source) {
    string o = sprintf("%O", source);
    sscanf(o, "%*s\"%s\"", string filename);
    create(source, filename);
  }

  public .Token.Token lex();

  public string `position_info_message() {
    string msg = "";
    if (position_start->byte != cursor) {
      msg += sprintf(
        "    at byte range %d..%d, column %d..%d",
        position_start->byte, cursor, position_start->column, column + 1
      );

      if (position_start->line != line) {
        msg += sprintf(" on line %d..%d", position_start->line, line);
      } else {
        msg += sprintf(" on line %d", line);
      }

      msg += "\n";
    } else {
      msg += sprintf(
        "    at byte %d, column %d on line %d\n",
        cursor, column, line
      );
    }

    return msg;
  }

  //! Returns the input filename. If the input source was a file on disk the
  //! the path is returns, else @code{stdin@} is returned.
  public string `filename() {
    return _filename;
  }

  //! Creates a token with @[current_string] as value, @[position_start] as
  //! start location and the current position as end location.
  protected .Token.Token make_simple_token(.Token.Type type) {
    ADD_CALL_COUNT();

    if (!current_string && char) {
      current_string = sprintf("%c", char);
    }

    .Token.Position end = .Token.Position(at_byte + 1, line, column + 1);
    .Token.Location loc = .Token.Location(_filename, position_start, end);

    return .Token.Token(type, loc, current_string, lex_state);
  }

  protected variant .Token.Token make_simple_token(
    string value,
    .Token.Type type
  ) {
    current_string = value;
    return make_simple_token(type);
  }

  protected int `at_byte() {
    int p = cursor;
    // If p == 0 no byte has been read yet
    return p > 0 ? p - 1 : p;
  }

  protected void set_start_position() {
    position_start = .Token.Position(at_byte, line, column);
  }

  protected void maybe_reset_lex_state() {
    if (lex_state == LEX_STATE_PREPROC_UNQUOTED) {
      TRACE("### reset lex state from unquoted to default\n");
      lex_state = LEX_STATE_DEFAULT;
    } else if (lex_state == LEX_STATE_PREPROC_DEFINE) {
      int prev = look_behind(2);

      if (prev != '\\') {
        TRACE("### reset lex state from define to default\n");
        lex_state = LEX_STATE_DEFAULT;
      }
    }
  }

  protected int consume() {
    ADD_CALL_COUNT();

    if (cursor >= length) {
      return UNDEFINED;
      error("Trying to read beyond input end\n");
    }

    int value = source[cursor];
    column += 1;
    cursor += 1;

    if (value == '\0') {
      return UNDEFINED;
    }

    if (value == '\n') {
      maybe_reset_lex_state();
      inc_line();
    }

    return value;
  }

  protected int advance() {
    return char = consume();
  }

  protected int read_non_ws() {
    ADD_CALL_COUNT();
    int prev_col = column;
    int prev_line = line;
    int value = consume();

    if (is_whitespace(value)) {
      cursor -= 1;
      column = prev_col;
      line = prev_line;
      return UNDEFINED;
    }

    return value;
  }

  protected void simple_put_back() {
    ADD_CALL_COUNT();
    column -= 1;
    cursor -= 1;
  }

  protected void inc_line() {
    ADD_CALL_COUNT();
    line += 1;
    column = 0;
  }

  protected void eat_whitespace_and_newline() {
    ADD_CALL_COUNT();

    while (is_whitespace(char)) {
      advance();
    }
  }

  protected int look_behind(int n) {
    ADD_CALL_COUNT();
    return source[cursor - n];
  }

  protected int peek_source() {
    ADD_CALL_COUNT();

    // May look peculiar, but we move ahead one pos in consume()
    int data = source[cursor];

    if (data == '\0') {
      return UNDEFINED;
    }

    return data;
  }

  protected bool read_word(string word) {
    ADD_CALL_COUNT();
    int len = sizeof(word);
    int pos = cursor;
    int col_in = column;
    simple_put_back();

    String.Buffer buf = String.Buffer();
    function add = buf->putchar;

    loop: while (int c = consume()) {
      switch (c) {
        case '_':
        case '0'..'9':
        // FIXME: Add support for ISO-8859-* and what not
        case 'a'..'z':
        case 'A'..'Z':
          add(c);
          break;

        default:
          break loop;
      }
    }

    string w = buf->get();

    if (w != word) {
      column = col_in;
      cursor = pos;
      return false;
    }

    current_string = w;

    return true;
  }

  protected array low_read_word() {
    ADD_CALL_COUNT();
    string old_current = current_string;
    int old_column = column;
    int old_pos = cursor;
    simple_put_back();

    String.Buffer buf = String.Buffer();
    function add = buf->putchar;

    loop: while (int c = read_non_ws()) {
      switch (c) {
        case '_':
        // FIXME: Add support for ISO-8859-* and what not
        case 'a'..'z':
        case 'A'..'Z':
        case '0'..'9':
          add(c);
          break;

        default:
          simple_put_back();
          break loop;
      }
    }

    string w = buf->get();

    if (sizeof(w) > 0) {
      current_string = w;
    }

    return ({ w, lambda () {
      current_string = old_current;
      column = old_column;
      cursor = old_pos;
    }});
  }

  protected string read_line() {
    ADD_CALL_COUNT();
    int pos = cursor;
    String.Buffer buf = String.Buffer();
    function add = buf->putchar;

    while (int c = consume()) {
      if (c == '\n') {
        simple_put_back();
        line -= 1;
        break;
      }

      add(c);
    }

    return buf->get();
  }

  protected string read_number() {
    ADD_CALL_COUNT();
    String.Buffer buf = String.Buffer();
    function add = buf->putchar;

    int cc = char;

    if (!is_digit(cc)) {
      if (!(< '-', '.' >)[cc]) {
        SYNTAX_ERROR("Expected '-' or '.' but got '%c'\n", cc);
      }
      add(char);
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
      NUM_OCTAL,
    };

    NumState state = char == '.' ? NUM_FLOAT : NUM_NONE;

    #define exit_loop() do {                              \
      column -= 1;                                        \
      cursor -= 1;                                        \
      break loop;                                         \
    } while (0)

    #define read_digits() do {                            \
      while (int c = read_non_ws()) {                     \
        switch (c) {                                      \
          case '0'..'9': add(c); break;                   \
          default: exit_loop();                           \
        }                                                 \
      }                                                   \
    } while (0)

    #define read_hex_digits() do {                        \
      while (int c = read_non_ws()) {                     \
        switch (c) {                                      \
          case '0'..'9': add(c); break;                   \
          case 'a'..'f': case 'A'..'F': add(c); break;    \
          default: exit_loop();                           \
        }                                                 \
      }                                                   \
    } while (0)

    #define read_binary_digits() do {                     \
      while (int c = read_non_ws()) {                     \
        switch (c) {                                      \
          case '0'..'1': add(c); break;                   \
          default: exit_loop();                           \
        }                                                 \
      }                                                   \
    } while (0)

    loop: while (int c = read_non_ws()) {
      if (c == '.') {
        // Range, e.g [1..10]
        if (peek_source() == '.') {
          exit_loop();
        }

        if (state != NUM_INT && state != NUM_NONE) {
          SYNTAX_ERROR("Malformed number\n");
        }

        state = NUM_FLOAT;
        add(c);
      } else if (c == '0' && state == NUM_NONE) {
        int next = peek_source();

        if ((< 'x', 'X' >)[next]) {
          state = NUM_HEX;
          add(c);
          add(read_non_ws());
          read_hex_digits();
        } else if ((< 'b', 'B' >)[next]) {
          state = NUM_BIN;
          add(c);
          add(read_non_ws());
          read_binary_digits();
        } else if (
          next && !(<
            '.', ')', ';', ',', ':',
            '\\', ']', '>', '}', ' ', '\t', '\n'
          >)[next]
        ) {
          // Is this actually octal, e.g. 0755 as access mode?
          state = NUM_OCTAL;
          add(c);
          read_digits();
        } else {
          state = NUM_INT;
          add(c);
        }
      } else if ((< 'e', 'E' >)[c]) {
        state = NUM_EXP;
        add(c);
        int next = peek_source();
        if (next == '-' || next == '+') {
          add(consume());
        }
        read_digits();
      } else if (c >= '0' && c <= '9') {
        if (state == NUM_NONE) {
          state = NUM_INT;
        }

        add(c);
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

  protected bool gobble(int c) {
    ADD_CALL_COUNT();
    int next = peek_source();

    if (next == c) {
      advance();
      return true;
    }

    return false;
  }
}

class Lexer {
  inherit BaseLexer;

  public .Token.Token lex() {
    current_string = UNDEFINED;

    if (!advance()) {
      return UNDEFINED;
    }

    eat_whitespace_and_newline();

    if (!char || char == '\0') {
      return UNDEFINED;
    }

    TRACE("Current: \"%c\" at (%d:%d)\n", char, line, column);

    set_start_position();

    if (IS_STATE_UNQUOTED) {
      return lex_unquoted(.Token.MACRO_LITERAL);
    }

    switch (char) {
      case '\\': {
        TRACE("Current is backslash (\\)\n");
        if (lex_state != LEX_STATE_PREPROC_DEFINE) {
          SYNTAX_ERROR("Illegal character '%c'\n", char);
        }

        return make_simple_token(.Token.CONT_LINE);
      }

      case '(': {
        if (gobble('{')) {
          return make_simple_token("({", .Token.ARRAY_START);
        } else if (gobble('[')) {
          return make_simple_token("([", .Token.MAPPING_START);
        } else if (gobble('<')) {
          return make_simple_token("(<", .Token.MULTISET_START);
        } else if (gobble('?')) {
          return make_simple_token("(?", .Token.SAFE_APPLY);
        }
        return make_simple_token(.Token.PAREN_LEFT);
      }

      case ']': {
        if (gobble(')')) {
          return make_simple_token("])", .Token.MAPPING_END);
        }
        return make_simple_token(.Token.BRACKET_RIGHT);
      }

      case ')': {
        return make_simple_token(.Token.PAREN_RIGHT);
      }

      case '}': {
        if (gobble(')')) {
          return make_simple_token("})", .Token.ARRAY_END);
        }

        return make_simple_token(.Token.CURLY_RIGHT);
      }

      case '>': {
        if (gobble(')')) {
          return make_simple_token(">)", .Token.MULTISET_END);
        } else if (gobble('>')) {
          if (gobble('=')) {
            return make_simple_token(">>=", .Token.RSH_EQ);
          }
          return make_simple_token(">>", .Token.RSH);
        } else if (gobble('=')) {
          return make_simple_token(">=", .Token.GE);
        }

        return make_simple_token(.Token.GREATER_THAN);
      }

      case '[': {
        if (gobble('?')) {
          return make_simple_token("[?", .Token.SAFE_START_INDEX);
        }
        return make_simple_token(.Token.BRACKET_LEFT);
      }

      case '{': {
        return make_simple_token(.Token.CURLY_LEFT);
      }

      case '<': {
        if (gobble('<')) {
          if (gobble('=')) {
            return make_simple_token("<<=", .Token.LSH_EQ);
          }
          return make_simple_token("<<", .Token.LSH);
        } else if (gobble('=')) {
          return make_simple_token("<=", .Token.LE);
        }

        return make_simple_token(.Token.LESS_THAN);
      }

      case '!': {
        if (gobble('=')) {
          return make_simple_token("!=", .Token.NE);
        }
        return make_simple_token(.Token.NOT);
      }

      case '=': {
        if (gobble('=')) {
          return make_simple_token("==", .Token.EQ);
        }
        return make_simple_token(.Token.ASSIGN);
      }

      case '-': {
        if (gobble('-')) {
          return make_simple_token("--", .Token.DEC);
        } else if (gobble('=')) {
          return make_simple_token("-=", .Token.SUB_EQ);
        } else if (gobble('>')) {
          if (gobble('?')) {
            return make_simple_token("->?", .Token.SAFE_INDEX);
          }
          return make_simple_token("->", .Token.ARROW);
        }

        int next_c = peek_source();

        if (is_digit(next_c) || next_c == '.') {
          return lex_number();
        }

        return make_simple_token(.Token.MINUS);
      }

      case '+':  {
        if (gobble('+')) {
          return make_simple_token("++", .Token.INC);
        } else if (gobble('=')) {
          return make_simple_token("+=", .Token.ADD_EQ);
        }

        return make_simple_token(.Token.PLUS);
      }

      case '&': {
        if (gobble('&')) {
          return make_simple_token("&&", .Token.LAND);
        } else if (gobble('=')) {
          return make_simple_token("&=", .Token.AND_EQ);
        }
        return make_simple_token(.Token.AMP);
      }

      case '|': {
        if (gobble('|')) {
          return make_simple_token("||", .Token.LOR);
        } else if (gobble('=')) {
          return make_simple_token("|=", .Token.OR_EQ);
        }
        return make_simple_token(.Token.PIPE);
      }

      case '.': {
        if (gobble('.')) {
          if (gobble('.')) {
            return make_simple_token("...", .Token.DOT_DOT_DOT);
          }
          return make_simple_token("..", .Token.DOT_DOT);
        } else {
          int next = peek_source();
          if (next && next >= '0' && next <= '9') {
            return lex_number();
          }
        }
        return make_simple_token(.Token.DOT);
      }

      case ':': {
        if (gobble(':')) {
          return make_simple_token("::", .Token.COLON_COLON);
        }
        return make_simple_token(.Token.COLON);
      }

      case '^': {
        if (gobble('=')) {
          return make_simple_token("^=", .Token.XOR_EQ);
        }
        return make_simple_token(.Token.XOR);
      }

      case '*': {
        if (gobble('*')) {
          if (gobble('=')) {
            return make_simple_token("**=", .Token.POW_EQ);
          }
          return make_simple_token("**", .Token.POW);
        } else if (gobble('=')) {
          return make_simple_token("*=", .Token.MULT_EQ);
        }
        return make_simple_token(.Token.MULT);
      }

      case '/': {
        if (gobble('=')) {
          return make_simple_token("/=", .Token.DIV_EQ);
        } else if (gobble('/')) {
          return lex_line_comment();
        } else if (gobble('*')) {
          return lex_block_comment();
        }
        return make_simple_token(.Token.DIV);
      }

      case '?': {
        if (gobble('=')) {
          return make_simple_token("?=", .Token.ATOMIC_GET_SET);
        }
        return make_simple_token(.Token.QUESTION);
      }

      case ',': {
        return make_simple_token(.Token.COMMA);
      }

      case ';': {
        return make_simple_token(.Token.SEMICOLON);
      }

      case '~': {
        return make_simple_token(.Token.TILDE);
      }

      case '@': {
        return make_simple_token(.Token.AT);
      }

      case '"': {
        return lex_string();
      }

      case '\'': {
        return lex_character_literal();
      }

      case '_': {
        if (peek_source() == '_') {
          if (.Token.Token t = lex_magic_dunderscore()) {
            return t;
          }
        } else if (read_word("_Static_assert")) {
          return make_simple_token(.Token.STATIC_ASSERT);
        }

        return lex_symbol_name();
      }

      case '%': {
        if (gobble('=')) {
          return make_simple_token("%=", .Token.MOD_EQ);
        }
        return make_simple_token(.Token.MOD);
      }

      case '`': {
        return lex_backtick();
      }

      case '#': {
        int next = peek_source();
        // FIXME: There are more valid multiline string delimiters than " I
        //        think. Verify that stuff.
        if (next == '"') {
          return lex_multiline_string();
        }

        return lex_preprocessor_directive();
      }

      default: {
        if (is_alpha(char)) {
          if (.Token.Token t = lex_identifier()) {
            return t;
          }

          return lex_symbol_name();
        }

        if (is_digit(char)) {
          if (.Token.Token t = lex_number()) {
            return t;
          }
        }

        SYNTAX_ERROR("Unknown token '%c'", char);
      }
    }
  }

  private .Token.Token lex_magic_dunderscore() {
    [string word, function reset] = low_read_word();
    current_string = word;

    switch (word) {
      case "__attribute__": return make_simple_token(.Token.ATTRIBUTE_ID);
      case "__deprecated__": return make_simple_token(.Token.DEPRECATED_ID);
      case "__func__": return make_simple_token(.Token.FUNCTION_NAME);
      case "__unknown__": return make_simple_token(.Token.UNKNOWN);
      case "__unused__": return make_simple_token(.Token.UNUSED);
      case "__weak__": return make_simple_token(.Token.WEAK);
    }

    // FIXME: According to the Pike lexer `__UPCASE__` symbols are okay to
    //        define by the user. It's lowercase ones that are reserved from
    //        what I now understand.
    if (has_suffix(word, "__")) {
      // SYNTAX_ERROR(
      //   "Symbols with leading and tailing double underscores are reserved\n"
      // );
      return make_simple_token(.Token.DUNDERSCORE);
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
      case "zero": return make_simple_token(.Token.ZERO_ID);
    }

    reset();

    return UNDEFINED;
  }

  private .Token.Token lex_line_comment() {
    string line = read_line();

    if (has_prefix(line, "!")) {
      current_string = line[1..];
      return make_simple_token(.Token.DOC_COMMENT);
    } else {
      current_string = line;
      return make_simple_token(.Token.COMMENT);
    }

    TODO("Return token from lex_line_comment()\n");
  }

  private .Token.Token lex_block_comment() {
    int pos = cursor;
    String.Buffer buf = String.Buffer();
    function add = buf->putchar;

    while (true) {
      int c = consume();

      if (c == UNDEFINED) {
        SYNTAX_ERROR("Unterminated block comment\n");
      }

      if (c == '*' && gobble('/')) {
        break;
      }

      add(c);
    }

    current_string = buf->get();

    return make_simple_token(.Token.BLOCK_COMMENT);
  }

  // FIXME: Escape sequences
  private .Token.Token lex_multiline_string() {
    int pos = cursor;
    String.Buffer buf = String.Buffer();
    function add = buf->putchar;
    // Eat the current "
    int prev = consume();

    while (int c = consume()) {
      if (c == '"' && prev != '\\') {
        break;
      }

      add(c);
      prev = c;
    }

    current_string = buf->get();

    return make_simple_token(.Token.STRING);
  }

  // FIXME: Escape sequences
  private .Token.Token lex_string() {
    int pos = cursor;
    String.Buffer buf = String.Buffer();
    function add = buf->putchar;
    int prev;

    while (true) {
      int c = consume();

      if (c == '\n') {
        TODO("Handle Newline in string\n");
      } else if (c == UNDEFINED) {
        SYNTAX_ERROR("Unterminated string literal\n");
      }

      if (c == '"' && prev != '\\') {
        break;
      }

      add(c);
      prev = c;
    }

    current_string = buf->get();

    return make_simple_token(.Token.STRING);
  }

  // FIXME: Escape sequences
  private .Token.Token lex_character_literal() {
    advance();

    if (char == '\\') {
      advance();
      if (!gobble('\'')) {
        SYNTAX_ERROR("Unterminated character literal\n");
      }
      current_string = sprintf("\\%c", char);
      return make_simple_token(.Token.CHAR);
    }

    if (peek_source() != '\'') {
      SYNTAX_ERROR("Unterminated character literal\n");
    }

    int v = char;
    advance();
    current_string = sprintf("%c", v);

    return make_simple_token(.Token.CHAR);
  }

  private .Token.Token lex_number() {
    current_string = read_number();
    .Token.Type t = is_float(current_string) ? .Token.FLOAT : .Token.NUMBER;
    return make_simple_token(t);
  }

  private .Token.Token lex_symbol_name() {
    [string word, function reset] = low_read_word();

    if (!word || sizeof(word) < 1) {
      SYNTAX_ERROR("Unresolved symbol\n");
    }

    return make_simple_token(.Token.SYMBOL_NAME);
  }

  private .Token.Token lex_preprocessor_directive() {
    // low_read_word() will step back one character
    consume();
    [string word, function reset] = low_read_word();

    if ((< "pike", "charset", "pragma", "include" >)[current_string]) {
      lex_state = LEX_STATE_PREPROC_UNQUOTED;
    } else if (current_string == "define") {
      TRACE("*** Set lex state to macro define\n");
      lex_state = LEX_STATE_PREPROC_DEFINE;
    }

    return make_simple_token(.Token.MACRO_DIR);
  }

  private .Token.Token lex_unquoted(.Token.Type t) {
    simple_put_back();

    String.Buffer buf = String.Buffer();
    function add = buf->putchar;

    while (int c = read_non_ws()) {
      add(c);
    }

    current_string = buf->get();
    return make_simple_token(t);
  }

  private .Token.Token lex_backtick() {
    int next_c = peek_source();

    if (!next_c) {
      SYNTAX_ERROR("Expecting token after `\n");
    }

    #define ASSERT_VALID_NEXT() do {                                          \
      if (!(< ' ', '\t', '\v', '\n', '(', ',', ';' >)[peek_source()]) {       \
        SYNTAX_ERROR("Illegal ` identifier. Expected %s", current_string);    \
      }                                                                       \
    } while (0)

    if (next_c == '_' || is_alpha(next_c)) {
      consume();
      [string word, function reset] = low_read_word();

      if (word && gobble('=')) {
        word += "=";
      }

      current_string = "`" + word;

      ASSERT_VALID_NEXT();

      return make_simple_token(.Token.SYMBOL_NAME);
    }

    if (next_c == '(') {
      advance();
      advance();

      if (char != ')') {
        SYNTAX_ERROR("Illegal ` identifier. Expected `()");
      }

      return make_simple_token("`()", .Token.SYMBOL_NAME);
    }

    while (int c = advance()) {
      switch (c) {
        case '%':
        case '&':
        case '+':
        case '/':
        case '^':
        case '|':
        case '~': {
          current_string = sprintf("`%c", c);
          ASSERT_VALID_NEXT();
          return make_simple_token(.Token.SYMBOL_NAME);
        }

        case '!': {
          if (gobble('=')) {
            current_string = "`!=";
          } else {
            current_string = "`!";
          }

          ASSERT_VALID_NEXT();
          return make_simple_token(.Token.SYMBOL_NAME);
        }

        case '*': {
          if (gobble('*')) {
            current_string = "`**";
          } else {
            current_string = "`*";
          }

          ASSERT_VALID_NEXT();
          return make_simple_token(.Token.SYMBOL_NAME);
        }

        case '-': {
          if (gobble('>')) {
            if (gobble('=')) {
              current_string = "`->=";
            } else {
              current_string = "`->";
            }
          } else {
            current_string = "`-";
          }

          ASSERT_VALID_NEXT();
          return make_simple_token(.Token.SYMBOL_NAME);
        }

        case '<': {
          if (gobble('<')) {
            current_string = "`<<";
          } else if (gobble('=')) {
            current_string = "`<=";
          } else {
            current_string = "`<";
          }

          ASSERT_VALID_NEXT();
          return make_simple_token(.Token.SYMBOL_NAME);
        }

        case '>': {
          if (gobble('>')) {
            current_string = "`>>";
          } else if (gobble('=')) {
            current_string = "`>=";
          } else {
            current_string = "`>";
          }

          ASSERT_VALID_NEXT();
          return make_simple_token(.Token.SYMBOL_NAME);
        }

        case '=': {
          if (gobble('=')) {
            current_string = "`==";
          } else {
            SYNTAX_ERROR("Illegal ` identifier. Expected `==");
          }

          ASSERT_VALID_NEXT();
          return make_simple_token(.Token.SYMBOL_NAME);
        }

        case '[': {
          if (gobble(']')) {
            current_string = "`[]";
            if (gobble('=')) {
              current_string += "=";
            }
          } else if (gobble('.')) {
            if (!gobble('.')) {
              SYNTAX_ERROR("Illegal ` identifier. Expected `[..]");
            }

            if (!gobble(']')) {
              SYNTAX_ERROR("Illegal ` identifier. Expected `[..]");
            }

            current_string = "`[..]";
          } else {
            SYNTAX_ERROR("Illegal ` identifier. Expected `[..]");
          }

          ASSERT_VALID_NEXT();
          return make_simple_token(.Token.SYMBOL_NAME);
        }
      }
    }

    SYNTAX_ERROR("Illegal ` identifier");
  }
}
