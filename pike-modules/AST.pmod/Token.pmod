#charset utf-8
#pike __REAL_VERSION__

// NOTE: Keep this in sync with the string array below
public enum Type {
  /* Taken from the C source */

  ADD_EQ,                                 // +=
  AND_EQ,                                 // &=
  ARRAY_ID,                               // array
  ARROW,                                  // ->
  ATOMIC_GET_SET,                         // ?=
  ATTRIBUTE_ID,                           // __attribute__
  AUTO_ID,                                // auto
  BITS,                                   // bits
  BREAK,                                  // break
  CASE,                                   // case
  CATCH,                                  // catch
  CLASS,                                  // class
  COLON_COLON,                            // ::
  CONSTANT,                               // constant
  CONTINUE,                               // continue
  DEC,                                    // --
  DEFAULT,                                // default
  DEPRECATED_ID,                          // __deprecated__
  DIV_EQ,                                 // /=
  DO,                                     // do
  DOT_DOT_DOT,                            // ...
  DOT_DOT,                                // ..
  ELSE,                                   // else
  ENUM,                                   // enum
  EQ,                                     // ==
  EXTERN,                                 // extern
  FINAL_ID,                               // final
  FLOAT_ID,                               // float
  FLOAT,                                  // float literal
  FOR,                                    // for
  FOREACH,                                // foreach
  FUNCTION_ID,                            // function
  FUNCTION_NAME,                          // __func__ FIXME: __FUNCTION__???
  GAUGE,                                  // gauge
  GE,                                     // >=
  GLOBAL,                                 // global
  IDENTIFIER,                             // identifier
  IF,                                     // if
  IMPORT,                                 // import
  INC,                                    // ++
  INHERIT,                                // inherit
  INLINE,                                 // inline
  INT_ID,                                 // int
  LAMBDA,                                 // lambda
  LAND,                                   // &&
  LE,                                     // <=
  LEX_EOF,                                // end of file
  LOCAL_ID,                               // local
  LOR,                                    // ||
  LSH_EQ,                                 // <<=
  LSH,                                    // <<
  MAPPING_ID,                             // mapping
  MIXED_ID,                               // mixed
  MOD_EQ,                                 // %=
  MULT_EQ,                                // *=
  MULTISET_END,                           // >)
  MULTISET_ID,                            // multiset
  MULTISET_START,                         // (<
  NE,                                     // !=
  NOT,                                    // !
  NUMBER,                                 // integer literal
  OBJECT_ID,                              // object
  OPTIONAL,                               // optional
  OR_EQ,                                  // |=
  POW_EQ,                                 // **=
  POW,                                    // **
  PREDEF,                                 // predef
  PRIVATE,                                // private
  PROGRAM_ID,                             // program
  PROTECTED,                              // protected
  PUBLIC,                                 // public
  RESERVED,                               // reserved identifier
  RETURN,                                 // return
  RSH_EQ,                                 // >>=
  RSH,                                    // >>
  SAFE_APPLY,                             // (?
  SAFE_INDEX,                             // ->?
  SAFE_START_INDEX,                       // [?
  SSCANF,                                 // sscanf
  STATIC_ASSERT,                          // _Static_assert
  STATIC,                                 // static
  STRING_ID,                              // string
  STRING,                                 // string literal
  SUB_EQ,                                 // -=
  SWITCH,                                 // switch
  TYPEDEF,                                // typedef
  TYPEOF,                                 // typeof
  UNKNOWN,                                // __unknown__
  UNUSED,                                 // __unused__
  VARIANT,                                // variant
  VERSION,                                // version prefix
  VOID_ID,                                // void
  WEAK,                                   // __weak__
  WHILE,                                  // while
  XOR_EQ,                                 // ^=

  /* Additions, not from Pike C source */
  AMP,                                    // &
  ARRAY_END,                              // })
  ARRAY_START,                            // ({
  ASSIGN,                                 // =
  AT,                                     // @
  BLOCK_COMMENT,                          // /*
  BRACKET_LEFT,                           // [
  BRACKET_RIGHT,                          // ]
  CHAR,                                   // character literal
  COLON,                                  // :
  COMMA,                                  // ,
  COMMENT,                                // //
  CURLY_LEFT,                             // {
  CURLY_RIGHT,                            // }
  CONT_LINE,                              // backslash
  DIV,                                    // /
  DOC_COMMENT,                            // //!
  DOT,                                    // .
  DUNDERSCORE,                            // Double underscore __XXX__
  GREATER_THAN,                           // >
  LESS_THAN,                              // <
  MACRO_DIR,                              // #(...)
  MACRO_LITERAL,                          // string literal
  MAPPING_END,                            // ])
  MAPPING_START,                          // ([
  MINUS,                                  // -
  MOD,                                    // %
  MULT,                                   // *
  PAREN_LEFT,                             // (
  PAREN_RIGHT,                            // )
  PIPE,                                   // |
  PLUS,                                   // +
  QUESTION,                               // ?
  SEMICOLON,                              // ;
  TILDE,                                  // ~
  XOR,                                    // ^
  ZERO_ID,                                // zero

  // Special
  EOF,                                    // End of file
}

public string type_to_string(Token|Type t) {
  if (!intp(t)) {
    t = t->type;
  }

  return ({
    "ADD_EQ",
    "AND_EQ",
    "ARRAY_ID",
    "ARROW",
    "ATOMIC_GET_SET",
    "ATTRIBUTE_ID",
    "AUTO_ID",
    "BITS",
    "BREAK",
    "CASE",
    "CATCH",
    "CLASS",
    "COLON_COLON",
    "CONSTANT",
    "CONTINUE",
    "DEC",
    "DEFAULT",
    "DEPRECATED_ID",
    "DIV_EQ",
    "DO",
    "DOT_DOT_DOT",
    "DOT_DOT",
    "ELSE",
    "ENUM",
    "EQ",
    "EXTERN",
    "FINAL_ID",
    "FLOAT_ID",
    "FLOAT",
    "FOR",
    "FOREACH",
    "FUNCTION_ID",
    "FUNCTION_NAME",
    "GAUGE",
    "GE",
    "GLOBAL",
    "IDENTIFIER",
    "IF",
    "IMPORT",
    "INC",
    "INHERIT",
    "INLINE",
    "INT_ID",
    "LAMBDA",
    "LAND",
    "LE",
    "LEX_EOF",
    "LOCAL_ID",
    "LOR",
    "LSH_EQ",
    "LSH",
    "MAPPING_ID",
    "MIXED_ID",
    "MOD_EQ",
    "MULT_EQ",
    "MULTISET_END",
    "MULTISET_ID",
    "MULTISET_START",
    "NE",
    "NOT",
    "NUMBER",
    "OBJECT_ID",
    "OPTIONAL",
    "OR_EQ",
    "POW_EQ",
    "POW",
    "PREDEF",
    "PRIVATE",
    "PROGRAM_ID",
    "PROTECTED",
    "PUBLIC",
    "RESERVED",
    "RETURN",
    "RSH_EQ",
    "RSH",
    "SAFE_APPLY",
    "SAFE_INDEX",
    "SAFE_START_INDEX",
    "SSCANF",
    "STATIC_ASSERT",
    "STATIC",
    "STRING_ID",
    "STRING",
    "SUB_EQ",
    "SWITCH",
    "TYPEDEF",
    "TYPEOF",
    "UNKNOWN",
    "UNUSED",
    "VARIANT",
    "VERSION",
    "VOID_ID",
    "WEAK",
    "WHILE",
    "XOR_EQ",
    "AMP",
    "ARRAY_END",
    "ARRAY_START",
    "ASSIGN",
    "AT",
    "BLOCK_COMMENT",
    "BRACKET_LEFT",
    "BRACKET_RIGHT",
    "CHAR",
    "COLON",
    "COMMA",
    "COMMENT",
    "CURLY_LEFT",
    "CURLY_RIGHT",
    "CONT_LINE",
    "DIV",
    "DOC_COMMENT",
    "DOT",
    "DUNDERSCORE",
    "GREATER_THAN",
    "LESS_THAN",
    "MACRO_DIR",
    "MACRO_LITERAL",
    "MAPPING_END",
    "MAPPING_START",
    "MINUS",
    "MOD",
    "MULT",
    "PAREN_LEFT",
    "PAREN_RIGHT",
    "PIPE",
    "PLUS",
    "QUESTION",
    "SEMICOLON",
    "TILDE",
    "XOR",
    "ZERO_ID",
    "EOF",
  })[t] || "<< UNRESOLVED >>";
}

public bool is_attribute(Token t) {
  return (< ATTRIBUTE_ID, DEPRECATED_ID >)[t->type];
}

public bool is_builtin_type(Token|Type t) {
  if (!intp(t)) {
    t = t->type;
  }

  return (<
    ARRAY_ID,
    AUTO_ID,
    FINAL_ID,
    FLOAT_ID,
    FUNCTION_ID,
    INT_ID,
    LOCAL_ID,
    MAPPING_ID,
    MIXED_ID,
    MULTISET_ID,
    OBJECT_ID,
    PROGRAM_ID,
    STRING_ID,
    VOID_ID,
    ZERO_ID,
  >)[t];
}

public bool is_modifier(Token|Type t) {
  if (!intp(t)) {
    t = t->type;
  }

  return (<
    UNUSED,
    WEAK,
    CONTINUE,
    EXTERN,
    FINAL_ID,
    INLINE,
    LOCAL_ID,
    OPTIONAL,
    PRIVATE,
    PROTECTED,
    PUBLIC,
    STATIC,
    VARIANT
  >)[t];
}

constant PRAGMA_DIRECTIVES = (<
  "all_final",
  "all_inline",
  "compiler_trace",
  "deprecation_warnings",
  "disassemble",
  "dont_save_parent",
  "dynamic_dot",
  "no_compiler_trace",
  "no_deprecation_warnings",
  "no_disassemble",
  "no_dynamic_dot",
  "no_strict_types",
  "save_parent",
  "strict_types",
>);

public class Position(
  public int byte,
  public int line,
  public int column
) {
  protected mixed cast(string how) {
    switch (how) {
      case "mapping": return ([ "byte": byte, "line": line, "column": column ]);
      default: error("Can't cast %O to %O\n", object_program(this), how);
    }
  }

  protected string _sprintf(int m) {
    switch (m) {
      default:
        return sprintf(
          "%O(byte: %O, line: %O, column: %O)",
          object_program(this), byte, line, column
        );
    }
  }
}

public class Location(
  public string file,
  public Position start,
  public Position end
) {
  protected mixed cast(string how) {
    switch (how) {
      case "mapping":
        return ([
          "file": file,
          "start": (mapping) start,
          "end": (mapping) end
        ]);

      default: error("Can't cast %O to %O\n", object_program(this), how);
    }
  }

  protected string _sprintf(int m) {
    switch (m) {
      default:
        return sprintf(
          "%O(file: %O, start: %O, end: %O)",
          object_program(this), file, start, end
        );
    }
  }
}

public class Token(
  public Type type,
  public Location location,
  public string value,
  public int /* .Lexer.State */ context,
) {
  protected mixed cast(string how) {
    switch (how) {
      case "mapping":
        return ([
          "type": type,
          "location": (mapping) location,
          "value": value,
          "context": context,
        ]);

      default: error("Can't cast %O to %O\n", object_program(this), how);
    }
  }

  protected string _sprintf(int m) {
    switch (m) {
      default:
        return sprintf(
          "%O(type: %O, value: %O, location: %O, ctx: %O)",
          object_program(this), type_to_string(type), value, location, context
        );
    }
  }
}

public class EofToken {
  inherit Token;

  protected void create(Location loc, int /* .Lexer.State */ context) {
    ::create(EOF, loc, "\0", context);
  }

  public bool `!() {
    return true;
  }

  public bool `==(mixed what) {
    if (undefinedp(what) || what == 0) {
      return true;
    }

    return false;
  }
}
