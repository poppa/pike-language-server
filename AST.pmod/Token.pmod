enum Type {
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
  DIV,                                    // /
  DOC_COMMENT,                            // //!
  DOT,                                    // .
  GREATER_THAN,                           // >
  LESS_THAN,                              // <
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
  SYMBOL_NAME,                            // symbol name
  TILDE,                                  // ~
  XOR,                                    // ^
}

class Position(
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

class Location(
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

class Token(
  public Type type,
  public Location location,
  public string value
) {
  protected mixed cast(string how) {
    switch (how) {
      case "mapping":
        return ([
          "type": type,
          "location": (mapping) location,
          "value": value
        ]);

      default: error("Can't cast %O to %O\n", object_program(this), how);
    }
  }

  protected string _sprintf(int m) {
    switch (m) {
      default:
        return sprintf(
          "%O(type: %O, value: %O, location: %O)",
          object_program(this), type, value, location
        );
    }
  }
}
