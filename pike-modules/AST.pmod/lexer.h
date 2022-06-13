#ifndef LEXER_H
#define LEXER_H

#define TODO(what...)                                 \
  do {                                                \
    string msg = sprintf(what);                       \
    if (!has_suffix(msg, "\n")) {                     \
      msg += "\n";                                    \
    }                                                 \
    error("TODO: %s%s", msg, position_info_message);  \
  } while (0)

#ifdef LEXER_DEBUG
# define ASSERT_DEBUG(what, msg) if (!(what)) error(msg)
#else
# define ASSERT_DEBUG(what, msg) 0
#endif

#ifdef LEXER_DEBUG
# define TRACE(X...)werror("%s:%d: %s",basename(__FILE__),__LINE__,sprintf(X))
#else
# define TRACE(X...)0
#endif

#ifdef LEXER_CALL_COUNT
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
#endif // LEXER_CALL_COUNT

#define SYNTAX_ERROR(ARGS...) {                                           \
  string msg = sprintf(ARGS);                                             \
  if (!has_suffix(msg, "\n")) {                                           \
    msg += "\n";                                                          \
  }                                                                       \
  msg += position_info_message;                                           \
  error(msg);                                                             \
}

#endif /* LEXER_H */
