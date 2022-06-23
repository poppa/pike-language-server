#ifndef PARSER_H
#define PARSER_H

#ifdef PASER_DEBUG
# define TRACE(X...)werror("%s:%d: %s",basename(__FILE__),__LINE__,sprintf(X))
#else
# define TRACE(X...)0
#endif

#define TODO(what...)                                 \
  do {                                                \
    string msg = sprintf(what);                       \
    if (!has_suffix(msg, "\n")) {                     \
      msg += "\n";                                    \
    }                                                 \
    error("TODO: %s", msg);                           \
  } while (0)

#endif /* parser.h */
