#ifndef PARSER_H
#define PARSER_H

#define TODO(what...)                                 \
  do {                                                \
    string msg = sprintf(what);                       \
    if (!has_suffix(msg, "\n")) {                     \
      msg += "\n";                                    \
    }                                                 \
    error("TODO: %s", msg);                           \
  } while (0)

#endif /* parser.h */
