#ifndef LSP_SERVER_H
#define LSP_SERVER_H

#if defined(PLS_LSP_SERVER_DEBUG) || defined(PLS_LSP_DEBUG)
# define DEBUG(X...)werror("%s:%d: %s",basename(__FILE__),__LINE__,sprintf(X))
#else
# define DEBUG(X...)0
#endif

#endif
