#ifndef LSP_TEXTDOCUMENT_H
#define LSP_TEXTDOCUMENT_H

#if defined(PLS_LSP_TEXTDOCUMENT_DEBUG) || defined(PLS_LSP_DEBUG)
# define DEBUG(X...)werror("%s:%d: %s",basename(__FILE__),__LINE__,sprintf(X))
#else
# define DEBUG(X...)0
#endif

#endif
