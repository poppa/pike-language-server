#ifndef AST_H
#define AST_H

#define TODO(what...) error("TODO: " + sprintf(what))

#ifdef AST_DEBUG
# define ASSERT_DEBUG(what, msg) if (!(what)) error(msg)
#else
# define ASSERT_DEBUG(what, msg) 0
#endif

#ifdef AST_DEBUG
# define TRACE(X...)werror("%s:%d: %s",basename(__FILE__),__LINE__,sprintf(X))
#else
# define TRACE(X...)0
#endif


#endif
