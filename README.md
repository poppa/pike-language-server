# Pike Language Server

![Pike Programming Language](assets/pike_logo.png)

**!!!NOTE!!!** This project is super very much in its infancy **!!!NOTE!!!**

This is a humble an naive stab at implementing a
[Language Server](https://microsoft.github.io/language-server-protocol/) for the
[Pike](https://pike.lysator.liu.se/) programming language.

I have no idea how far this project will go, but you have to take the first step
to get somewhere, so here it is.

## Outline

The first and primary goal is to get a linter working. This has no connection
to the Language Server (LS, and LSP for the protocol) per se. The idea is to
create a standalone Pike linter (and consequently a code formatter). If this
goal is acheived the linter can then be used in the LS.

### Linter and formatter - Lexing, Parsing and AST

AFAIK Pike doesn't have any Abstract Syntax Tree (AST) representation and
there's no way (at least no easy way) to utilize Pike's internal C-based
lexer and parser to statically parse and analyze Pike code. So static lexing
and parsing needs to be re-implemented to then create an AST for proper linting
and formatting.

A working (we'll to the best of my abilities an knowlege)
[lexer](pike-modules/AST.pmod/Lexer.pmod) is in place and seems to lex Pike
code pretty well. It may need some performance imrovements, but hey, its doing
stuff. I took a long and hard look at Pike's built-in C-lexer, so this new one
shouldn't deviate too much from the built-in, albeit it's extended with some
extra and more granular token types.

So the next step is to create a parser that will generate an AST.

### LSP implementation

I've started at a basic LSP implementation and an LS client for
[VSCode](https://code.visualstudio.com/). The client and server can communicate
with eachother, albeit noting real is done at the moment.

The idea here is, of course, that the LSP implementation should be "standalone"
and not depend on any particular client. It should also be possible to implement
different server types since various editors and IDE's may prefer different
ways of communcation.

The current intial server is a "Stdio"-server which simply reads from `stdin`
and writes to `stdout`. It may not be the most performant method, and it's
probably not very multi-threaded, but it works for some of the initial LSP
stuff.

### JSON-RPC

The LSP uses [`JSON-RPC`](https://www.jsonrpc.org/) as messaging protocol. I've
written a simple [`JSON-RPC` implementation](pike-modules/JsonRpc.pmod/module.pmod).

### Testing testing testing

Personally I thing Pike's built-in testing framework is super-clonky and
har to use, so I've created my own, extremely limited, unit testing framework
called [Pest](https://github.com/poppa/pest), which, you can tell by its name,
is influenced by [Jest](https://jestjs.io/).

All Pike tests reside in [pike-modules/test](pike-modules/test/).

Pest is a Git-submodule of this repository so you won't need to clone
that repo to work with this.

To run the Pike tests you can simply run `npm run test:pike` in the root of
this repository. To run a specific test file, or a specific test, you can
pass some arguments:

```sh
# Run all test files starting with "lsp."
npm run test:pike -- -f "lsp.*"

# Run all tests where the description of the test contains the word "callback"
# in all test files starting with "lsp."
npm run test:pike -- -f "lsp.*" -t "*callback*"
```
