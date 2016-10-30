# c-parser-in-haskell
An implementation of a C parser in Haskell. Designed for refactoring.

This runs on GHC 7.8.4

# Installing

Use stack to install it in a sandbox:
 - `stack setup`
 - `stack install`
 
Or get a GHC 7.8.4 and install it with `cabal install`

# Usage

An example program using the parser is something like that: 

```
module Main where

import MiniC.AST
import MiniC.MiniCPP
import MiniC.ParseProgram
import MiniC.Parser.Lexical
import MiniC.Parser

main = do let testFile = "slre-master/slre.h"
          source <- readFile testFile
          ast <- parseWithPreproc (whole translationUnit) testFile source
          doSomethingWithAST ast
```

Where `doSomethingWithAST` analyzes the syntax tree and returns some result to the user.
