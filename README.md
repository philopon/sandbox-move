sandbox-move
============
move cabal sandbox directory.

example
==========
```.sh
$ ls -a from
.                    LICENSE              main.hs
..                   Setup.hs             sandbox-move.cabal
.README.md.swp       cabal.sandbox.config
.cabal-sandbox       dist
$ sandbox-move from to
$ ls -a to
.                    LICENSE              main.hs
..                   Setup.hs             sandbox-move.cabal
.README.md.swp       cabal.sandbox.config
.cabal-sandbox       dist
$ cd to
$ cabal sandbox hc-pkg -- check
```
