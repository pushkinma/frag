![Haskell frag 3D-shooter game screenshot](/Screenshot.png)
# frag [![Realease](https://img.shields.io/badge/Release-v1.2-blue.svg)](https://github.com/pushkinma/frag) [![Windows build status](https://img.shields.io/badge/Windows%20build-passing-brightgreen.svg)](https://ci.appveyor.com/project/pushkinma/frag)
Patched version of [frag](https://wiki.haskell.org/Frag)

What's new:
* Building with [stack](http://haskellstack.org)
* [GHC 8.0](https://www.haskell.org/ghc/) builds with no warnings
* Internal [AFRP](https://wiki.haskell.org/Arrows-based_Functional_Reactive_Programming) replaced by external [Yampa](https://wiki.haskell.org/Yampa)
* [HLint](https://hackage.haskell.org/package/hlint) `No hints` (ignore [Reduce duplication](https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/extending_ghc.html#annotating-modules))

Building:
  `stack install`

Usage:
  `frag leveleg`
