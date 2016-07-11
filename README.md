![Haskell frag 3D-shooter game screenshot](/Screenshot.png)

Patched version of [frag](https://wiki.haskell.org/Frag)

Differences:
* Building with [stack](http://haskellstack.org)
* [GHC 8.0](https://www.haskell.org/ghc/) builds with no warnings (7.10 too)
* Internal [AFRP](https://wiki.haskell.org/Arrows-based_Functional_Reactive_Programming) replaced by external [Yampa](https://wiki.haskell.org/Yampa)
* [HLint](https://hackage.haskell.org/package/hlint) `No hints` (ignore [Reduce duplication](https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/extending_ghc.html#annotating-modules))

Building:
  `stack install`

Usage:
  `frag-exe leveleg`
