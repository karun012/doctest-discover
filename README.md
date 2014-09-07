#doctest-discover

* Do you have to maintain a list of source files that cabal needs to use to run [doctests](https://hackage.haskell.org/package/doctest)?

doctest-discover (inspired by [hspec-discover](https://hackage.haskell.org/package/hspec-discover)) makes it easy to run doctests via [cabal](http://www.haskell.org/cabal/).

doctest-discover is published on [hackage](https://hackage.haskell.org/package/doctest-discover)

The only thing you need to do is create a driver file with one line (let's call it *doctest-driver.hs*):

```haskell
{-# OPTIONS_GHC -F -pgmF doctest-discover #-}
```

Use it as the main for the test-suite in your cabal file: 
```cabal
test-suite cooler-doctests
  default-language:   Haskell2010
  type:               exitcode-stdio-1.0
  ghc-options:        -threaded
  main-is:            doctest-driver.hs
  build-depends:      base >4 && <5, doctest, doctest-discover
  HS-Source-Dirs:     test
```

doctest-discover is used as a pre-processor. Since there is nothing to pre-process in the file, it builds a temporary file with the code needed to run doctests via cabal.

###*Note*

This is in beta. This version assumes that your source files are in a directory named **"src"**. Future versions will let you add configuration for ignoring tests (not recommended if you are into TDD :smirk: ), passing additional flags to doctest, using a different folder for your source files, etc.
