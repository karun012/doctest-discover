#doctest-discover

* Do you spend time creating a doctest driver for every cabal project? 

* Do you have to maintain a list of source files that cabal needs to use to run doctests?

doctest-discover (inspired by hspec-discover) makes it easy to run doctests via cabal.

The only thing you need to do is create a driver file (let's call it doctest-driver.hs) with one line:

```haskell
{-# OPTIONS_GHC -F -pgmF doctest-discover #-}
```

Use this file as the main for the test-suite in your cabal file: 
```cabal
test-suite cooler-doctests
  default-language:   Haskell2010
  type:               exitcode-stdio-1.0
  ghc-options:        -threaded
  main-is:            doctest-driver.hs
  build-depends:      base >4 && <5, doctest-discover
  HS-Source-Dirs:     test

```

doctest-discover is used as a pre-processor. Since there is nothing to pre-process in the file, it builds a temporary file with the code needed to run doctests 
via cabal.
