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

##Configuration

###Ignoring files

Create a json file with the ingore list (let's call it config.json):
```json
{
    "ignore": ["foo.hs", "bar.hs", "baz.hs"]
}
```
Make sure you put this config file the root directory of your project.

Change your driver to specify the config file name
```haskell
{-# OPTIONS_GHC -F -pgmF doctest-discover -optF config.json #-}
```

###Source folders
Here's how you specify a list of source folders:

```json
{
    "ignore": ["foo.hs", "bar.hs", "baz.hs"],
    "sourceFolders": ["src", "src/foobar", "someOtherSource"]
}
```

###*Note*

This is in beta. Future versions will let you add configuration for passing additional flags to doctest.
