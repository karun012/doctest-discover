#doctest-discover

Do you spend time creating a doctest driver for every cabal project? 

doctest-discover (inspired by hspec-discover) makes it easy to run doctests via cabal.

The only thing you need to do is create a driver file with one line:

```haskell
{-# OPTIONS_GHC -F -pgmF doctest-discover #-}
```

doctest-discover is used as a pre-processor. Since there is nothing to pre-process in the file, it builds a temporary file with the code needed to run doctests 
via cabal.
