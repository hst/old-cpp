#!/usr/bin/env runhaskell

> import Distribution.Simple
> import System

Use the usual main function from Cabal, with a hook for running our
tests.

> main = defaultMainWithHooks (defaultUserHooks {
>                                runTests = runHSTTests
>                              })

A hook for running tests.  The ‘tests’ executable will be built during
the build phase.

> runHSTTests args b package buildInfo = do
>     system "dist/build/tests/tests"
>     return ()
