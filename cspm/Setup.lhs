#!/usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import System

Use the usual main function from Cabal, with a hook for running our
tests.

> main = defaultMainWithHooks (defaultUserHooks {
>                                runTests = runHSTTests
>                              })

Copy the ‘test’ directory into Cabal's build directory.  This will
make it easier to construct the right paths when we start calling the
test scripts, and makes sure that any temporary files that the test
scripts create get stashed away in the build directory.

> copyTests dest
>     = do
>       putStrLn "Copying test scripts..."
>       system $ "cp -R tests " ++ dest
>       return ()

A hook for running tests.  We pass in the location of Cabal's
dist/build directory, since the test scripts will call several
executables that are built there.

> runHSTTests args b package buildInfo
>     = do
>       let dest = buildDir buildInfo
>       copyTests dest
>       system ("DIST_BUILD=" ++ dest ++ " " ++
>               dest ++ "/tests/run-tests.sh")
>       return ()
