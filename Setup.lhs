#! /usr/bin/env runhaskell

> import Setup.Configure (configure)
> import Distribution.Simple
> main = defaultMainWithHooks $ simpleUserHooks { confHook = configure }
