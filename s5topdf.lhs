#!/usr/bin/env runhaskell
> {-# LANGUAGE OverloadedStrings #-}

The [Shelly](https://github.com/yesodweb/Shelly.hs) library has some utilities
for making it easier to use Haskell as a shell scripting language.

> import           Shelly
> import qualified Data.Char as C
> import           System.Environment

The `main` function picks out two parameters and tries to make one a `Int`. If
any of that fails, it prints the usage message and bails.

> main = shelly $ verbosely $ do
>       args <- getArgs
>       case args of
>           [slides', url] | all C.isNumber slides' ->
>       liftIO $ putStrLn "hi"

> -- vim: set filetype=haskell:
