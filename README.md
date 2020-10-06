# Porpoise

[![Hackage](https://img.shields.io/hackage/v/porpoise.svg)](https://hackage.haskell.org/package/porpoise)

[![Saving the Finless Porpoise](https://raw.githubusercontent.com/SamuelSchlesinger/porpoise/main/porpoise.jpeg)](https://www.worldwildlife.org/stories/saving-the-finless-porpoise)

An extremely minimal, unopinionated framework for building HTTP servers built over the [wai](https://hackage.haskell.org/package/wai) server interface.

This framework is unopinionated, but here is the sort of structure I would recommend while using
it:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Prelude hiding (id, (.))
import Data.ByteString (ByteString)
import Web.Porpoise
import Data.Text (Text)
import Data.Aeson (Value)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)

data Command =
    HealthCheck
  | NoMatch

main :: IO ()
main = run 8080 $ toApplication (processCommand . requestCommand)

requestCommand :: Server IO Request Command
requestCommand = do
  r <- id
  body <- liftIO $ lazyRequestBody r
  pure $ case (requestMethod r, pathInfo r, queryString r, body) of
    ("GET", ["health"], [], "") -> HealthCheck
    _ -> NoMatch

healthy204 :: Status
healthy204 = mkStatus 204 "Healthy"

some404 :: Status
some404 = mkStatus 404 "Not Found"

allContentTypes :: (ByteString, ByteString)
allContentTypes = ("Content-Type", "*/*")

emptyBody :: ByteString
emptyBody = ""

processCommand :: Server IO Command Response
processCommand = id >>= pure . \case
  HealthCheck ->
    responseBuilder healthy204 [allContentTypes] emptyBody
  NoMatch ->
    responseBuilder some404 [allContentTypes] emptyBody
```

This framework is not a silver bullet. In particular, when you have a typical
HTTP application, you probably want to use something like `servant`. But lets say
you are porting an old service over from some other language to Haskell, and
you can't leverage the existing server combinators in `servant`, then this
is an alternative to writing it by hand in `wai`, giving the `Server` type
which reveals some of the nice structure inside of the `wai` concept of a `Server`.

What you shouldn't do is use `Server` for all of your code, necessarily. It
does not have an instance of `MonadUnliftIO`, and your server code should almost
certainly be written in a type that can have an instance for that class. This
library is meant to form the top, infrastructural layer of a server program,
and allow for certain types of control flow at that layer, such as the use of
`callCC`.
