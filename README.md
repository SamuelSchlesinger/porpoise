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
