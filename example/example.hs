{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import Web.Porpoise

main :: IO ()
main = Warp.run 8080 $ toApplication do
  pure $ responseLBS (mkStatus 404 "Thingy not found") [(hContentType, "text/html")] "<p> Ya sorry </p>"
