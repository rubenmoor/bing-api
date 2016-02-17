{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8      as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import           Data.Default               (def)
import           Data.Text.IO               (getLine)
import           Network.Bing
import           Options                    (Options (..), getOptions)
import           Prelude                    hiding (getLine)

main :: IO ()
main = do
  opts <- getOptions
  getLine
    >>= searchQueryBody opts
    >>= LChar8.putStr
