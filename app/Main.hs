{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO (getLine)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import           Data.Default (def)
import           Network.Bing
import           Options      (Options (..), getOptions)
import Prelude hiding (getLine)

main :: IO ()
main = do
  opts <- getOptions
  let accountKey = makeAccountKey . Char8.pack $ optAccountKey opts
      options    = def { optsKey = accountKey
                       , optsTop = 1
                       , optsCompression = True
                       }
  getLine
    >>= searchQuery options
    >>= LChar8.putStr
