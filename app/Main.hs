{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default (def)
import Network.Bing

main :: IO ()
main = do
  let accountKey = makeAccountKey "UDjp1eB2K7fUaHtR2kF3rvY0gckbZa3HbUzZlk/6PrU="
      options    = def { optsKey = accountKey
                       , optsTop = 1
                       , optsCompression = True
                       }
  result <- searchQuery options "XBox"
  print result
