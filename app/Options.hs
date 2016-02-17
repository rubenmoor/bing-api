module Options
  ( options
  , getOptions
  , Options (..)
  ) where

import           Options.Applicative

data Options = Options
  { optAccountKey :: String
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> options)
   ( fullDesc
  <> progDesc "Access to the Bing search engine via command line"
  <> header   "Haskell Bing API"
   )

options :: Parser Options
options = Options
  <$> strOption ( long    "account-key"
               <> short   'k'
               <> metavar "KEY"
               <> help    "An Azure account key from https://datamarket.azure.com/account/keys"
                )
