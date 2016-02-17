module Options
  ( options
  , getOptions
  , Options (..)
  ) where

import           Options.Applicative
import Network.Bing.Types

-- data Options = Options
  -- { optAccountKey :: String
  -- }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> options)
   ( fullDesc
  <> progDesc "Access to the Bing search engine via command line"
  <> header   "Haskell Bing API"
   )

options :: Parser Options
options = Options
  <$> option auto ( long    "account-key"
                 <> short   'k'
                 <> metavar "KEY"
                 <> help    "An Azure account key from https://datamarket.azure.com/account/keys"
                  )
  <*> option auto ( long    "format"
                 <> short   'f'
                 <> metavar "FORMAT"
                 <> value   JSON
                 <> showDefault
                 <> help    "JSON or XML"
                  )
  <*> option auto ( long    "service"
                 <> short   's'
                 <> metavar "SERVICE"
                 <> value   WebOnly
                 <> showDefault
                 <> help    "One of WebOnly, Web, Image, Video, News, SpellingSuggestion, RelatedSearch, Composite [Web,Image, ...]"
                 )
  <*> option auto ( long    "top"
                 <> short   't'
                 <> metavar "NUMBER"
                 <> value   10
                 <> showDefault
                 <> help    "number of results"
                 )
  <*> option auto ( long     "skip"
                 <> short    's'
                 <> metavar  "NUMBER"
                 <> value    0
                 <> showDefault
                 <> help     "pagination: starting point offset"
                 )
  <*> option auto ( long     "compression"
                 <> short    'c'
                 <> metavar   "BOOL"
                 <> value    False
                 <> showDefault
                 <> help     "use gzip to compress web traffic"
                 )
  <*> option auto ( long     "query-parameters"
                 <> short    'q'
                 <> metavar  "QUERY"
                 <> value    []
                 <> showDefault
                 <> help     "additional query parameters, e.g. ODATA, like this: -q '[(\"foo\", \"bar\")]'")
