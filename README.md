# bing-api

A Haskell library to access the MS Bing API for web searches

## Command-line use

Install [stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)

    $ git clone https://github.com/rubenmoor/bing-api.git
    $ cd bing-api
    $ stack install

For available options do

    $ bing-api-exe -h

Search for xbox

    $ echo xbox | bing-api-exe -k my_azure_account_key

## Use as library

    import Data.Text.IO    (putStrLn)
    import Network.Bing    (Options (..), Composite, Web, Image, makeAccountKey, searchQuery)
    import Control.Lens    ((^.))
    import Data.Aeson.Lens (key, nth, _String)
    import Data.Text       (putStrLn)
    import Prelude hiding  (putStrLn)

    main :: IO ()
    main = do
      let options = def { optsKey     = makeAccountKey "my_azure_account_key"
                        , optsTop     = 1
                        , optsService = Composite [Web, Image]
                        }
      r <- searchQuery options "xbox"
      let url = r ^. key "d" . key "results" . nth 0 . key "Url" . _String 
      putStrLn url