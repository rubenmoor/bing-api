{-# LANGUAGE OverloadedStrings #-}

module Network.Bing
    ( searchQuery
    , module Network.Bing.Types
    ) where

import           Control.Lens          ((&), (.~), (^.))
import           Data.ByteString.Lazy  (ByteString)
import           Data.Foldable         (foldr')
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import           TextShow              (showt)

import           Network.Bing.Types
import           Network.Wreq          (defaults, getWith, header, param,
                                        responseBody)
import qualified Network.Wreq          as Wreq

searchQuery :: Options -> Text -> IO ByteString
searchQuery options str = do
  let format      = optsFormat     options
      accountKey  = unAccountKey $ optsKey options
      service     = optsService    options
      top         = optsTop        options
      compression = optsCompression options
      requestOpts =
        [ param "$format" .~ [showt format]
        , param "$top" .~ [showt top]
        , param "Query" .~ ["'" <> str <> "'"]
        , header "Authorization" .~ ["Basic " <> accountKey]
        ] <> if compression
                then [header "Accept-Encoding" .~ ["gzip"]]
                else []
  r <- getWith (applyOptions requestOpts) (getUrl service)
  pure $ r ^. responseBody

applyOptions :: [Wreq.Options -> Wreq.Options] -> Wreq.Options
applyOptions = foldr' ($) defaults
