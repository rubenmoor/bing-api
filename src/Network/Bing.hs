{-# LANGUAGE OverloadedStrings #-}

module Network.Bing
    ( searchQuery
    , searchQueryBody
    , module Network.Bing.Types
    ) where

import           Control.Lens            (view, (.~), (?~))
import           Data.ByteString.Lazy    (ByteString)
import           Data.Foldable           (foldr')
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           TextShow                (showt)

import           Network.Bing.Types
import           Network.Wreq            (Response, auth, basicAuth, defaults,
                                          getWith, header, param, responseBody)
import qualified Network.Wreq            as Wreq

-- | query bing with given 'Options' and get a 'Network.Wreq.Response'
searchQuery :: Options -> Text -> IO (Response ByteString)
searchQuery options str = do
  let format      = optsFormat      options
      accountKey  = unAccountKey  $ optsKey options
      service     = optsService     options
      top         = optsTop         options
      compression = optsCompression options
      queryParams = optsQueryParams options
      requestOpts =
        [ param "$format" .~ [showt format]
        , param "$top"    .~ [showt top]
        , param "Query"   .~ ["'" <> str <> "'"]
        , auth ?~ basicAuth accountKey accountKey
        ]
        <> if compression
              then [header "Accept-Encoding" .~ ["gzip"]]
              else []
        <> getHeaders service
        <> map toOptionParam queryParams
  getWith (applyOptions requestOpts) (getUrl service)

-- | query bing with given 'Options' and get a 'Network.Wreq.Response'
searchQueryBody :: Options -> Text -> IO ByteString
searchQueryBody opts = fmap (view responseBody) . searchQuery opts

applyOptions :: [Wreq.Options -> Wreq.Options] -> Wreq.Options
applyOptions = foldr' ($) defaults

toOptionParam :: (Text, Text) -> Wreq.Options -> Wreq.Options
toOptionParam (key, value) = param key .~ [value]
