{-# LANGUAGE OverloadedStrings #-}

module Network.Bing
    ( searchQuery
    , searchQueryBody
    , module Network.Bing.Types
    ) where

import           Control.Lens         ((&), (.~), (?~), (^.), view)
import           Data.Aeson.Lens      (key, nth, _String)
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable        (foldr')
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text.Lazy.Encoding   as Text
import qualified Data.Text.Lazy as Text
import           TextShow             (showt)

import           Network.Bing.Types
import           Network.Wreq         (auth, basicAuth, defaults, getWith,
                                       header, param, responseBody, Response)
import qualified Network.Wreq         as Wreq

searchQuery :: Options -> Text -> IO (Response ByteString)
searchQuery options str = do
  let format      = optsFormat     options
      accountKey  = unAccountKey $ optsKey options
      service     = optsService    options
      top         = optsTop        options
      compression = optsCompression options
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
  getWith (applyOptions requestOpts) (getUrl service)

searchQueryBody :: Options -> Text -> IO ByteString
searchQueryBody opts = fmap (view responseBody) . searchQuery opts

applyOptions :: [Wreq.Options -> Wreq.Options] -> Wreq.Options
applyOptions = foldr' ($) defaults
