{-# LANGUAGE OverloadedStrings #-}

module Network.Bing
    ( searchQuery
    , module Network.Bing.Types
    ) where

import           Control.Lens         ((&), (.~), (?~), (^.))
import           Data.Aeson.Lens      (key, _String)
import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import           TextShow             (showt)

import           Network.Bing.Types
import           Network.Wreq         (auth, basicAuth, defaults, getWith,
                                       header, param, responseBody)

searchQuery :: Options -> Text -> IO ByteString
searchQuery options str = do
  let format      = optsFormat     options
      accountKey  = unAccountKey $ optsKey options
      service     = optsService    options
      top         = optsTop        options
      compression = optsCompression options
      requestOpts = defaults & param "$format" .~ [showt format]
                             & param "$top" .~ [showt top]
                             & param "Query" .~ ["'" <> str <> "'"]
                             & header "Authorization" .~ ["Basic " <> accountKey]
      requestOpts' = if compression then requestOpts & header "Accept-Encoding" .~ ["gzip"]
                                    else requestOpts
  r <- getWith requestOpts' (getUrl service)
  pure $ r ^. responseBody
