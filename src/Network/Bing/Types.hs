{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.Bing.Types where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Default           (Default, def)
import           Data.Monoid            ((<>))
import           TextShow               (TextShow, showb)

newtype AccountKey = AccountKey { unAccountKey :: ByteString }

makeAccountKey :: ByteString -> AccountKey
makeAccountKey str = AccountKey . Base64.encode $ str <> ":" <> str

data Options = Options
  { optsKey         :: AccountKey
  , optsFormat      :: Format
  , optsService     :: Service
  , optsTop         :: Int
  , optsSkip        :: Int
  , optsCompression :: Bool
  }

instance Default Options where
  def = Options { optsKey     = AccountKey ""
                , optsFormat  = JSON
                , optsService = WebOnly
                , optsTop     = 10
                , optsSkip    = 0
                , optsCompression = False
                }

data Format
 = JSON
 | XML

instance TextShow Format where
  showb JSON = "json"
  showb XML  = "atom"

data Service
  = WebOnly
  | Web
  | Image
  | Video
  | News
  | SpellingSuggestion
  | RelatedSearch
  | Composite [CompositeServices]

data CompositeServices
  = CompWeb
  | CompImage
  | CompVideo
  | CompNews
  | CompSpell
  | CompRelatedSearch

getUrl :: Service -> String
getUrl = \case
    WebOnly            -> baseUrl <> "SearchWeb/Web"
    Web                -> baseUrl <> searchStr <> "Web"
    Image              -> baseUrl <> searchStr <> "Image"
    Video              -> baseUrl <> searchStr <> "Video"
    News               -> baseUrl <> searchStr <> "News"
    SpellingSuggestion -> baseUrl <> searchStr <> "SpellingSuggestion"
    RelatedSearch      -> baseUrl <> searchStr <> "RelatedSearch"
    Composite _        -> baseUrl <> searchStr <> "Composite"
  where
    baseUrl = "https://api.datamarket.azure.com/Bing/"
    searchStr = "Search/"

getHeaders :: Service -> String
getHeaders (Composite services) = undefined
getHeaders _ = []