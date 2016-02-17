{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Bing.Types where

import           Control.Lens          ((.~))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.Default          (Default, def)
import           Data.Monoid           ((<>))
import qualified Data.Text             as Text
import           Network.Wreq          (param)
import qualified Network.Wreq          as Wreq
import           TextShow              (TextShow, showb, showt)

newtype AccountKey = AccountKey { unAccountKey :: ByteString }

instance Read AccountKey where
  readsPrec _ input = [(AccountKey . Char8.pack $ input, "")]

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
 deriving (Read, Show)

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
  | Composite [Service]
  deriving (Read, Show)

instance TextShow Service where
  showb Web                = "Web"
  showb Image              = "Image"
  showb Video              = "Video"
  showb News               = "News"
  showb SpellingSuggestion = "Spell"
  showb RelatedSearch      = "RelatedSearch"
  showb _                  = error "only Web, Image, Video, News, Spell, and RelatedSearch allowed in composite search"

getCompositeParam :: [Service] -> (Wreq.Options -> Wreq.Options)
getCompositeParam services = param "Sources" .~ [strs]
  where strs = Text.intercalate "+" $ map showt services

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

getHeaders :: Service -> [Wreq.Options -> Wreq.Options]
getHeaders (Composite services) = [getCompositeParam services]
getHeaders _ = []
