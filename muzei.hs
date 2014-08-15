{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.Curl.Download
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL

import GHC.Generics (Generic)

data ImageDetails = ImageDetails {
      byline     :: String, -- "Pierre-Auguste Renoir, 1881"
      detailsUri :: String,           -- "http://www.wikiart.org/en/pierre-auguste-renoir/the-luncheon-of-the-boating-party-1881-1?utm_source=Muzei&utm_campaign=Muzei"
      imageUri   :: String,           -- "http://storage.googleapis.com/muzeifeaturedart/fullres/the-luncheon-of-the-boating-party-pierre-auguste-renoir-1881.jpg"
      nextTime   :: String,           -- "2014-08-16T02:00:00+00:00"
      thumbUri   :: String,           -- "http://storage.googleapis.com/muzeifeaturedart/thumbs/the-luncheon-of-the-boating-party-pierre-auguste-renoir-1881.jpg"
      title      :: String            -- "The Luncheon of the Boating Party"
    } deriving (Show, Generic)

instance FromJSON ImageDetails
main = do
  doc <- openURIString "http://muzeiapi.appspot.com/featured?cachebust=1"
  what doc

what :: Either String String -> IO ()
what (Left errMsg) = print errMsg
what (Right result) = print $ (decode $ BL.pack result :: Maybe ImageDetails)
