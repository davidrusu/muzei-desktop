{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.Curl.Download (openURIString)
import Network.Curl.Download.Lazy (openLazyURI)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.List.Split (splitOn)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.Process (shell, createProcess)
import qualified Data.ByteString.Lazy.Char8 as BL 

import GHC.Generics (Generic)

data ImageDetails = ImageDetails {
      byline     :: String, -- "Pierre-Auguste Renoir, 1881"
      detailsUri :: String, -- "http://www.wikiart.org/en/pierre-auguste-renoir/the-luncheon-of-the-boating-party-1881-1?utm_source=Muzei&utm_campaign=Muzei"
      imageUri   :: String, -- "http://storage.googleapis.com/muzeifeaturedart/fullres/the-luncheon-of-the-boating-party-pierre-auguste-renoir-1881.jpg"
      nextTime   :: String, -- "2014-08-16T02:00:00+00:00"
      thumbUri   :: String, -- "http://storage.googleapis.com/muzeifeaturedart/thumbs/the-luncheon-of-the-boating-party-pierre-auguste-renoir-1881.jpg"
      title      :: String  -- "The Luncheon of the Boating Party"
    } deriving (Show, Generic)

instance FromJSON ImageDetails
main = do
  doc <- openURIString "http://muzeiapi.appspot.com/featured?cachebust=1"
  let result = getResult doc
  setWallpaper result

getResult :: Either String a -> a
getResult (Left errMsg) = error errMsg
getResult (Right result) = result

setWallpaper :: String -> IO ()
setWallpaper result = saveImage $ imageUri $ jsonToImageDetails result

jsonToImageDetails :: String -> ImageDetails
jsonToImageDetails json = case decode $ BL.pack json of
                            Nothing -> error "failed to get json"
                            Just imgDts -> imgDts

saveImage :: String -> IO ()
saveImage uri = do
  let imageName = last $ splitOn "/" uri
  homeDir <- getHomeDirectory
  let muzeiHome = homeDir ++ "/.muzei"

  createDirectoryIfMissing False muzeiHome
  
  let filePath = muzeiHome ++ "/" ++ imageName
  
  downloadImageIfMissing filePath uri

  let bgProcess = shell ("feh --bg-fill " ++ filePath)
  _ <- createProcess bgProcess
  return ()

downloadImageIfMissing :: String -> String -> IO ()
downloadImageIfMissing filePath uri = do
  imageExists <- doesFileExist filePath
  case imageExists of
    True  -> return ()
    False -> writeImage filePath uri
  
writeImage :: String -> String-> IO ()
writeImage filePath uri = do
  result <- openLazyURI uri
  let img = getResult result
  BL.writeFile filePath img
