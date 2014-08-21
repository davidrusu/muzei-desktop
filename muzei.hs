{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import System.Posix.Daemonize
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

main = daemonize loop
-- main = serviced CreateDaemon {
--          privilegedAction = return (),
--          program = loop,
--          name = Nothing,
--          user = Nothing,
--          group = Nothing,
--          syslogOptions = [],
--          pidfileDirectory = Nothing,
--          killWait = Just 4
--        }

loop = do
  --print "starting loop"
  appendFile "/tmp/yo" "starting loop\n"
  fetchResult <- fetchImageAndSetBackground
  appendFile "/tmp/yo" ((show fetchResult) ++ "\n")
  let sleepTime = case fetchResult of
                    Left errMsg   -> 10^7   -- 10 second
                    Right success -> 3600 * 10^6 -- 1 hour

  threadDelay sleepTime
  loop

fetchImageAndSetBackground :: IO (Either String String)
fetchImageAndSetBackground = do
  doc <- openURIString "http://muzeiapi.appspot.com/featured?cachebust=1"
  
  case doc of
    Left errMsg  -> return $ Left errMsg
    Right result -> case jsonToImageDetails result of 
                               Left errMsg   -> return $ Left errMsg
                               Right imgDtls -> saveImage $ imageUri imgDtls

getResult :: Either String a -> a
getResult (Left errMsg) = error errMsg
getResult (Right result) = result

jsonToImageDetails :: String -> Either String ImageDetails
jsonToImageDetails json = case decode $ BL.pack json of
                            Nothing      -> Left "failed to get json"
                            Just imgDtls -> Right imgDtls

saveImage :: String -> IO (Either String String)
saveImage uri = do
  let imageName = last $ splitOn "/" uri
  homeDir <- getHomeDirectory
  let muzeiHome = homeDir ++ "/.muzei"

  createDirectoryIfMissing False muzeiHome
  
  let filePath = muzeiHome ++ "/" ++ imageName
  
  downloadResult <- downloadImageIfMissing filePath uri

  case downloadResult of
    Left errMsg      -> return $ Left errMsg
    Right successMsg -> do
                        setWallpaper filePath
                        return $ Right ("Set wallpaper: "++filePath)

downloadImageIfMissing :: String -> String -> IO (Either String String)
downloadImageIfMissing filePath uri = do
  imageExists <- doesFileExist filePath
  case imageExists of
    True  -> return $ Right "Image already existes"
    False -> writeImage filePath uri
  
writeImage :: String -> String-> IO (Either String String)
writeImage filePath uri = do
  result <- openLazyURI uri
  case result of
    Left errMsg -> return $ Left errMsg
    Right img   -> do
                BL.writeFile filePath img
                return $ Right "Successfully wrote image"

setWallpaper :: String -> IO ()
setWallpaper filePath = do
  let bgProcess = shell ("feh --bg-max " ++ filePath)
  _ <- createProcess bgProcess
  return ()
