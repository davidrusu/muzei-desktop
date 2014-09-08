{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent (threadDelay)
import System.Posix.Daemonize (daemonize)
import Network.Curl.Download (openURIString)
import Network.Curl.Download.Lazy (openLazyURI)
import Data.Aeson (FromJSON, decode)
import Data.List.Split (splitOn)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.Process (shell, createProcess, readProcessWithExitCode)
import System.Exit
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

data WindowManager = Xfce | Other deriving (Show)

main :: IO ()
main = daemonize loop

loop :: IO ()
loop = do
  fetchResult <- fetchImageAndSetBackground
  threadDelay $ determineSleepTime fetchResult
  loop

determineSleepTime :: Either String String -> Int
determineSleepTime fetchResult = 
    case fetchResult of 
      (Left _)  -> secondsToMicroSeconds 1   -- try again in 1s
      (Right _) -> secondsToMicroSeconds 600 -- check for art in ten minutes
    where secondsToMicroSeconds = (*10^6)

fetchImageAndSetBackground :: IO (Either String String)
fetchImageAndSetBackground = do
  doc <- openURIString "http://muzeiapi.appspot.com/featured?cachebust=1"
  case doc of
    Left errMsg  -> return $ Left errMsg
    Right result -> jsonToImageDetails result 

jsonToImageDetails :: String -> IO (Either String String)
jsonToImageDetails json = case decode $ BL.pack json of
                            Nothing      -> return $ Left "failed to get json"
                            Just imgDtls -> saveImage $ imageUri imgDtls

saveImage :: String -> IO (Either String String)
saveImage uri = do
  homeDir <- getHomeDirectory
  let muzeiHome = homeDir ++ "/.muzei"
  createDirectoryIfMissing False muzeiHome
  
  let imageName = last $ splitOn "/" uri
  let filePath = muzeiHome ++ "/" ++ imageName
  
  downloadResult <- downloadImageIfMissing filePath uri

  case downloadResult of
    Left errMsg      -> return $ Left errMsg
    Right successMsg -> setWallpaper filePath

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

setWallpaper :: String -> IO (Either String String)
setWallpaper filePath = do
  desktopSession <- checkSession
  let bgProcess = getWallpaperProcess desktopSession filePath
  _ <- createProcess bgProcess
  return $ Right ("Set wallpaper " ++ filePath)

checkSession :: IO (WindowManager)
checkSession = do
  (exitCode, output, _) <- readProcessWithExitCode "pgrep" ["xfdesktop"] ""
  case exitCode of
    ExitSuccess   -> return Xfce
    ExitFailure _ -> return Other


getWallpaperProcess Xfce filePath  = shell ("xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitor0/image-path --set " ++ filePath)
getWallpaperProcess Other filePath = shell ("feh --bg-fill " ++ filePath)
