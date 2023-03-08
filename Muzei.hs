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

data ArtMetaData = ArtMetaData {
      attribution :: String, -- "wikiart.org"
      byline      :: String, -- "Author, YEAR"
      detailsUri  :: String, -- link to wikiart
      imageUri    :: String, -- link to image
      nextTime    :: String, -- time stamp
      thumbUri    :: String, -- link to thumbnail image
      title       :: String  -- painting title
    } deriving (Show, Generic)

instance FromJSON ArtMetaData

data WindowManager = Xfce | Other

main :: IO ()
main = daemonize loop

loop :: IO ()
loop = do
  updateResult <- updateWallpaper
  threadDelay $ calcSleepTime updateResult
  loop

calcSleepTime :: Either String String -> Int
calcSleepTime (Left _)  = toMicro 1   -- Maybe no network connection, try again in 1s
calcSleepTime (Right _) = toMicro 600 -- check for art in ten minutes

toMicro :: (Num a) => a -> a
toMicro = (*10^6)

updateWallpaper :: IO (Either String String)
updateWallpaper = do
  result <- openURIString "https://muzeiapi.appspot.com/featured?cachebust=1"
  case result >>= toArtMetaData of
    Left err  -> return $ Left err
    Right art -> saveImage art

toArtMetaData :: String -> Either String ArtMetaData
toArtMetaData json = case decode $ BL.pack json of
                       Nothing          -> Left $ "Not valid json: " ++ json
                       Just artMetaData -> Right artMetaData

muzeiHomeDir :: IO (String)
muzeiHomeDir = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/.muzei"

saveImage :: ArtMetaData -> IO (Either String String)
saveImage artMetaData = do
  muzeiHome <- muzeiHomeDir
  createDirectoryIfMissing False muzeiHome
  
  let uri = imageUri artMetaData
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
  _ <- createProcess $ shell $ wallpaperCommand desktopSession filePath
  return $ Right ("Set wallpaper " ++ filePath)

checkSession :: IO (WindowManager)
checkSession = do
  (exitCode, output, _) <- readProcessWithExitCode "pgrep" ["xfdesktop"] ""
  case exitCode of
    ExitSuccess   -> return Xfce
    ExitFailure _ -> return Other

wallpaperCommand :: WindowManager -> String -> String
wallpaperCommand Xfce filePath  = "xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitor0/image-path --set " ++ filePath
wallpaperCommand Other filePath = "feh --bg-max " ++ filePath
