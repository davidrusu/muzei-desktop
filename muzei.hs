import Network.Curl.Download
import Text.JSON

main = do
  doc <- openURIString "http://muzeiapi.appspot.com/featured?cachebust=1"
  let json = getJsonResult doc
  putStrLn $ show json

getJsonResult :: (JSON a) => Either String String -> Result a
getJsonResult Left msg = Error msg
getJsonResult Right js = decode js
