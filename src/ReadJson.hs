module ReadJson where


import Data.Aeson.Types (Object, parseMaybe)
import qualified Data.ByteString as B
import Control.Exception.Base (SomeException, try)
import Data.Aeson (decodeStrict)
import System.Environment.Blank (getArgs)

{--
jsonFromFile handler  path = do
                     json <- try (B.readFile $ head path) :: IO (Either SomeException B.ByteString)
                     case json of
                       Left e -> putStrLn $ (show e )
                       Right context-> do
                         let result = decodeStrict context :: Maybe Object
                         case result of
                             Nothing     -> putStrLn $ "Warning! Invalid config fail" 
                             Just jsonObject -> jsonObject

--getBlogRepoURL :: Object -> String
getValFromJson info =
    case parseMaybe extractFromJson info of
        Nothing  -> ""
        Just val -> val
    
extractFromJson  = \info    -> info .: "social"
                             >>=
                             \socialInfo -> socialInfo .: "GitHub"
                             >>=
                             \gitHubInfo -> gitHubInfo .: "blog"
--}                             