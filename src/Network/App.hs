{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
--{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

module Network.App where

--import qualified Data.Aeson.Lens as L

--import Data.Aeson
-- ( AsPrimitive, AsValue, _Array, _Integer, _JSON, _Object, _String, key, values, values, values )

--import Data.Aeson.Encode

--import Control.Monad.Error

--import Network.ErrorTypes

-- import Network.ErrorTypes (ErrorVK)

--import Network.Types (Message (..))

import Config (Config)
import qualified Control.Concurrent.Lifted as CCL
import Control.Concurrent.Lifted (MVar)
import Control.Lens (_Wrapped)
import Control.Lens.Combinators (preview)
import Control.Lens.Fold ((^?))
import qualified Control.Monad as CM
import Control.Monad (when)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson (decodeStrict, eitherDecodeStrict, encode, parseJSON)
import qualified Data.Aeson.Lens as AL
import Data.Aeson.Lens (values)
import Data.Aeson.Types ((.:), FromJSON, Parser, Value, defaultOptions, fieldLabelModifier, genericParseJSON, parseMaybe, withObject)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.Map.Internal (Map)
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import GHC.Generics
import Logger.Class (Log (..))
import Network.HTTP.Conduit (http)
import Network.HTTP.Simple
import Network.Types
import Network.Types
import Network.URI
import qualified MutableList as ML
import Control.Monad as CM

idGroup = "202652768" :: BS8.ByteString

keyGroup = "13b47b20e6324c0dcc288baec4a318ee359bd16dc2f57c7bf215755241faf955de12b014a780ac7f9e455"

keyUser = "454751226"

versionService = "5.103" :: BS8.ByteString

uriVK = "api.vk.com"

newtype SecKey = SecKey {secKey :: String}

getKeyAccessUrl =
  Url
    { -- requestHost = uriVK,
      requestMethod = "GET",
      requestPath = "https://" <> uriVK <> "/method/groups.getLongPollServer",
      requestQS = [("access_token", Just keyGroup), ("group_id", Just idGroup), ("v", Just versionService)]
    }

getMessageUrl :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> Url
getMessageUrl k server ts =
  Url
    { -- requestHost = BS8.pack   server     ,
      requestMethod = "GET",
      requestPath = server,
      requestQS = [("act", Just "a_check"), ("key", Just k), ("ts", Just ts), ("wait", Just "25")]
    }

sendMessageUrl :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> Url
sendMessageUrl user_id message buttons =
  Url
    { requestMethod = "GET",
      requestPath = "https://" <> uriVK <> "/method/messages.send",
      requestQS =
        [ ("user_id", Just user_id),
          ("random_id", Just "0"),
          ("peer_id", Just idGroup),
          ("message", Just message),
          ("dont_parse_links", Just "0"),
          ("disable_mentions", Just "0"),
          ("intent", Just "default"),
          -- ("title", Just ""),
          ("keyboard", Just buttons),
          ("access_token", Just keyGroup),
          ("v", Just versionService)
        ]
    }

repeatButtons =
  Buttons
    { one_time = False,
      buttons =
        [ [ Button
              { action =
                  Action
                    { type' = "text",
                      payload = "{\"button\": \"1\"}",
                      label = "1"
                    },
                color = "primary"
              },
            Button
              { action =
                  Action
                    { type' = "text",
                      payload = "{\"button\": \"2\"}",
                      label = "2"
                    },
                color = "primary"
              },
            Button
              { action =
                  Action
                    { type' = "text",
                      payload = "{\"button\": \"3\"}",
                      label = "3"
                    },
                color = "primary"
              }
          ]
        ]
    }

emptyButtons =
  Buttons
    { one_time = False,
      buttons = []
    }

requestVK ::
  MonadIO m =>
  MonadThrow m =>
  Config ->
  Url ->
  m Message
requestVK listUser Url {..} = do
  request' <- parseRequest $ BS8.unpack requestPath
  let request =
        setRequestMethod requestMethod $
          setRequestQueryString
            requestQS
            request'
  response <- httpBS request
  let status = getResponseStatusCode response
  case status of
    200 -> do
      let body = getResponseBody response
      case decodeStrict . getResponseBody $ response :: Maybe Message of
        Just mess -> do
          pure mess
        Nothing -> do
          pure $ ErrorVK $ ErrVK "0" "Invalid Json"
    _ -> do
      pure $ ErrorVK $ ErrVK "1" "no 200"

answerCreator :: 
  Integer -> String -> Int -> MVar (Map Integer Int) -> 
  IO (BS8.ByteString, BS8.ByteString)
answerCreator _ ['\\', 'r', 'e', 'p', 'e', 'a', 't'] _ _ =  
    return
      ( BS8.pack "how many times will you repeat ?",
        LBS.toStrict $ encode repeatButtons
      )

answerCreator id str btn l   
  | btn /= 0 = do
     _ <- addList id   btn l   
     return 
      ( BS8.pack $ " ОК, i will repeat " <> show btn <> " time(s), try",
        LBS.toStrict $ encode emptyButtons
      )
  | otherwise = do
    repeat <- findInList l id
    case repeat of 
     Just r -> do  
      let s =concat[str <>", " | r <- [0..r-1]]
      return  (encodeUtf8 . T.pack $ s, LBS.toStrict $ encode emptyButtons)
     Nothing -> 
      return  (encodeUtf8 . T.pack $ str, LBS.toStrict $ encode emptyButtons)

 --lookup (id ::Int  l >>= \(Just user) -> update user btn >>= return()

addList :: Integer -> Int -> MVar (Map Integer Int) -> IO ()
addList id btn l = do  
  t <- ML.lookup l id  
  case t  of
   Just t -> 
    ML.update l id  btn
   Nothing -> do
     ML.insert l id btn 
     return()    

findInList :: MVar (Map Integer Int) -> Integer -> IO (Maybe Int)  
findInList l id  = ML.lookup l id  >>= \repeat -> return repeat   
     