{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Network.Api where

import Data.Text.Internal.Lazy (Text)
import Data.Aeson.Types (ToJSON, FromJSON)
import GHC.Generics



data Question = Question { title :: Text
                         , isAnswered :: Bool
                         , score :: Int
                         , tags :: [Text] }
  deriving (Generic, ToJSON, FromJSON,Show, Eq)

newtype Questions = Questions [Question]
  deriving (Generic, ToJSON, Show, Eq)
  
instance Receivable Questions where
  receive = useFromJSON  