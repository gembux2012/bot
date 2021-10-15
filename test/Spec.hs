{-# LANGUAGE OverlappingInstances #-}

module Main where

import Test.Hspec

import Network.Types (Message(..), Access'(..),ErrVK(..))
import Network.Class (getAccess)
import Control.Monad.Reader (runReaderT, runReader)
import Logger.Types (VKOpts(..), button, Config)

main :: IO ()
main = hspec spec 
spec :: Spec
spec =  do
  describe "bot VK logick" $ do
    it "acces" $ do
    runReader getAccess  opts >>= (`shouldSatisfy` checkAuthToken)
 where opts = VKOpts { button =  "jkh"} 

checkAuthToken (Access (Access' _ _ _))  = True
checkAuthToken (ErrorVK (ErrVK _ _)) = True
checkAuthToken _ = False

