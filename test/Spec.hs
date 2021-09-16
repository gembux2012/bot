module Main where

import Test.Hspec

import Network.Types (Message(..), Access'(..),ErrVK(..))
import Network.Class (getAccess)
import Control.Monad.Reader (runReaderT)
--import Network.Class
--import Network.Types 


main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "acces" $ do
     runReaderT (requestAccess >>= (`shouldSatisfy` checkAuthToken)) app

checkAuthToken (Access (Access' _ _ _))  = True
checkAuthToken (ErrorVK (ErrVK _ _)) = True
checkAuthToken _ = False     