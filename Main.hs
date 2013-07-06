module Main (main) where

import Control.Monad (forever)
import System.Environment (getArgs)
import qualified Data.Text as T

import Application
import Types

main :: IO ()
main = do
	[jid, pass] <- getArgs
	handler <- app
	handler Ready
	handler (UpdateAccount (T.pack jid) (T.pack pass))
	forever (readLn >>= handler)
