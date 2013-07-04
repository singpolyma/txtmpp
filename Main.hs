module Main (main) where

import Control.Monad (forever)
import System.Environment (getArgs)

import Application

main :: IO ()
main = do
	[jid, pass] <- getArgs
	handler <- app jid pass
	forever (readLn >>= handler)
