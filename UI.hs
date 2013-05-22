module UI where

import Control.Monad (forever)
import Types

emit :: OutSignal -> IO ()
emit = print

run :: (InSignal -> IO ()) -> IO ()
run handler = forever (readLn >>= handler)
