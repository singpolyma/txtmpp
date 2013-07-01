module UI where

import Control.Monad (forever)
import Types

emit :: SignalToUI -> IO ()
emit = print

run :: (SignalFromUI -> IO ()) -> IO ()
run handler = forever (readLn >>= handler)
