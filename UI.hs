module UI (emit) where

import Types

emit :: SignalToUI -> IO ()
emit = print
