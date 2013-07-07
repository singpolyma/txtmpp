module UI (emit) where

import Prelude ()
import BasicPrelude
import Types

emit :: (MonadIO m) => SignalToUI -> m ()
emit = liftIO . print
