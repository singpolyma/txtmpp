module Main (main) where

import Types
import HaskadesBinding

main :: IO ()
main = haskadesRun "asset:///ui.qml" Slots { }
