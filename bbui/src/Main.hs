module Main (main) where

import Application
import HaskadesBinding

main :: IO ()
main = app >>= haskadesRun "asset:///ui.qml"
