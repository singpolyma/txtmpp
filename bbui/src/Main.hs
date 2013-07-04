module Main (main) where

import Application
import HaskadesBinding

main :: IO ()
main = do
	let jid = "REDACTED"
	let pass = "REDACTED"

	handler <- app jid pass
	haskadesRun "asset:///ui.qml" handler
	print "Quitting..."
