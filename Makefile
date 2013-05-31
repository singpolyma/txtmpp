Main: Main.hs Types.hs UI.hs Nick.hs Ping.hs Disco.hs
	hlint *.hs
	ghc -XHaskell98 -O2 -Wall -fno-warn-name-shadowing Main.hs

.PHONY: clean
clean:
	$(RM) *.o *.hi Main
