Main: Main.hs Types.hs Application.hs Nick.hs Ping.hs Disco.hs UI.hs Accounts.hs
	hlint *.hs
	ghc -XHaskell98 -O2 -Wall -fno-warn-name-shadowing -DUIMODULE=UI -DAPPNAME=\"txtmpp\" Main.hs

.PHONY: clean
clean:
	$(RM) *.o *.hi Main
