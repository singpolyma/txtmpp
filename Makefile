Main: Main.hs Types.hs UI.hs
	ghc -O2 Main.hs

.PHONY: clean
clean:
	$(RM) *.o *.hi Main
