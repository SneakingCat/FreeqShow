all:
	ghc --make Main.hs -O2 -o freeqshow

clean:
	rm -f freeqshow *~ *.o *.hi js/*~
