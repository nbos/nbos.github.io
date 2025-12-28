.PHONY: all dict wl overlap clean

all: dict wl overlap

dict: dict.hs
	ghc dict.hs
	rm -f dict.hi dict.o

wl: wl.hs
	ghc wl.hs
	rm -f wl.hi wl.o

overlap: overlap.hs
	ghc overlap.hs
	rm -f overlap.hi overlap.o

clean:
	rm -f dict wl overlap *.hi *.o
