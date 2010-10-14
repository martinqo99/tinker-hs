all: randompick

randompick: randompick.hs
	ghc -fvia-C -O2 -optc-O3 --make $<

clean:
	rm -f randompick *.hi *.o 
