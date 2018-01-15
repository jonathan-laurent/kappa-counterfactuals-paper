DIAGS=model cpats
SVGS=$(DIAGS:%=%.svg)
PDFS=$(DIAGS:%=%.pdf)

all: $(PDFS)

%.pdf: %.svg
	rsvg-convert -f pdf $< > $@

# Add +RTS -xc to dump the stack trace.
$(SVGS): Main
	./Main

Main: Main.hs
	cabal exec -- ghc -prof -fprof-auto -fprof-cafs --make $< -o $@

clean:
	rm -f Main
	rm -f *~ *.hi *.ho *.o *.svg

full-clean: clean
	rm -f *.pdf
