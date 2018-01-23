MAIN=ijcai18
TEX=*.tex
LATEX=pdflatex
DOT=figures/dot/cex.pdf figures/dot/dumb-story.pdf

all : $(MAIN).pdf

figures/dot/%.pdf : figures/dot/%.dot
	dot -T pdf $< > $@

$(MAIN).pdf : $(MAIN).tex $(TEX) $(DOT)
	$(LATEX) $<
	bibtex $(MAIN)
	$(LATEX) $<
	$(LATEX) $<

.PHONY: diagrams
diagrams :
	cd diagrams ; make
	cp diagrams/*.pdf figures/

.PHONY: clean
clean:
	rm -f *.aux *.pdf *.out *.log *~ *.bbl *.blg
	rm -f _region_.tex
	rm -f figures/dot/*.pdf
	rm -rf _minted-$(MAIN)

.PHONY: full-clean
full-clean: | clean
	cd diagrams ; make full-clean
