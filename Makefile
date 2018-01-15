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
	cp diagrams/model.pdf figures/model.pdf
	cp diagrams/mixture.pdf figures/mixture.pdf

.PHONY: clean
clean:
	rm -f *.aux *.pdf *.out *.log *~ *.bbl *.blg
	rm -rf figures/dot/*.pdf
	rm -rf _minted-$(MAIN)
