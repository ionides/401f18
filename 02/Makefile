default: r-scripts pdf-docs

r-scripts: notes02.R
pdf-docs: notes02.pdf

PDFLATEX = pdflatex
BIBTEX = bibtex
MAKEIDX = makeindex
RM = rm -f
CP = cp

REXE = R --vanilla
RCMD = $(REXE) CMD
RSCRIPT = Rscript --vanilla


%.pdf: export R_QPDF=qpdf
%.pdf: export R_GSCMD=gs
%.pdf: export GS_QUALITY=ebook

%.pdf: %.tex
	$(PDFLATEX) $*
	-$(BIBTEX) $*
	$(PDFLATEX) $*
	$(PDFLATEX) $*

%.so: %.c
	$(RCMD) SHLIB -o $*.so $*.c
	$(RM) $*.o

%.tex: %.Rnw
	$(RSCRIPT) -e "library(knitr); knit(\"$*.Rnw\")"

%.R: %.Rnw
	$(RSCRIPT) -e "library(knitr); purl(\"$*.Rnw\")"

%.idx: %.tex
	-$(LATEX) $*

%.ind: %.idx
	$(MAKEIDX) $*

%.pdf: %.eps
	$(EPSTOPDF) $*.eps --outfile=$*.pdf

clean:
	$(RM) *.log *.blg *.ilg *.aux *.lof *.lot *.toc *.idx
	$(RM) *.ttt *.fff *.out *.nav *.snm 
	$(RM) *.o *.so
	$(RM) *.brf
	$(RM) Rplots.ps Rplots.pdf

fresh: clean
	$(RM) *.ps *.bbl *.ind *.dvi 
	$(RM) -r cache/* figure/*
