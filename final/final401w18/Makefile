default: pdf-docs

pdf-docs:final401w18.pdf

%.html: %.Rmd
	Rscript --vanilla -e "rmarkdown::render(\"$*.Rmd\",output_format=\"html_document\")"

%.pdf: %.Rmd
	Rscript --vanilla -e "rmarkdown::render(\"$*.Rmd\")"

%.html: %.md
	pandoc $*.md > $*.html

%.R: %.Rmd
	Rscript --vanilla -e "library(knitr); purl(\"$*.Rmd\",output=\"$*.R\")"

clean:
	$(RM) *.o *.so *.log *.aux *.out *.nav *.snm *.toc *.bak
	$(RM) Rplots.ps Rplots.pdf

fresh: clean
	$(RM) -r cache figure

