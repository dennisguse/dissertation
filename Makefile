build: tex
	pdflatex 00main.tex
	bibtex 00main.aux
	makeindex %.idx
	pdflatex 00main.tex

tex: chapter07.tex chapter08.tex chapter09.tex appendix.tex
	echo "Done"

%.tex : %.Rnw
	Rscript -e "library(knitr); knit('$<')"

clean:
	-rm *.aux
	-rm *~
	-rm *.old
	-rm *.lol
	-rm *.log