build: tex
	pdflatex 00main.tex
	biber 00main
#	makeindex 00main.idx
	pdflatex 00main.tex

tex: 00main.tex chapter05.tex chapter07.tex chapter08.tex chapter09.tex appendix.tex
	echo "Done"

%.Rnw : R_eval.R R_plot.R data.csv data_minimal.csv

%.tex : %.Rnw
	Rscript -e "library(knitr); knit('$<')" || (rm $@ && exit -1)

update :
	cp ../cscu-tools_svn/#r-evaluation/*.csv ./data/

clean:
	-rm *.bak
	-rm *.aux
	-rm *~
	-rm *.old
	-rm *.lol
	-rm *.log
	-rm *.bbl
	rm 00main.tex chapter05.tex chapter07.tex chapter08.tex chapter09.tex appendix.tex

copydata:
	cp ~/hack/cscu-tools_svn/#r-evaluation/data_minimal.csv ./data_minimal.csv
	cp ~/hack/cscu-tools_svn/#r-evaluation/data.csv ./data.csv
	cp ~/hack/cscu-tools_svn/#r-evaluation/data_minimal.csv ./data/data_minimal.csv
	cp ~/hack/cscu-tools_svn/#r-evaluation/data.csv ./data/data.csv

