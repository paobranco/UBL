#
# Makefile for producing a report
#
# Jul, 2009 - Luis Torgo (based on stuff from others)
#

R_PRG = R
export R_HOME = /usr/lib/R
#SWV   = ${R_HOME}/bin/Sweave
#STG   = ${R_HOME}/bin/Stangle
TPUTS = ${TEXINPUTS}

# The sources of the report (tex, Rnw and other files (e.g. bib, idx))
RNW_CMPS = performanceEstimation
OTHER =  
ROOT = performanceEstimation

# The name of the report to produce
all: performanceEstimation.pdf

code: $(RNW_CMPS:=.R)

clean:
	rm -f  *.log *.dvi *~  

# On what does the report depends?
performanceEstimation.pdf:  $(RNW_CMPS:=.tex) ${OTHER}
	TEXINPUTS=${TPUTS} pdflatex $<
	bibtex ${ROOT}
	if [ -e ${ROOT}.idx ]; \
	then makeindex ${ROOT}; \
	fi
	TEXINPUTS=${TPUTS} pdflatex $<
	TEXINPUTS=${TPUTS} pdflatex $<

	rm *.log *.aux
#	mv *.aux  $(dir $<)

# How to build the tex files from the Rnw (Sweave) files
%.tex: %.Rnw
#	${SWV} $<
	echo "library(knitr);  knit('$<')" | ${R_PRG} --no-save --vanilla


# How to build the R code files from the Rnw (Sweave) files
%.R: %.Rnw
#	${STG} $<
	echo "library(knitr); knit('$<',tangle=TRUE)" | ${R_PRG} --no-save --vanilla

%.bib: %.aux
	TEXINPUTS=${TPUTS} pdflatex $<
	bibtex $<

%.aux:
	TEXINPUTS=${TPUTS} pdflatex $<
	bibtex $<

%.idx:
	TEXINPUTS=${TPUTS} pdflatex $<
	makeindex $<




