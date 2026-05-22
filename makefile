SRCS := $(wildcard *.qmd)

diff: diff.pdf

diff.pdf: diff.tex
	pdflatex $<

diff.tex: food_access.tex
	latexdiff submitted.tex $< > $@

food_access.tex: $(SRCS)
	quarto render --to elsevier-pdf