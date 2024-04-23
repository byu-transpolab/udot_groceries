SRCS := $(wildcard *.qmd)

diff: diff.pdf

diff.pdf: diff.tex
	pdflatex $<

diff.tex: Where-s-dinner-coming-from--A-utility-based-investigation-of-access-to-nutrition-in-Utah..tex
	latexdiff submitted.tex $< > $@

Where-s-dinner-coming-from--A-utility-based-investigation-of-access-to-nutrition-in-Utah..tex: $(SRCS)
	quarto render --to elsevier-pdf