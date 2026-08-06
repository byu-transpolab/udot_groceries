SRCS := $(wildcard *.qmd)

.PHONY: docx pdf food_access.docx

docx: _book/food_access.docx

food_access.docx: _book/food_access.docx

pdf: food_access.tex

diff: diff.pdf

diff.pdf: diff.tex
	pdflatex $<

diff.tex: food_access.tex
	latexdiff submitted.tex $< > $@

food_access.tex: $(SRCS)
	quarto render --to elsevier-pdf

_book/food_access.docx: $(SRCS) jtlu_template.docx _quarto.yml
	quarto render --to docx
