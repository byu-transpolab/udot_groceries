# load R libraries here; the `include` flag in the chunk options above tells
# whether to print the results or not. Usually you don't want to print the
# library statements, or any code on the pdf.

# Main Packages ========
# I use these in every doc
library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(targets)
options(dplyr.summarise.inform = FALSE)

my_theme <- theme_bw(base_size = 12) +
  theme(
    legend.title = element_text(size = rel(1)),
    legend.text = element_text(size = rel(0.8)),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(0.8)),
    plot.title = element_text(size = rel(2))
  )
theme_set(my_theme)

# Instructions and options =========
# doesn't wrap numbers in tags latex can't handle
options(modelsummary_format_numeric_latex = "plain")
# prints missing data in tables as blank space
options(knitr.kable.NA = '') 
# tells kableExtra to not load latex table packages in the chunk output
options(kableExtra.latex.load_packages = FALSE) 
# round and format numbers that get printed in the text of the article.
inline_hook <- function(x) {
  if (is.numeric(x)) {
    format(x, digits = 3, big.mark = ",")
  } else x
}
knitr::knit_hooks$set(inline = inline_hook)
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
# options for latex-only output
if(knitr::is_latex_output()) {
  knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, sanitize = TRUE)
} 
if(knitr::pandoc_to("docx")) {
  knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
} 
