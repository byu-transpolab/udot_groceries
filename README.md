# UDOT Groceries Access Map

As part of the Equitable Access to Nutrition research project funded by UDOT, 
we have constructed an interactive map of block-level access to grocery stores.

This repository contains the code to construct the map and build a Shiny
application to show the map on a webserver.


## Environments

The repository is built using `renv` and `targets` to aid reproducibility. 
Part of the targets stream is a shell script calling `wget` and `osmium`. You
must therefore build this script on a computer with `bash` and those two commands
in the path.