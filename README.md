# Utility-Based Grocery Access in Utah

This repository contains the data-processing pipeline, statistical models, maps,
scenario analysis, and manuscript for **“Where's dinner coming from? A
utility-based investigation of food access in Utah.”** The work was completed as
part of the Equitable Food Access research project funded by the Utah Department
of Transportation (UDOT).

The study measures grocery access at the Census block-group level by combining
store characteristics, observed grocery-trip flows, and multimodal travel times.
Destination-choice models estimated for Salt Lake, Utah, and San Juan counties
are used to create a utility-based accessibility measure for communities across
Utah. The analysis then evaluates four possible interventions:

1. adding a grocery store;
2. improving an existing store;
3. improving walking and transit connections; and
4. expanding grocery delivery.

## Repository contents

- `_targets.R` defines the complete [`targets`](https://books.ropensci.org/targets/)
  workflow, from source data through model outputs and policy scenarios.
- `R/` contains functions for preparing store and Census data, calculating R5
  travel times, estimating choice models, and computing accessibility.
- `data/` contains the shareable spatial, store-audit, brand, and mode-utility
  inputs used by the pipeline.
- `sh/get_osm.sh` downloads and assembles the Utah/Four Corners OpenStreetMap
  network used for routing.
- `index.qmd` and `02_litreview.qmd` through `06_conclusion.qmd` contain the
  Quarto manuscript; `_quarto.yml` controls its Elsevier PDF output.
- `docs/imputation.qmd` documents the grocery-store attribute imputation.
- `renv.lock` records the R package environment.

Generated pipeline objects are stored in `_targets/`, routing inputs in `r5/`,
and rendered Quarto output in `_book/`. These directories are intentionally not
tracked by Git.

## Requirements

The reproducible environment is based on R 4.5.0. To run the full analysis you
will also need:

- [`renv`](https://rstudio.github.io/renv/) and the R packages recorded in
  `renv.lock`;
- [Quarto](https://quarto.org/) and a LaTeX distribution to render the paper;
- Java with enough memory for `r5r` (the pipeline requests a 14 GB Java heap);
- Bash, `wget`, and the [`osmium`](https://osmcode.org/osmium-tool/) command-line
  tool; and
- internet access for Census/ACS data, population-weighted centroids, UTA GTFS,
  and OpenStreetMap extracts. A Census API key is recommended for `tidycensus`
  requests.

The routing calculation is computationally and memory intensive. The committed
configuration calculates car, walk, and transit times for all Utah block groups
and grocery destinations, so a full run can take substantially longer than a
typical R analysis.

## Restricted input data

Three StreetLight trip-flow files used to estimate the destination-choice
models are not included in this repository:

```text
data/streetlight/streetlight_groceries_saltlake.csv
data/streetlight/streetlight_groceries_utah.csv
data/streetlight/streetlight_groceries_sanjuan.csv
```

Place authorized copies at those paths before running the full pipeline. The
source files are commercial location-based-services data and cannot be
reconstructed by the download scripts in this repository.

## Reproducing the analysis

Clone the repository, open the project directory, and restore its R environment:

```r
install.packages("renv")
renv::restore()
```

Confirm that Java, `wget`, and `osmium` are available, add the restricted
StreetLight inputs described above, and run the pipeline from the repository
root:

```sh
Rscript run.R
```

Alternatively, `run.sh` starts the same pipeline as a persistent, low-priority
background R process. The `targets` workflow downloads the dated OpenStreetMap
extracts and UTA GTFS feed when needed, retrieves Census data, builds multimodal
travel-time matrices, estimates the models, and evaluates the accessibility
scenarios. Subsequent runs skip targets that are already up to date.

Useful commands for inspecting or selectively rebuilding the workflow include:

```r
targets::tar_visnetwork()
targets::tar_make(names = "access")
targets::tar_read(access)
```

## Rendering the manuscript

After the required pipeline targets have been built, render the PDF with:

```sh
quarto render --to elsevier-pdf
```

This produces the book PDF under `_book/` and retains the intermediate TeX as
configured in `_quarto.yml`. The repository's `makefile` also supports
`make diff`, which uses `latexdiff` and `pdflatex` to compare the rendered
manuscript with `submitted.tex`.

## Reproducibility notes

- The OSM script uses dated January 2024 Geofabrik extracts, and the routing
  departure time is set to May 9, 2023. The GTFS download URL is not versioned,
  so deleting `r5/gtfs.zip` may retrieve a newer UTA schedule and change results.
- Several data-acquisition steps require network access, and map tiles used in
  manuscript figures are also fetched during rendering.
- No open-source license is currently included. Unless one is added, the code
  and data remain subject to the rights of their respective owners.
