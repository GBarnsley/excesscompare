
<!-- README.md is generated from README.Rmd. Please edit that file -->

# excesscompare

This repository contains the data and code to generate this short
analysis

## Contents

The **analysis** directory contains:

-   [:file_folder: report](/analysis/report): R Markdown source document
    for manuscript. Includes code to reproduce the figures and tables
    generated by the analysis. It also has a rendered version,
    `report.docx`, suitable for reading (the code is replaced by figures
    and tables in this file)
-   [:file_folder: data](/analysis/data): Data used in the analysis.
-   [:file_folder: figures](/analysis/figures): Plots and other
    illustrations

## How to run in your browser or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping:

-   open the `.Rproj` file in RStudio
-   run `devtools::install()` to ensure you have the packages this
    analysis depends on (also listed in the [DESCRIPTION](/DESCRIPTION)
    file).
-   ensure that packages are at the correct versions with
    `renv::restore()`.
-   finally, open `analysis/report/report.Rmd` and knit to produce the
    `report.docx`, or run
    `rmarkdown::render("analysis/report/report.Rmd", params = list(gather_raw_data = FALSE, parallel_ = FALSE, generate_model_fits = FALSE))`
    in the R console

### Licenses

**Code :** See the [DESCRIPTION](DESCRIPTION) file

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.