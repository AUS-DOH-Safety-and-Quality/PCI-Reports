# PCI-Reports

This repository contains code for how the Cardiac Registry of Western Australia (CRoWA) prodcues reports.  It contains code, definitions and methods.
There is no actual data included in this repository of code.

It fulfils our commitment to being open about how the data is transformed, and used to produce reports.  

### Getting Started

The report generation process assumes that the input data is a single Comma Separated Values (CSV) file following the same structure as the submission to the National Cardiac Registry (NCR). See the `ncr_extract_template.csv` file for an example of the expected structure.

### Reproducibility in R

R is the coding language used to produce the reports.  Specifically, this report uses the `renv` package to ensure a consistent development environment.  Using this means the code works consistently on different computers. 

To load the development environment navigate to the project directory in a new R session and run:

```r
source("renv/activate.R")
```

If you see a warning that any packages are not installed, run:

```r
renv::restore()
```

This will install and load any required packages (including `renv`) for the current session. If you update the report to use new packages (or remove existing ones), you can update the 'standard' development environment for the project by running:

```r
renv::snapshot()
```

This will update the `renv.lock` file with the current state of the project and its dependencies.

#### Manually Adding Dependencies

In most cases, the `renv::snapshot()` function will automatically detect any changes to dependencies and update the `renv.lock` file accordingly. However, if you need to manually add a dependency, you can do so by adding the package to the `_add_dependencies.R` file.

This file is not loaded/sourced by the report, but simply used to declare to `renv` that a package is required.

### Quarto

This repository uses [Quarto](https://quarto.org/index.html) to generate the report. Quarto is fully backwards-compatible with R Markdown, and is bundled with RStudio.
