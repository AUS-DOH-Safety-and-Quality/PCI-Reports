# Cardiac Registry of Western Australia (CRoWA) PCI Clinician Reports

Automated generation of clinician level reports for Percutaneous Intervention (PCI) in the Cardiac Registry of Western Australia, maintained by the Healthcare Quality Intelligence Unit (HQIU). This code can also be applied to the standard National Cardiac Registry submissions for PCI.

## Overview

This project processes raw PCI data and generates Word document reports for each clinician, summarising and visualizing latest quarterly data in patient characteristics and clinical quality indicators.

It uses a **Two-Stage Pipeline**:

1.  **Data Processing** (`prepare_pci_data` from the `utilities.R` file): Cleans raw data, applies strict clinical rules (e.g., STEMI logic), and acts as the "Source of Truth".
2.  **Report Generation** (`pci_clinician_report.Rmd`): Generates a customised clinician report using the processed data.

------------------------------------------------------------------------

## 1. Quick Start

### Prerequisites

- R (Version 4.0+)
- Required R Packages are loaded via `renv`
- Local Microsoft Word Installation

### Step 1: Open .Rproj file in RStudio

Navigate to the folder where you have cloned the Git repository, and open the `PCI-Reports.Rproj` file. Your working directory will be automatically set to the root folder of the `.Rproj` file. If it is not, please run the following command, replacing the directory path with your local path.

``` r
setwd("C:/GitHub/PCI-Reports")
```

### Step 2: Open the parent file in the root directory

Open the `generate_pci_reports.R` script from the root folder in your RStudio IDE.

### Step 3: Run Data Load and Pre-Processing from `generate_pci_reports.R`

Run the first section of code, "Load Libraries and Data".

### Step 4: Run PCI Clinician Report

Run the second section of the code, making sure that you update the `input_period_start` and `input_period_end` dates to the period for which you want to cover. You can set the `input_period_frequency` value to either `quarterly` or `monthly`. This section of code will complete the following steps for each clinician listed in the processed data:

1.  Generate a Word document report customised to the individual clinician
2.  **(Requires Local Microsoft Word Installation)** Use the Microsoft Word SVG rendering engine to ensure that a `.png` fallback is created for each `.svg` image, as `.svg` images are unable to be viewed on Microsoft 365 Word Online.
3.  **(Requires Local Microsoft Word Installation)** Convert the `.docx` file to `.pdf` using Microsoft Word.
4.  **(Requires SharePoint site access)** Upload both the `.docx` and `.pdf` outputs to a nominated Microsoft SharePoint folder.

------------------------------------------------------------------------

## 2. Contact

**Department**: WA Health, HQIU

**Author**: [safetyandquality\@health.wa.gov.au](mailto:safetyandquality@health.wa.gov.au)
