################################################################################
## Name: generate_pci_reports.R
## Purpose: Main script to generate PCI reports
################################################################################

# Run the script to generate fixed primary operator field
source('utilities/po_temp_fix.R')

# Run the script for WA PCI operator list
## This script is not to be added to the repo, it will be stored elsewhere to be
## read into the R environment in the future. Temporarily used here.
source('utilities/wa_pci_operators.R')

# Generate PCI Clinician Report ------------------------------------------------
input_po_name <- "Target Clinician"
rmarkdown::render(
  "pci_clinician_report/pci_clinician_report.Rmd",
  output_format = "word_document",
  output_file = "../_output/pci_clinician_report.docx",
  params = list(
    target_po = input_po_name,
    period_start = "2023-01-01",
    period_end = "2025-12-31",
    period_frequency = "quarterly"
  )
)
