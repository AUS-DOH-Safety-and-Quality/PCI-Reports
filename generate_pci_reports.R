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

period_end <- "2025-12-31"
period_start <- "2023-01-01"
period_frequency <- "quarterly"

unique_wa_pci_operators <- unique(wa_pci_operators |> dplyr::select(PCIOperatorName, PCIOperatorHE))

## Loop through all operators in list
for (i in 1:nrow(unique_wa_pci_operators)) {
  input_po_name <- unique_wa_pci_operators[i]$PCIOperatorName
  he_number <- sub("@.*", "", unique_wa_pci_operators[i]$PCIOperatorHE)
  rmarkdown::render(
    "pci_clinician_report/pci_clinician_report.Rmd",
    output_format = "word_document",
    output_file = paste0("../_output/", format.Date(period_end, "%Y%m%d"), "_",
                         he_number, "_", "pci_clinician_report.docx"),
    params = list(
      target_po = input_po_name,
      period_start = period_start,
      period_end = period_end,
      period_frequency = period_frequency
    )
  )
}

