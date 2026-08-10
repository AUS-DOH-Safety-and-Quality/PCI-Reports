################################################################################
## Name: generate_pci_reports.R
## Purpose: Main script to generate PCI reports
################################################################################

# Generate PCI Clinician Report ------------------------------------------------
input_po_name <- "Target Clinician"
quarto::quarto_render(
  "pci_clinician_report/pci_clinician_report.qmd",
  output_format = "docx",
  output_file = "pci_clinician_report.docx",
  execute_params = list(
    target_po = input_po_name,
    period_start = "2023-01-01",
    period_end = "2025-12-31",
    period_frequency = "monthly"
  )
)
