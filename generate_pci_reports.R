################################################################################
## Name: generate_pci_reports.R
## Purpose: Main script to generate PCI reports
################################################################################

library(lubridate)
library(doconv)
library(qiverse.azure)
library(qiverse.powerbi)

# Set up Azure Access Tokens
tk_pbi <- qiverse.azure::get_az_tk('pbi_df')
tk_sp <- qiverse.azure::get_az_tk('sp')

# Read in WA PCI Operator List
wa_pci_operators <- qiverse.powerbi::download_dataflow_table(
  workspace_name = 'PCI Data Set',
  dataflow_name = 'ref_pci_operators',
  table_name = 'wa_pci_operators',
  access_token = tk$credentials$access_token
)

# Read pci_data_raw from dataflow
pci_data_raw <- qiverse.powerbi::download_dataflow_table(
  workspace_name = "PCI Data Set",
  dataflow_name = "4_ncr_merged",
  table_name = "ncr_combined",
  access_token = tk_pbi$credentials$access_token
)

# Generate PCI Clinician Report ------------------------------------------------
## Set parameters
period_end <- "2026-06-30"
period_start <- "2023-07-01"
period_frequency <- "quarterly"

unique_wa_pci_operators <- unique(wa_pci_operators |> dplyr::select(PCIOperatorName, PCIOperatorHE, Site))

## Loop through all operators in list
for (i in 1:nrow(unique_wa_pci_operators)) {
  # Generate parameters for the operator
  input_po_name <- unique_wa_pci_operators[i,]$PCIOperatorName
  he_number <- sub("@.*", "", unique_wa_pci_operators[i,]$PCIOperatorHE)

  file_name <- paste0(
    year(period_end), "Q", quarter(period_end),
    "_",
    he_number,
    "_",
    "pci_clinician_report.docx"
  )

  # Render the report
  rmarkdown::render(
    "pci_clinician_report/pci_clinician_report.Rmd",
    output_format = "word_document",
    output_file = paste0("../_output/", file_name),
    params = list(
      target_po = input_po_name,
      period_start = period_start,
      period_end = period_end,
      period_frequency = period_frequency
    )
  )

  # Upload to Sharepoint Site
  upload_sharepoint_file(
    src = paste0("_output/", file_name),
    site_url = "https://wahealthdept.sharepoint.com/sites/cardiovascular/",
    dest_fldr_url = paste0(
      "https://wahealthdept.sharepoint.com/:f:/r/sites/cardiovascular/individual_reports/",
      he_number, " - ", input_po_name
    ),
    token = tk_sp
  )

  # Convert docx to pdf
  doconv_to_pdf_fixed(paste0("_output/", file_name))

  # Upload to Sharepoint Site
  upload_sharepoint_file(
    src = gsub(".docx", ".pdf", paste0("_output/", file_name)),
    site_url = "https://wahealthdept.sharepoint.com/sites/cardiovascular/",
    dest_fldr_url = paste0(
      "https://wahealthdept.sharepoint.com/:f:/r/sites/cardiovascular/individual_reports/",
      he_number, " - ", input_po_name
    ),
    token = tk_sp
  )
}
