################################################################################
## Name: generate_pci_reports.R
## Purpose: Main script to generate PCI reports
################################################################################

# 1. Load Libraries and Data ------------------------------------------------------
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
  access_token = tk_pbi$credentials$access_token
)

# Read pci_data_raw from dataflow
pci_data_raw <- qiverse.powerbi::download_dataflow_table(
  workspace_name = "PCI Data Set",
  dataflow_name = "4_ncr_merged",
  table_name = "ncr_combined",
  access_token = tk_pbi$credentials$access_token
)

# Indicator metadata
indicators_file <- file.path("reference_files", "indicators.csv")
ind_desc_df <- read.csv(indicators_file)

## Utilities
source("utilities/utilities.R")
pci_data_clean <- prepare_pci_data(pci_data_raw)
pci_indicators_summary <- summarise_pci_indicators(pci_data_clean, ind_desc_df)
write.csv(pci_data_clean, "_output/pci_data_clean.csv", row.names = FALSE)
write.csv(pci_indicators_summary, "_output/pci_indicators_summary.csv", row.names = FALSE)

# 2. Generate PCI Clinician Report ------------------------------------------------
## Set parameters
input_period_start <- "2023-07-01"
input_period_end <- "2026-06-30"
input_period_frequency <- "quarterly"

unique_wa_pci_operators <- unique(wa_pci_operators |> dplyr::select(PCIOperatorName, PCIOperatorHE, Site))

## Loop through all operators in list
for (i in 1:nrow(unique_wa_pci_operators)) {
  # Generate parameters for the operator
  input_po_name <- unique_wa_pci_operators[i,]$PCIOperatorName
  he_number <- sub("@.*", "", unique_wa_pci_operators[i,]$PCIOperatorHE)

  file_name <- paste0(
    year(input_period_end), "Q", quarter(input_period_end),
    "_",
    he_number,
    "_",
    "pci_clinician_report.docx"
  )

  # Render the report
  rmarkdown::render(
    "pci_clinician_report/pci_clinician_report.Rmd",
    output_format = "word_document",
    output_file = file_name,
    output_dir = "_output",
    params = list(
      target_po = input_po_name,
      period_start = input_period_start,
      period_end = input_period_end,
      period_frequency = input_period_frequency
    )
  )

  # Open, update and save the report in word to create PNG fallback using native Microsoft Word SVG rendering
  doconv::docx_update(paste0("_output/", file_name))

  # Upload to Sharepoint Site
  qiverse.sharepoint::upload_sharepoint_file(
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
  qiverse.sharepoint::upload_sharepoint_file(
    src = gsub(".docx", ".pdf", paste0("_output/", file_name)),
    site_url = "https://wahealthdept.sharepoint.com/sites/cardiovascular/",
    dest_fldr_url = paste0(
      "https://wahealthdept.sharepoint.com/:f:/r/sites/cardiovascular/individual_reports/",
      he_number, " - ", input_po_name
    ),
    token = tk_sp
  )
}

# 3. Generate PCI Hospital Posters ----------------------------------------------------
input_period_start <- "2026-04-01"
input_period_end <- "2026-06-30"

source("pci_poster/poster_generator.R")
