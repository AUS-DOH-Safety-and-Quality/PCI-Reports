library(readxl)
library(readr)
library(stringr)
library(here)

# Path to the dictionary
dict_path <- here("reference_files", "wa_pci_data_dictionary.xlsm")

if (!file.exists(dict_path)) {
  stop("Dictionary file not found!")
}

# List sheets to find the right one
sheets <- excel_sheets(dict_path)
message("Sheets found: ", paste(sheets, collapse = ", "))

# Identify the indicator sheet (looking for "NCR Indicators" specifically)
indicator_sheet <- "NCR Indicators"

if (is.na(indicator_sheet)) {
  stop("Could not automatically identify the Indicator Specification sheet.")
}

message("Extracting from sheet: ", indicator_sheet)

# Read the sheet
specs <- read_excel(dict_path, sheet = indicator_sheet)

# Save to CSV for the agent to read
output_path <- here("reference_files", "extracted_indicator_specs.csv")
write_csv(specs, output_path)
message("Saved extracted specs to: ", output_path)
