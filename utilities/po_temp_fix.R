# This script is used to temporarily fix the po from the NCR data extract. Once the PowerBI dataflow is updated with the correct logic flow, this is no longer required.

# Setup ####
library(qiverse)
library(data.table)

tk <- get_az_tk('pbi_df')

# Load Datasets ####
angioplasty_smhs <- download_dataflow_table(
  workspace_name = 'DOH - Clinical Datasets',
  dataflow_name = 'Cardiobase SMHS - Load',
  table_name = 'ANGIOPLASTY',
  access_token = tk$credentials$access_token
) |>
  as.data.table()

lookup_smhs <- download_dataflow_table(
  workspace_name = 'DOH - Clinical Datasets',
  dataflow_name = 'Cardiobase SMHS - Load',
  table_name = 'LOOKUP',
  access_token = tk$credentials$access_token
) |>
  as.data.table()

angioplasty_scgh <- download_dataflow_table(
  workspace_name = 'DOH - Clinical Datasets',
  dataflow_name = 'Cardiobase SCGH - Load',
  table_name = 'ANGIOPLASTY',
  access_token = tk$credentials$access_token
) |>
  as.data.table()

lookup_scgh <- download_dataflow_table(
  workspace_name = 'DOH - Clinical Datasets',
  dataflow_name = 'Cardiobase SCGH - Load',
  table_name = 'LOOKUP',
  access_token = tk$credentials$access_token
) |>
  as.data.table()

# Transform Existing pci_data ####
pci_data_dt <- fread("pci_data.csv")

## For SMHS (RPH and FSH) ####
check_operators_smhs <- merge(
  pci_data_dt[CARDIOBASE_GROUP == 'SMHS', .(ANGIOPLASTY_ID, doa, dop, po)],
  angioplasty_smhs[, .(ANGIOPLASTY_ID, TEST_DATE, ORDERING_DOCTOR, OPERATOR1_LKP_ID, OPERATOR1STATUS_LKP_ID, OPERATOR2_LKP_ID, OPERATOR2STATUS_LKP_ID, OPERATOR3_LKP_ID, OPERATOR3STATUS_LKP_ID)],
  by = 'ANGIOPLASTY_ID',
  all.x = TRUE
) |>
  merge(
    lookup_smhs[, .(LOOKUP_ID, OPERATOR1_ITEM_DATA = ITEM_DATA, OPERATOR1_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR1_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_smhs[, .(LOOKUP_ID, OPERATOR1STATUS_ITEM_DATA = ITEM_DATA, OPERATOR1STATUS_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR1STATUS_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_smhs[, .(LOOKUP_ID, OPERATOR2_ITEM_DATA = ITEM_DATA, OPERATOR2_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR2_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_smhs[, .(LOOKUP_ID, OPERATOR2STATUS_ITEM_DATA = ITEM_DATA, OPERATOR2STATUS_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR2STATUS_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_smhs[, .(LOOKUP_ID, OPERATOR3_ITEM_DATA = ITEM_DATA, OPERATOR3_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR3_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_smhs[, .(LOOKUP_ID, OPERATOR3STATUS_ITEM_DATA = ITEM_DATA, OPERATOR3STATUS_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR3STATUS_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  _[, c('OPERATOR1_LKP_ID', 'OPERATOR1STATUS_LKP_ID', 'OPERATOR2_LKP_ID', 'OPERATOR2STATUS_LKP_ID', 'OPERATOR3_LKP_ID', 'OPERATOR3STATUS_LKP_ID') := NULL]

### Double check that po is solely sourced from cardiobase
check_operators_smhs[po != OPERATOR1_ITEM_DATA]

### Check for records where there is a listed consultant in 2 or 3, but not in 1
check_operators_smhs[
  OPERATOR1STATUS_ITEM_DATA != 'Consultant' &
    (OPERATOR2STATUS_ITEM_DATA == 'Consultant' | OPERATOR3STATUS_ITEM_DATA == 'Consultant')] |>
  View()

### Check for records where there is no listed consultant for the procedure
check_operators_smhs[
  OPERATOR1STATUS_ITEM_DATA != 'Consultant' &
    is.na(OPERATOR2_ITEM_DATA)]

## For SCGH ####
check_operators_scgh <- merge(
  pci_data_dt[CARDIOBASE_GROUP == 'SCGH', .(ANGIOPLASTY_ID, doa, dop, po)],
  angioplasty_scgh[, .(ANGIOPLASTY_ID, TEST_DATE, ORDERING_DOCTOR, OPERATOR1_LKP_ID, OPERATOR1STATUS_LKP_ID, OPERATOR2_LKP_ID, OPERATOR2STATUS_LKP_ID, OPERATOR3_LKP_ID, OPERATOR3STATUS_LKP_ID)],
  by = 'ANGIOPLASTY_ID',
  all.x = TRUE
) |>
  merge(
    lookup_scgh[, .(LOOKUP_ID, OPERATOR1_ITEM_DATA = ITEM_DATA, OPERATOR1_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR1_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_scgh[, .(LOOKUP_ID, OPERATOR1STATUS_ITEM_DATA = ITEM_DATA, OPERATOR1STATUS_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR1STATUS_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_scgh[, .(LOOKUP_ID, OPERATOR2_ITEM_DATA = ITEM_DATA, OPERATOR2_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR2_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_scgh[, .(LOOKUP_ID, OPERATOR2STATUS_ITEM_DATA = ITEM_DATA, OPERATOR2STATUS_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR2STATUS_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_scgh[, .(LOOKUP_ID, OPERATOR3_ITEM_DATA = ITEM_DATA, OPERATOR3_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR3_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  merge(
    lookup_scgh[, .(LOOKUP_ID, OPERATOR3STATUS_ITEM_DATA = ITEM_DATA, OPERATOR3STATUS_ADDITIONAL_DATA = ADDITIONAL_DATA)],
    by.x = 'OPERATOR3STATUS_LKP_ID',
    by.y = 'LOOKUP_ID',
    all.x = TRUE
  ) |>
  _[, c('OPERATOR1_LKP_ID', 'OPERATOR1STATUS_LKP_ID', 'OPERATOR2_LKP_ID', 'OPERATOR2STATUS_LKP_ID', 'OPERATOR3_LKP_ID', 'OPERATOR3STATUS_LKP_ID') := NULL]

### Double check that po is solely sourced from cardiobase
check_operators_scgh[po != OPERATOR1_ITEM_DATA]

### Check for records where there is a listed consultant in 2 or 3, but not in 1
check_operators_scgh[
  # Not mentioning cardiologist as operator status, or in operator additional data
  (OPERATOR1STATUS_ITEM_DATA != 'Cardiologist' & !(tolower(OPERATOR1_ADDITIONAL_DATA) == 'cardiologist')) &
    # And there is a cardiologist in operator 2 or 3
    (OPERATOR2STATUS_ITEM_DATA == 'Cardiologist' | tolower(OPERATOR2_ADDITIONAL_DATA) == 'cardiologist' |
       OPERATOR3STATUS_ITEM_DATA == 'Cardiologist' | tolower(OPERATOR3_ADDITIONAL_DATA) == 'cardiologist')] |>
  View()

### Check for records where there is no listed consultant for the procedure
check_operators_scgh[
  # Not mentioning cardiologist as operator status, or in operator additional data
  (OPERATOR1STATUS_ITEM_DATA != 'Cardiologist' & !(tolower(OPERATOR1_ADDITIONAL_DATA) == 'cardiologist')) &
    is.na(OPERATOR2_ITEM_DATA)]

# Create correct mapping for procedure operator
po_fixed_smhs <- check_operators_smhs[
  , .(ANGIOPLASTY_ID,
      po,
      po_fixed = fcase(
        OPERATOR1STATUS_ITEM_DATA == 'Consultant', trimws(OPERATOR1_ITEM_DATA),
        OPERATOR2STATUS_ITEM_DATA == 'Consultant', trimws(OPERATOR2_ITEM_DATA),
        OPERATOR3STATUS_ITEM_DATA == 'Consultant', trimws(OPERATOR3_ITEM_DATA),
        default = trimws(OPERATOR1_ITEM_DATA) # default to first operator if none are consultant
      ))]
po_fixed_scgh <- check_operators_scgh[
  , .(
    ANGIOPLASTY_ID,
    po,
    po_fixed = fcase(
      (OPERATOR1STATUS_ITEM_DATA == 'Cardiologist' | tolower(OPERATOR1_ADDITIONAL_DATA) == 'cardiologist'), trimws(OPERATOR1_ITEM_DATA),
      (OPERATOR2STATUS_ITEM_DATA == 'Cardiologist' | tolower(OPERATOR2_ADDITIONAL_DATA) == 'cardiologist'), trimws(OPERATOR2_ITEM_DATA),
      (OPERATOR3STATUS_ITEM_DATA == 'Cardiologist' | tolower(OPERATOR3_ADDITIONAL_DATA) == 'cardiologist'), trimws(OPERATOR3_ITEM_DATA),
      default = trimws(OPERATOR1_ITEM_DATA) # default to first operator if none are consultant
    ))]

po_fixed <- rbind(
  po_fixed_smhs,
  po_fixed_scgh
)

