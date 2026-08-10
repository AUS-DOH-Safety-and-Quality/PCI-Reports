################################################################################
## Name: setup_raw_data.R
## Purpose: Generates a dummy 'pci_data.csv' file for testing the 
##          'process_ncr_data.R' script.
##
## Author: Antigravity (Assistant)
## Date: January 2026
################################################################################

library(tidyverse)
library(lubridate)

set.seed(42)

# Parameters
n_rows <- 500
hospitals <- c("Royal Perth Hospital", "Fiona Stanley Hospital", "Sir Charles Gairdner Hospital")
start_date <- as.Date("2024-01-01")
end_date   <- as.Date("2024-12-31")

# Helper function for random dates
rand_date <- function(n) {
  start_date + days(sample(0:(as.numeric(end_date - start_date)), n, replace = TRUE))
}

# Generate Data
pci_data <- tibble(
  hid = sample(hospitals, n_rows, replace = TRUE),
  dop = rand_date(n_rows), # Date of Procedure
  
  # Basic Demographics
  age = round(rnorm(n_rows, 65, 10)),
  sex = sample(calculate_dummy_sex <- c(1, 2), n_rows, replace = TRUE), # 1=Male, 2=Female
  
  # Times
  # tso: Symptom Onset (0-12 hours before arrival)
  toa = as.POSIXct(paste(dop, "10:00:00")), # Arrival at hospital
  tso = toa - minutes(sample(30:800, n_rows, replace = TRUE)),
  dso = as.Date(tso),
  doa = as.Date(toa),
  
  # tbd: Door to Balloon (30-120 mins after arrival)
  tbd = toa + minutes(sample(30:150, n_rows, replace = TRUE)),
  top = as.POSIXct(paste(dop, "23:59:59")), # Time of Day Procedure? top in spec seems to usually be "Time of Procedure" or similar cut-off
  # Actually, spec uses top for dbd calc. Let's assume tbd is Time Balloon.
  
  # ECG Times
  tecgd = toa - minutes(sample(10:60, n_rows, replace = TRUE)), # ECG before arrival usually? Or at arrival. 
  # Let's say ECG is slightly after arrival for ER, or before for ambulance.
  decgd = as.Date(tecgd),
  
  # Clinical Vars
  acs = sample(c(0, 1), n_rows, replace=TRUE, prob=c(0.3, 0.7)), # 1=ACS
  pci = sample(c(0, 1), n_rows, replace=TRUE, prob=c(0.1, 0.9)), # 1=PCI Done
  inp = sample(c("0", "1", NA), n_rows, replace=TRUE, prob=c(0.7, 0.2, 0.1)), 
  iht = sample(c(0, 1), n_rows, replace=TRUE, prob=c(0.9, 0.1)), # Inter-hospital transfer
  
  # Outcomes (Strings as per spec regex)
  ihstr = sample(c("0", "1"), n_rows, replace=TRUE, prob=c(0.98, 0.02)), # Stroke
  ihbl  = sample(as.character(0:8), n_rows, replace=TRUE), # Bleeding categories
  dis   = sample(as.character(1:6), n_rows, replace=TRUE, prob=c(rep(0.19, 5), 0.05)), # Discharge status (6 is Dead)
  
  # Follow up
  stat30 = sample(c("0", "1"), n_rows, replace=TRUE),
  crh30  = sample(c("0", "1"), n_rows, replace=TRUE), # Readmission
  pc30   = sample(c("0", "1"), n_rows, replace=TRUE), 
  
  # Revasc
  ihpci  = sample(c("0", "1"), n_rows, replace=TRUE),
  ihpcip = sample(c("0", "1"), n_rows, replace=TRUE),
  ihcab  = sample(c("0", "1"), n_rows, replace=TRUE, prob=c(0.95, 0.05)),
  ihpcab = sample(c("0", "1"), n_rows, replace=TRUE),
  ihtvcab = sample(c("0", "1"), n_rows, replace=TRUE),
  pci30   = sample(c("0", "1"), n_rows, replace=TRUE),
  cab30   = sample(c("0", "1"), n_rows, replace=TRUE),
  
  # Meds
  dstp = sample(c("0", "1"), n_rows, replace=TRUE),
  doll = sample(c("0", "1"), n_rows, replace=TRUE),
  crehab = sample(c("-1", "0", "1"), n_rows, replace=TRUE), # Rehab
  dasp = sample(c("0", "1"), n_rows, replace=TRUE),
  doap = sample(c("0", "1"), n_rows, replace=TRUE),
  
  dmort30 = floor_date(dop + days(sample(c(NA, 0:60), n_rows, replace=TRUE, prob=c(0.95, rep(0.05/61, 61)))), "day") # Death date - mostly NA (alive)
)

# Specs require these columns to exist
# Checking columns from process_ncr_data.R transformation:
# tbd, top, tecgd, decgd, toa, doa, tso, dso

write_csv(pci_data, "pci_data.csv")
message("Dummy pci_data.csv created.")
