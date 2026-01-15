################################################################################
## Project: Cardiac Registry of Western Australia (CRoWA) analysis & reporting
## Script: process_ncr_data.R
## Department: WA Health, Healthcare Quality Intelligence Unit (HQIU) 
## Warning: Standard Header
##
## Purpose: Processes raw 'pci_data.csv' to apply NCR Indicator logic.
##          1. Validates Data against NCR Specifications
##          2. Calculates Exclusions and Inclusion flags
##          3. Generates Summaries for Poster Generation
##
## Author: Richard Gillett
## Date: January 2026
################################################################################

library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(writexl)
library(tidyr)
library(here) # For robust paths

# -----------------------------------------------------------------------------
# 1. Load Data
# -----------------------------------------------------------------------------
input_file <- here("pci_data.csv")

# Function to generate dummy data if file is missing
generate_dummy_data <- function() {
  message("Generating dummy data for testing...")
  set.seed(42)
  n_rows <- 500
  hospitals <- c("Royal Perth Hospital", "Fiona Stanley Hospital", "Sir Charles Gairdner Hospital")
  start_date <- as.Date("2024-01-01")
  end_date   <- as.Date("2024-12-31")
  
  rand_date <- function(n) {
    start_date + days(sample(0:(as.numeric(end_date - start_date)), n, replace = TRUE))
  }
  
  tibble(
    hid = sample(hospitals, n_rows, replace = TRUE),
    dop = rand_date(n_rows),
    # Basic Demographics
    age = round(rnorm(n_rows, 65, 10)),
    sex = sample(calculate_dummy_sex <- c(1, 2), n_rows, replace = TRUE),
    # Times
    toa = as.POSIXct(paste(dop, "10:00:00")),
    tso = toa - minutes(sample(30:800, n_rows, replace = TRUE)),
    dso = as.Date(tso),
    doa = as.Date(toa),
    # tbd: Door to Balloon
    tbd = toa + minutes(sample(30:150, n_rows, replace = TRUE)),
    top = as.POSIXct(paste(dop, "23:59:59")),
    # ECG Times
    tecgd = toa - minutes(sample(10:60, n_rows, replace = TRUE)),
    decgd = as.Date(tecgd),
    # Clinical Vars
    acs = sample(c(0, 1), n_rows, replace=TRUE, prob=c(0.3, 0.7)),
    acst = sample(c("1", "2", "3"), n_rows, replace=TRUE), # 1=UA, 2=NSTEMI, 3=STEMI
    pci = sample(c(0, 1), n_rows, replace=TRUE, prob=c(0.1, 0.9)),
    inp = sample(c("0", "1", NA), n_rows, replace=TRUE, prob=c(0.7, 0.2, 0.1)),
    iht = sample(c(0, 1), n_rows, replace=TRUE, prob=c(0.9, 0.1)),
    # Outcomes
    ihstr = sample(c("0", "1"), n_rows, replace=TRUE, prob=c(0.98, 0.02)),
    ihbl  = sample(as.character(0:8), n_rows, replace=TRUE),
    dis   = sample(as.character(1:6), n_rows, replace=TRUE, prob=c(rep(0.19, 5), 0.05)),
    # Follow up
    stat30 = sample(c("0", "1"), n_rows, replace=TRUE),
    crh30  = sample(c("0", "1"), n_rows, replace=TRUE),
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
    crehab = sample(c("-1", "0", "1"), n_rows, replace=TRUE),
    dasp = sample(c("0", "1"), n_rows, replace=TRUE),
    doap = sample(c("0", "1"), n_rows, replace=TRUE),
    dmort30 = floor_date(dop + days(sample(c(NA, 0:60), n_rows, replace=TRUE, prob=c(0.95, rep(0.05/61, 61)))), "day")
  )
}

if (!file.exists(input_file)) {
    message("pci_data.csv not found. Using generated dummy data.")
    pci_data <- generate_dummy_data()
    if(!dir.exists(here("_files"))) dir.create(here("_files"), recursive = TRUE)
    write_csv(pci_data, here("_files", "fake_pci_data.csv"))
    message("Saved generated dummy data to _files/fake_pci_data.csv")
} else {
    message(paste("Reading", input_file, "..."))
    pci_data <- read_csv(input_file, show_col_types = FALSE)
}

# -----------------------------------------------------------------------------
# 2. Data Transformation (From Spec)
# -----------------------------------------------------------------------------
message("Applying data transformations...")

# Helper to robustly combine Date and Time columns
# If 'time_col' is already POSIXct, it returns it.
# If 'time_col' is character/HMS and 'date_col' is provided, it combines them.
combine_datetime <- function(date_val, time_val) {
  # If time is already a Date/POSIXt object, assume it allows robust handling or is already combined
  if (is.POSIXt(time_val)) return(time_val)
  
  # If time is NA, return NA
  # If date is NA, return NA
  
  # Attempt combination
  # Paste date and time strings
  dt_text <- paste(as.character(date_val), as.character(time_val))
  # Parse using lubridate (flexible format)
  out <- lubridate::ymd_hms(dt_text, quiet = TRUE)
  
  # Try without seconds if failed? Standardize to yMD HMS usually works for standard DB dumps.
  # If parsing failed (NA) but inputs weren't NA, might be just HM
  if (all(is.na(out)) && !all(is.na(dt_text))) {
      out <- lubridate::ymd_hm(dt_text, quiet = TRUE)
  }
  return(out)
}

# Basic Dates & Derived Columns
pci_data <- pci_data %>%
    mutate(
        dop = as.Date(dop),
        period_start = floor_date(dop, unit = "month"),
        period_end   = ceiling_date(period_start, unit = "month") - days(1),
        
        # --- Robust Date-Time Combinations ---
        # We explicitly combine Date and Time columns to single POSIXct objects
        # This handles midnight crossings correctly when calculating differences.
        
        # 1. Symptom Onset (dso + tso)
        dt_symptom = combine_datetime(dso, tso),
        
        # 2. Arrival / Door (doa + toa)
        dt_arrival = combine_datetime(doa, toa),
        
        # 3. First ECG (decgd + tecgd)
        dt_ecg     = combine_datetime(decgd, tecgd),
        
        # 4. Device / Balloon (dop + tbd, or dbd + tbd if available)
        # Using 'dop' as the date for 'tbd' as default, but ideally we'd use 'dbd'.
        # If 'dbd' exists in data, we should use it. For now, assuming dop.
        # Expert check: "tbd <= top ~ dop + days(1)" logic previously implied tbd is just time.
        dt_device  = combine_datetime(dop, tbd), 
        
        
        # --- Time Intervals (Minutes) ---
        
        # ecgdb: ECG to Device Time (Minutes)
        ecgdb = as.numeric(difftime(dt_device, dt_ecg, units = "mins")),
        
        # dbdt: Door to Device Time (Minutes)
        dbdt = as.numeric(difftime(dt_device, dt_arrival, units = "mins")),
        
        # symptom_to_door: Symptom Onset to Door (Minutes)
        symptom_to_door = as.numeric(difftime(dt_arrival, dt_symptom, units = "mins")),
        
        # inp: Inpatient Status - Prefer raw definition if available
        # If raw 'inp' exists and is not NA, use it. Otherwise derive fallback.
        inp_derived = case_when(
           as.character(acs) == "1" & symptom_to_door > 0 ~ "0",
           as.character(acs) == "1" & symptom_to_door <= 0 ~ "1",
           TRUE ~ NA_character_
        ),
        inp = if_else(!is.na(inp), as.character(inp), inp_derived)
    )

# -----------------------------------------------------------------------------
# 3. Indicator Logic Implementation
# -----------------------------------------------------------------------------
message("Applying Indicator Logic...")

# Helper function to categorize status
# status: "Pass" (Favourable Outcome), "Fail" (Unfavourable Outcome), "Excluded"
# For metrics where outcome=1 is UNFAVOURABLE (e.g. Mortality), Outcome=1 -> Fail
# For metrics where outcome=1 is FAVOURABLE (e.g. Rehab), Outcome=1 -> Pass

apply_indicator_logic <- function(data) {
    data %>%
    mutate(
        # --- NCR1: Door-to-ECG to PCI (<90m is guideline, but metric is Median Time) ---
        # Spec Den: pci==1 & acst==3 (STEMI) & inp==0 & iht==0 & ecgdb>0 & symptom_to_door<720
        NCR1_eligible = if_else(as.character(pci) == "1" & 
                                as.character(acst) == "3" &
                                as.character(inp) == "0" & 
                                as.character(iht) == "0" & 
                                ecgdb > 0 & 
                                symptom_to_door < 720, 1, 0, missing = 0),
                                
        NCR1_outcome  = if_else(NCR1_eligible == 1, ecgdb, NA_real_), # Outcome is the TIME
        
        NCR1_exclusion = case_when(
            NCR1_eligible == 1 ~ NA_character_,
            as.character(pci) != "1" ~ "Not PCI",
            as.character(acst) != "3" ~ "Not STEMI",
            as.character(inp) != "0" ~ "Inpatient",
            as.character(iht) != "0" ~ "Inter-hospital Transfer",
            ecgdb <= 0 ~ "Invalid ECG-to-Device Time",
            symptom_to_door >= 720 ~ "Symptom > 12h",
            TRUE ~ "Unknown Exclusion"
        ),
        
        NCR1_status = if_else(NCR1_eligible == 1, "Included", "Excluded"),

        # --- NCR2: Door to PCI ---
        # Spec Den: pci==1 & acst==3 (STEMI) & inp==0 & iht==0 & dbdt>0 & symptom_to_door<720
        NCR2_eligible = if_else(as.character(pci) == "1" & 
                                as.character(acst) == "3" &
                                as.character(inp) == "0" & 
                                as.character(iht) == "0" & 
                                dbdt > 0 & 
                                symptom_to_door < 720, 1, 0, missing = 0),
                                
        NCR2_outcome  = if_else(NCR2_eligible == 1, dbdt, NA_real_),
        
        NCR2_exclusion = case_when(
            NCR2_eligible == 1 ~ NA_character_,
            as.character(pci) != "1" ~ "Not PCI",
            as.character(acst) != "3" ~ "Not STEMI",
            as.character(inp) != "0" ~ "Inpatient",
            as.character(iht) != "0" ~ "Inter-hospital Transfer",
            dbdt <= 0 ~ "Invalid Door-to-Device Time",
            symptom_to_door >= 720 ~ "Symptom > 12h",
            TRUE ~ "Unknown Exclusion"
        ),
        NCR2_status = if_else(NCR2_eligible == 1, "Included", "Excluded"),

        # --- NCR3: Peri-PCI Stroke ---
        # Spec Den: ihstr == 0 | ihstr == 1 (basically everyone with valid data)
        NCR3_eligible = if_else(!is.na(ihstr) & as.character(ihstr) %in% c("0","1"), 1, 0),
        NCR3_outcome  = if_else(NCR3_eligible == 1 & as.character(ihstr) == "1", 1, 0, missing = 0),
        
        NCR3_exclusion = if_else(NCR3_eligible == 0, "Missing/Invalid Data", NA_character_),
        NCR3_status    = case_when(
            NCR3_eligible == 0 ~ "Excluded",
            NCR3_outcome == 1 ~ "Fail", # Stroke occurred
            TRUE ~ "Pass"
        ),

        # --- NCR4: In-hospital Major Bleeding ---
        # Spec Den: ihbl in [0-8]
        NCR4_eligible = if_else(str_detect(as.character(ihbl), "[012345678]"), 1, 0, missing = 0),
        # Spec Num: ihbl in [3,4,5,7,8] (Major Bleeding categories)
        NCR4_outcome  = if_else(NCR4_eligible == 1 & str_detect(as.character(ihbl), "[34578]"), 1, 0, missing=0),
        
        NCR4_exclusion = if_else(NCR4_eligible == 0, "Missing/Invalid Data", NA_character_),
        NCR4_status    = case_when(
            NCR4_eligible == 0 ~ "Excluded",
            NCR4_outcome == 1 ~ "Fail", # Bleed occurred
            TRUE ~ "Pass"
        ),

        # --- NCR5: In-hospital Mortality ---
        # Spec Den: dis in [1-6]
        NCR5_eligible = if_else(str_detect(as.character(dis), "[123456]"), 1, 0, missing = 0),
        NCR5_outcome  = if_else(NCR5_eligible == 1 & as.character(dis) == "6", 1, 0, missing=0), # 6 = Deceased
        
        NCR5_exclusion = if_else(NCR5_eligible == 0, "Missing/Invalid Data", NA_character_),
        NCR5_status    = case_when(
            NCR5_eligible == 0 ~ "Excluded",
            NCR5_outcome == 1 ~ "Fail",
            TRUE ~ "Pass"
        ),

        # --- NCR6: 30-Day Readmission ---
        NCR6_eligible = if_else(
               str_detect(as.character(dis), "[12345]") & 
               as.character(stat30) %in% c("1","0") & 
               as.character(crh30) %in% c("1","0"), 1, 0, missing=0),
        NCR6_outcome  = if_else(NCR6_eligible == 1 & 
                                as.character(crh30) == "1" & 
                                as.character(pc30) == "0", 1, 0, missing=0),
        
        NCR6_exclusion = case_when(
            NCR6_eligible == 1 ~ NA_character_,
            !str_detect(as.character(dis), "[12345]") ~ "Deceased/Invalid Discharge",
            TRUE ~ "Missing 30-day Follow-up"
        ),
        NCR6_status = case_when(
            NCR6_eligible == 0 ~ "Excluded",
            NCR6_outcome == 1 ~ "Fail",
            TRUE ~ "Pass"
        ),

        # --- NCR7: Unplanned Revasc ---
        NCR7_eligible = if_else(as.character(ihpci) %in% c("0","1") & as.character(ihcab) %in% c("0","1"), 1, 0, missing=0),
        NCR7_outcome  = if_else(NCR7_eligible == 1 & (
                          (as.character(ihpci)=="1" & as.character(ihpcip)=="0") |
                          (as.character(ihcab)=="1" & as.character(ihpcab)=="0" & as.character(ihtvcab)=="1") |
                          (as.character(pc30)=="0" & as.character(pci30)=="1") |
                          (as.character(pc30)=="0" & as.character(cab30)=="1")
                        ), 1, 0, missing=0),
        NCR7_exclusion = if_else(NCR7_eligible==0, "Missing Revasc Data", NA_character_),
        NCR7_status   = case_when(
            NCR7_eligible == 0 ~ "Excluded",
            NCR7_outcome == 1 ~ "Fail",
            TRUE ~ "Pass"
        ),

        # --- NCR8: 30-Day Mortality ---
        NCR8_eligible = if_else(str_detect(as.character(dis), "[123456]"), 1, 0, missing=0),
        # Assuming dmort30 is Date. If NA, we assume alive unless dis==6
        # Fix: Capture death WITHIN 30 days (<=), not AFTER (>=)
        NCR8_outcome  = if_else(NCR8_eligible == 1 & (
                           (!is.na(dmort30) & dmort30 <= (dop + 30)) |
                           as.character(dis) == "6"
                        ), 1, 0, missing=0),
        NCR8_exclusion = if_else(NCR8_eligible==0, "Invalid Discharge Data", NA_character_),
        NCR8_status    = case_when(
            NCR8_eligible == 0 ~ "Excluded",
            NCR8_outcome == 1 ~ "Fail",
            TRUE ~ "Pass"
        ),

        # --- NCR9: Lipid Lowering (Good Metric) ---
        # Placeholder for contraindication logic
        contra_lipid = FALSE, 
        
        NCR9_eligible = if_else(str_detect(as.character(dis), "[12345]") & 
                                !contra_lipid &
                                (as.character(dstp) %in% c("0","1") | as.character(doll) %in% c("0","1")), 1, 0, missing=0),
        
        NCR9_outcome  = if_else(NCR9_eligible == 1 & (as.character(dstp)=="1" | as.character(doll)=="1"), 1, 0, missing=0),
        
        NCR9_exclusion = case_when(
             NCR9_eligible == 1 ~ NA_character_,
             contra_lipid ~ "Contraindicated",
             !str_detect(as.character(dis), "[12345]") ~ "Deceased/Invalid Discharge",
             TRUE ~ "Missing Data"
        ),
        NCR9_status    = case_when(
            NCR9_eligible == 0 ~ "Excluded",
            NCR9_outcome == 1 ~ "Pass", # On Meds = Good
            TRUE ~ "Fail"
        ),

        # --- NCR10: Cardiac Rehab (Good Metric) ---
        NCR10_eligible = if_else(str_detect(as.character(dis), "[12345]") & 
                                 as.character(crehab) %in% c("-1","1","0"), 1, 0, missing=0),
        NCR10_outcome  = if_else(NCR10_eligible == 1 & as.character(crehab) == "1", 1, 0, missing=0),
        
        NCR10_exclusion = if_else(NCR10_eligible==0, "Deceased or Invalid Rehab Data", NA_character_),
        NCR10_status    = case_when(
            NCR10_eligible == 0 ~ "Excluded",
            NCR10_outcome == 1 ~ "Pass", # Referred = Good
            TRUE ~ "Fail"
        ),

        # --- NCR11: DAPT (Good Metric) ---
        # Placeholder for contraindication logic
        contra_dapt = FALSE,
        
        NCR11_eligible = if_else(str_detect(as.character(dis), "[12345]") & 
                                 !contra_dapt &
                                 (as.character(dasp) %in% c("0","1") | as.character(doap) %in% c("0","1")), 1, 0, missing=0),
        NCR11_outcome  = if_else(NCR11_eligible == 1 & (as.character(dasp)=="1" & as.character(doap)=="1"), 1, 0, missing=0),
        
        NCR11_exclusion = case_when(
             NCR11_eligible == 1 ~ NA_character_,
             contra_dapt ~ "Contraindicated",
             !str_detect(as.character(dis), "[12345]") ~ "Deceased/Invalid Discharge",
             TRUE ~ "Missing Data"
        ),
        NCR11_status    = case_when(
            NCR11_eligible == 0 ~ "Excluded",
            NCR11_outcome == 1 ~ "Pass", # On DAPT = Good
            TRUE ~ "Fail"
        )
    )
}

pci_data_processed <- apply_indicator_logic(pci_data)

# -----------------------------------------------------------------------------
# 4. Save Processed Data (Flags Added)
# -----------------------------------------------------------------------------
output_processed <- here("_files", "pci_data_processed.csv")
if(!dir.exists(dirname(output_processed))) dir.create(dirname(output_processed), recursive = TRUE)
write_csv(pci_data_processed, output_processed)
message(paste("Processed data saved to", output_processed))

# -----------------------------------------------------------------------------
# 5. Summarize for Poster
# -----------------------------------------------------------------------------
message("Summarizing for Poster...")

indicators <- paste0("NCR", 1:11)
summary_list <- list()

for (i_id in indicators) {
    
    col_elig <- sym(paste0(i_id, "_eligible"))
    col_out  <- sym(paste0(i_id, "_outcome"))
    
    # Check if this is a median metric (NCR1, NCR2)
    current_summary <- pci_data_processed %>%
        filter(!!col_elig == 1) %>% # Filter to eligible only
        group_by(hid, period_end) %>%
        summarise(
            # For Median metrics (outcome is numeric time), Num = Median
            # For Count metrics (outcome is 0/1), Num = Sum(1s)
            
            # Logic: If max outcome > 1, assume it's a time metric (safe assumption for min/sec logic)
            # OR explicitly check ID
            num = if(i_id %in% c("NCR1", "NCR2")) {
                median(!!col_out, na.rm = TRUE)
            } else {
                sum(!!col_out, na.rm = TRUE)
            },
            
            den = n(), # Denom is count of eligible rows
            .groups = "drop"
        ) %>%
        mutate(
            indicator_id = i_id,
            hospital_name = hid, # Map hid to hospital_name
            month_end_date = period_end
        ) %>%
        select(hospital_name, month_end_date, indicator_id, num, den)
        
    summary_list[[i_id]] <- current_summary
}

# Volume Metric (VOL_PCI)
vol_summary <- pci_data_processed %>%
    group_by(hid, period_end) %>%
    summarise(
        num = n(), # Total Procedures
        den = n(),
        .groups = "drop"
    ) %>%
    mutate(
        indicator_id = "VOL_PCI",
        hospital_name = hid,
        month_end_date = period_end
    ) %>%
    select(hospital_name, month_end_date, indicator_id, num, den)

summary_list[["VOL_PCI"]] <- vol_summary

final_summary <- bind_rows(summary_list)

# Fix Hospital Names (if hid is codes)
# Map known HIDs to Names if necessary, or assume HID is the Name
# The previous scripts used full names. If pci_data has codes, we might need a mapping.
# For now, we assume raw data has names or we utilize it as is.

output_summary <- here("_files", "cardiac_indicators_summary.xlsx")
write_xlsx(final_summary, output_summary)
message(paste("Summary generated:", output_summary))
