################################################################################
## Name: setup_data.R
## Purpose: Generates dummy data for the National Cardiac Registry (NCR) Poster.
##          Constructs a dataset mimicking the structure of real registry outputs.
##
## Author: Antigravity (Assistant)
## Date: January 2026
##
## Overview:
## 1. Defines the scope: Participating hospitals and reporting period.
## 2. Defines the Indicators: Based on NCR 2024 Annual Status Report.
## 3. Generates Random Data: Creates realistic numerators/denominators.
## 4. Exports to Excel: 'cardiac_indicators_summary.xlsx' for the poster script.
################################################################################

# -----------------------------------------------------------------------------
# 1. Setup and Libraries
# -----------------------------------------------------------------------------
library(writexl)    # For exporting to Excel
library(dplyr)      # For data manipulation
library(lubridate)  # For date handling

# -----------------------------------------------------------------------------
# 2. Configuration Parameters
# -----------------------------------------------------------------------------
# Participating Hospitals (Dummy Set)
hospitals <- c(
    "Royal Perth Hospital", 
    "Fiona Stanley Hospital", 
    "Sir Charles Gairdner Hospital"
)

# Date Range: Full Calendar Year 2024
months <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
# Set dates to end of month for reporting consistency
dates  <- ceiling_date(months, "month") - days(1)

# NCR Indicators Selection
# Selected based on available PowerPoint placeholders and User Request
indicators <- c(
  "NCR2",   # Door-to-PCI (Replaces Door-to-Wire)
  "NCR4",   # Major Bleeding
  "NCR6",   # Readmission
  "NCR8",   # 30-day Mortality
  "NCR9",   # Referral to Cardiac Rehab
  "NCR11"   # DAPT at Discharge
)

# -----------------------------------------------------------------------------
# 3. Data Generation Loops
# -----------------------------------------------------------------------------
# We simulate a "long" format dataset: One row per Hospital/Month/Indicator

set.seed(123) # Ensure reproducibility

data_list <- list()

for (h in hospitals) {
  for (d in as.character(dates)) {
    
    # --- A. Generate Clinical Performance Indicators ---
    for (i in indicators) {
      
      # Logic: Customize ranges to make data look realistic for each metric
      if (i == "NCR2") {
        # Door-to-PCI: High compliance expected (75-95%)
        den <- sample(20:50, 1)
        num <- round(den * runif(1, 0.75, 0.95))
        
      } else if (i == "NCR4") {
        # Bleeding: Low event rate (0-3%)
        den <- sample(20:50, 1)
        num <- round(den * runif(1, 0.00, 0.03)) 
        
      } else if (i == "NCR6") {
        # Readmission: Moderate event rate (5-12%)
        den <- sample(80:150, 1)
        num <- round(den * runif(1, 0.05, 0.12)) 
        
      } else if (i == "NCR8") {
        # Mortality: Low event rate (1-5%)
        den <- sample(50:150, 1)
        num <- round(den * runif(1, 0.01, 0.05)) 
        
      } else if (i == "NCR9") {
        # Cardiac Rehab: Moderate-High compliance (60-85%)
        den <- sample(30:60, 1)
        num <- round(den * runif(1, 0.60, 0.85))
        
      } else if (i == "NCR11") {
        # DAPT: Very High compliance expected (90-100%)
        den <- sample(50:150, 1)
        num <- round(den * runif(1, 0.90, 1.00)) 
      }
      
      # Store row
      data_list[[paste(h, d, i)]] <- data.frame(
        hospital_name = h,
        month_end_date = as.Date(d),
        indicator_id = i,
        num = num,
        den = den
      )
    }
  }
}

daily_data <- do.call(rbind, data_list)

# --- B. Generate Volume Metrics (Total PCI) ---
# Volume is often implicitly captured, but we create an explicit row here.
# ID: "VOL_PCI"

vol_list <- list()
for (h in hospitals) {
  for (d in as.character(dates)) {
     # Random monthly volume between 150-250 cases
     vol <- sample(150:250, 1) 
     
     vol_list[[paste(h, d)]] <- data.frame(
        hospital_name = h,
        month_end_date = as.Date(d),
        indicator_id = "VOL_PCI",
        num = vol,
        den = vol # Denom same as Num for simple count indicators
     )
  }
}
vol_data <- do.call(rbind, vol_list)

# -----------------------------------------------------------------------------
# 4. Final Combination and Export
# -----------------------------------------------------------------------------
final_data <- rbind(daily_data, vol_data)

if(!dir.exists("_files")) dir.create("_files", recursive = TRUE)
output_file <- "_files/cardiac_indicators_summary.xlsx"
write_xlsx(final_data, output_file)

message(paste("Dummy data generated and saved to", output_file))
