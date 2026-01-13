################################################################################
## Project: Cardiac Outcomes Registry (NCR) Reporting
## Script: poster_generator.R
## Department: WA Health, HQIU
## Warning: Standard Header
##
## Purpose: Creates National Cardiac Registry (NCR) Posters for hospitals
##          incorporating user-specified data and ANZELA-style visuals.
##
## Author: Richard Gillett
## Date: January 2026
################################################################################
##
## Overview:
## 1. Sets up directory paths and loads configuration.
## 2. Reads cardiac registry data (Excel) and indicator definitions (CSV).
## 3. Processes data to calculate totals, denominators, and percentages.
## 4. Maps NCR indicators to PowerPoint template placeholders.
##    (Note: Uses ANZELA placeholders as fallback if custom template is missing)
## 5. Iterates through each hospital to generate a specific PowerPoint poster.
## 6. Handles logic for inserting hospital logos vs. generated maps.
################################################################################

# -----------------------------------------------------------------------------
# 1. Setup and Library Loading
# -----------------------------------------------------------------------------
# Load necessary packages for data manipulation and PowerPoint generation
library(dplyr)      # For data manipulation
library(officer)    # For PowerPoint creation
library(svDialogs)  # For system dialogs (not strictly used in batch mode but kept for compat)
library(scales)     # For percentage formatting
library(readxl)     # For reading Excel data
library(here)       # For relative path management
library(lubridate)  # For date handling
library(readr)      # For reading CSV files

# Define base directory logic
# Checks if running from project root or checks for template existence
# Robust check: Look for the template file itself
if (file.exists("poster_template.pptx")) {
  base_dir <- "."
} else if (file.exists(file.path("cardiac_poster", "poster_template.pptx"))) {
  base_dir <- "cardiac_poster"
} else {
  # Fallback: check if we are in a weird relative state or fail
  stop("Could not find 'poster_template.pptx'. Please set working directory to the project root or 'cardiac_poster'.")
}

# Define Paths
template_path     <- file.path(base_dir, "poster_template.pptx")
output_poster_dir <- file.path(base_dir, "_files", "posters")
logo_folder       <- file.path(base_dir, "hospital_logos")
map_folder        <- file.path(base_dir, "hospital_specific_maps")
ref_dir           <- file.path(base_dir, "reference_files")

# Create output directory if it doesn't exist
if (!dir.exists(output_poster_dir)) {
  dir.create(output_poster_dir, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------------------------------------------------------
# 2. Data Loading & Indicator Configuration
# -----------------------------------------------------------------------------
data_file       <- file.path(base_dir, "_files", "cardiac_indicators_summary.xlsx")
indicators_file <- file.path(ref_dir, "indicators.csv")

# Check for data file
if (!file.exists(data_file)) {
  stop("Data file not found: cardiac_indicators_summary.xlsx")
}

# Read the main data
ncr_data <- readxl::read_excel(data_file)

# Read Indicator Descriptions
# Used to populate the narrative text on the poster
if (file.exists(indicators_file)) {
  ind_desc_df <- read_csv(indicators_file, show_col_types = FALSE)
  names(ind_desc_df) <- tolower(names(ind_desc_df))
  
  # Create a lookup vector: Indicator ID -> Description
  desc_lookup <- setNames(ind_desc_df$description, ind_desc_df$ncr_indicator_number)
} else {
  warning("Indicators reference file not found. Using fallback descriptions.")
  desc_lookup <- c()
}

# -----------------------------------------------------------------------------
# 3. Data Processing
# -----------------------------------------------------------------------------
# Calculate summaries for the reporting period (Last 12 months)
poster_summary_data <- ncr_data %>%
  mutate(month_end_date = lubridate::as_date(month_end_date)) %>%
  group_by(hospital_name) %>%
  mutate(
    # Identify the latest date -> 3 month window (Current Month + Previous 2)
    hosp_latest = max(month_end_date, na.rm = TRUE),
    hosp_start  = hosp_latest %m-% months(2) 
  ) %>%
  # Filter data to the 3-month window
  filter(month_end_date >= hosp_start & month_end_date <= hosp_latest) %>%
  group_by(hospital_name, indicator_id, hosp_start, hosp_latest) %>%
  summarise(
    total_num = if(first(indicator_id) %in% c("NCR1", "NCR2")) {
        median(num, na.rm = TRUE)
    } else {
        sum(num, na.rm = TRUE)
    },
    total_den = sum(den, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Calculate percentage
    pct = ifelse(!is.na(total_den) & total_den > 0, total_num / total_den, 0),
    pct_label = scales::percent(pct, accuracy = 0.1),
    pct_label_whole = scales::percent(pct, accuracy = 1),
    
    # Generate text for "Volume" type indicators (Cases vs %)
    poster_text = case_when(
        indicator_id == "VOL_PCI" ~ paste0(total_num, " Cases"),
        is.na(total_den) | total_den == 0 ~ "No Data",
        TRUE ~ paste0(total_num, "/", total_den, " (", pct_label, ")")
    )
  )

# -----------------------------------------------------------------------------
# 4. Placeholder Mapping
# -----------------------------------------------------------------------------
# Maps NCR Indicator IDs to the PowerPoint Shape Names (Placeholders).
# Note: Current mapping target is the standard ANZELA template slots as fallback.


# Maps NCR Indicator IDs to the PowerPoint Shape Names (Placeholders).
# Note: Current mapping target is the standard ANZELA template slots as fallback.

indicator_mapping <- c(
  "NCR2"    = "ph_door_to_reperfusion",      # Map Door-to-PCI
  "NCR4"    = "ph_ihbl",                     # Map Bleeding
  "NCR8"    = "ph_mort30r",                  # Map Mortality (Preserve existing)
  "NCR6"    = "ph_postop_critical_care",     # Map Readmission
  "NCR10"   = "ph_crehab",                   # Map Referrals to cardiac rehab (Moved to NCR10)
  "NCR11"   = "ph_pre_risk_assess",          # Map DAPT
  "VOL_PCI" = "ph_pci_count"                 # Map Volume
)

# Metric Types (Default is Percentage)
metric_types <- c(
  "NCR1"    = "median",
  "NCR2"    = "median",
  "VOL_PCI" = "count"
)

# Append descriptive text to the data frame for easy insertion
poster_summary_data <- poster_summary_data %>%
  rowwise() %>%
  mutate(
    raw_desc = desc_lookup[indicator_id],
    raw_desc = ifelse(is.na(raw_desc), "Indicator Description Missing", raw_desc),
    
    # Identify Type
    metric_type = ifelse(indicator_id %in% names(metric_types), metric_types[indicator_id], "percent"),
    
    # Safe Pct Calc
    safe_pct = ifelse(!is.na(total_den) & total_den > 0, total_num / total_den, 0),
    
    # Calculate Label based on Type
    val_label = case_when(
        metric_type == "median" ~ paste0(round(total_num), "m"),
        metric_type == "count"  ~ paste0(total_num, " Cases"),
        metric_type == "percent" ~ scales::percent(safe_pct, accuracy = 0.1),
        TRUE ~ ""
    ),

    pct_label_whole = case_when(
        metric_type == "median" ~ paste0(round(total_num), "m"),
        TRUE ~ scales::percent(safe_pct, accuracy = 1)
    ),
    
    # Construct the full narrative string: "85% - The time from..."
    descriptive_text = case_when(
        metric_type == "count" ~ format(total_num, big.mark = ",", scientific = FALSE),
        is.na(total_den) | total_den == 0 ~ "Data unavailable",
        metric_type == "median" ~ paste0(round(total_num), "m (Median) ", raw_desc),
        TRUE ~ paste0(scales::percent(safe_pct, accuracy = 0.1), " ", raw_desc)
    )
  ) %>% ungroup()


# -----------------------------------------------------------------------------
# 5. Poster Generation Loop
# -----------------------------------------------------------------------------
# Load the template once
poster_template <- read_pptx(template_path)

# Clear any existing slides locally to ensure clean state
while (length(poster_template) > 0L) {
  poster_template <- remove_slide(poster_template)
}

# Analyze layout to validate placeholder availability
layout_info <- layout_properties(poster_template, layout = "Title Slide", master = "Office Theme")
valid_labels <- layout_info$ph_label

hospitals <- unique(poster_summary_data$hospital_name)
message(paste("Generating posters for:", paste(hospitals, collapse=", ")))

# Helper to safely set placeholder
safe_ph_with <- function(doc, value, label) {
   if (!label %in% valid_labels) return(doc)
   tryCatch({
     ph_with(doc, value = value, location = ph_location_label(ph_label = label))
   }, error = function(e) {
     message(paste("Failed to set placeholder:", label, "-", e$message))
     return(doc)
   })
}

for (hosp in hospitals) {
  
  # --- A. Initialize Slide ---
  current_poster <- poster_template
  while (length(current_poster) > 0L) {
    current_poster <- remove_slide(current_poster, index = 1)
  }
  current_poster <- add_slide(current_poster, layout = "Title Slide", master = "Office Theme")
  
  # --- B. Filters for Current Hospital ---
  h_data <- poster_summary_data %>% filter(hospital_name == hosp)
  row1 <- h_data[1,]
  
  # --- C. Header Date Range Information, shows the reporting period --- 
  date_txt <- paste0(format(row1$hosp_start, "%b %y"), " to ", format(row1$hosp_latest, "%b %y"))
  title_txt <- paste0(hosp, " (", date_txt, ")")
  

  if ("ph_hosp_date_range" %in% valid_labels) {
    current_poster <- ph_with(current_poster, value = title_txt, location = ph_location_label(ph_label = "ph_hosp_date_range"))
  }
  
  # --- D. Insert Indicators ---
  for (ind in unique(h_data$indicator_id)) {
    ppt_ph <- indicator_mapping[ind]
    
    if (!is.na(ppt_ph)) {
        ind_row <- h_data %>% filter(indicator_id == ind)
        
        # Insert Narrative Text (or Number for VOL_PCI)
        if (ppt_ph %in% valid_labels) {
            current_poster <- ph_with(current_poster, value = ind_row$descriptive_text, location = ph_location_label(ph_label = ppt_ph))
        }
        
        # Special Logic for VOL_PCI: Also populate the secondary 'ph_pci' placeholder with narrative
        if (ind == "VOL_PCI" && "ph_pci" %in% valid_labels) {
            long_text <- paste0(ind_row$descriptive_text, " procedures were undertaken")
            current_poster <- ph_with(current_poster, value = long_text, location = ph_location_label(ph_label = "ph_pci"))
        }
        
        # Insert Big Number % (Shape name usually has _pct suffix)
        ppt_pct_ph <- paste0(ppt_ph, "_pct")
        if (ind != "VOL_PCI") {
           current_poster <- safe_ph_with(current_poster, ind_row$pct_label_whole, ppt_pct_ph)
        }
    }
  }
  
  # --- E. Logo / Map Insertion ---
  # Priority: 1. Official Logo (PNG), 2. Generated Map (PNG)
  
  # Clean hospital name (snake_case) for filename matching
  clean_name <- tolower(gsub("[^[:alnum:]]", "_", hosp))
  clean_name <- gsub("__", "_", clean_name)
  
  # Construct paths using clean name
  logo_path <- file.path(logo_folder, paste0(clean_name, ".png"))
  map_path  <- file.path(map_folder, paste0(clean_name, "_map.png"))


  if ("ph_hosp_logo" %in% valid_labels) {
      if (file.exists(logo_path)) {
          current_poster <- safe_ph_with(current_poster, external_img(logo_path), "ph_hosp_logo")
      } else if (file.exists(map_path)) {
          # Provide styling feedback to let valid placeholder know we are filling it with a map
          current_poster <- safe_ph_with(current_poster, external_img(map_path), "ph_hosp_logo")
      }
  }

  # --- F. Footer / Production Info ---
  prod_text <- paste0("Produced on ", format(Sys.Date(), "%d %B %Y"))
  current_poster <- safe_ph_with(current_poster, prod_text, "ph_poster_production_date")
  
  # --- G. Save Output ---
  # HQIU Style: Lowercase filenames
  out_name <- tolower(paste0(gsub(" ", "_", hosp), "_pci_poster.pptx"))
  out_file <- file.path(output_poster_dir, out_name)
  
  print(current_poster, target = out_file)
  message(paste("Saved:", out_file))
}

message("All posters generated successfully.")
