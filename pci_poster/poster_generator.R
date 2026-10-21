################################################################################
## Project: Cardiac Registry of Western Australia (CRoWA) reporting
## Script: poster_generator.R
## Department: WA Health, Healthcare Quality Intelligence Unit (HQIU)
##
## Purpose: Creates National Cardiac Registry (NCR) Posters for hospitals
##          incorporating user-specified data and NCR-style visuals.
################################################################################

# -----------------------------------------------------------------------------
# 1. Inputs
# -----------------------------------------------------------------------------

# input_period_end <- '2026-06-30'
# input_period_start <- '2026-04-01'

# -----------------------------------------------------------------------------
# 2. Setup and Library Loading
# -----------------------------------------------------------------------------
# Load necessary packages for data manipulation and PowerPoint generation
# library(dplyr) # For data manipulation
# library(officer) # For PowerPoint creation

# Define Paths
template_path <- file.path("pci_poster", "poster_template.pptx")
output_poster_dir <- file.path("_output", "posters")
logo_folder <- file.path("reference_files", "hospital_logos")
map_folder <- file.path("reference_files", "hospital_specific_maps")
ref_dir <- file.path("reference_files")

# Create output directory if it doesn't exist
if (!dir.exists(output_poster_dir)) {
  dir.create(output_poster_dir, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------------------------------------------------------
# 3. Data Processing
# -----------------------------------------------------------------------------
poster_summary_data <- summarise_pci_indicators(
  data = pci_data_clean |>
    dplyr::filter(
      dop >= input_period_start,
      dop <= input_period_end
    ) |>
    dplyr::mutate(
      period_start = as.Date(input_period_start, format = "%Y-%m-%d"),
      period_end = as.Date(input_period_end, format = "%Y-%m-%d")
    ),
  ind_metadata = ind_desc_df
)

# Merge in indicator metadata
poster_summary_data  <- poster_summary_data |>
  merge(
    ind_desc_df |> dplyr::select(indicator_id, indicator_type, poster_label),
    by = "indicator_id"
  )
# Add value and descriptive text to be input into poster
poster_summary_data <- poster_summary_data |>
  dplyr::mutate(
    value = dplyr::case_when(
      indicator_type %in% c("median", "count") ~ formatC(num, format = "f", big.mark = ",", digits = 0),
      indicator_type == "proportion" ~ paste0(formatC(num/den*100, format = "f", digits = 0), "%"),
      .default = "N/A"
    )
  )
poster_summary_data <- poster_summary_data |>
  dplyr::mutate(
    desc = dplyr::case_when(
      # Custom treatment for VOL_PCI to include desc from PCI_DATABASE
      indicator_id == "VOL_PCI" ~ paste0(
        stringr::str_replace(poster_label, "\\{s\\}", value),
        "        ",
        stringr::str_replace(
          poster_summary_data |> dplyr::filter(indicator_id == "PCI_DATABASE") |> dplyr::pull(poster_label),
          "\\{s\\}",
          paste0(
            poster_summary_data |> dplyr::filter(indicator_id == "PCI_DATABASE") |> dplyr::mutate(text = paste0(num, " (", value, ")")) |> dplyr::pull(text)
          )
        )
      ),
      indicator_type %in% c("median", "count") ~ stringr::str_replace(poster_label, "\\{s\\}", value),
      is.na(den) | den == 0 ~ "Data unavailable",
      indicator_type == "proportion" ~ stringr::str_replace(poster_label, "\\{s\\}", paste0(num, " of ", den)),
      .default = "Data unavailable"
    )
  )

# -----------------------------------------------------------------------------
# 5. Poster Generation Loop
# -----------------------------------------------------------------------------
# Load the template once
poster_template <- officer::read_pptx(template_path)

# Clear any existing slides locally to ensure clean state
while (length(poster_template) > 0L) {
  poster_template <- officer::remove_slide(poster_template)
}

# Analyze layout to validate placeholder availability
layout_info <- officer::layout_properties(poster_template, layout = "Title Slide", master = "Office Theme")
valid_labels <- layout_info$ph_label

hospitals <- unique(poster_summary_data$hospital_name)
message(paste("Generating posters for:", paste(hospitals, collapse = ", ")))

# Helper to safely set placeholder
safe_ph_with <- function(doc, value, label) {
  if (!label %in% valid_labels) {
    return(doc)
  }
  tryCatch(
    {
      officer::ph_with(doc, value = value, location = officer::ph_location_label(ph_label = label))
    },
    error = function(e) {
      message(paste("Failed to set placeholder:", label, "-", e$message))
      return(doc)
    }
  )
}

for (hosp in hospitals) {
  # --- A. Initialize Slide (reload template fresh each time to avoid accumulation) ---
  current_poster <- officer::read_pptx(template_path)
  while (length(current_poster) > 0L) {
    current_poster <- officer::remove_slide(current_poster, index = 1)
  }
  current_poster <- officer::add_slide(current_poster, layout = "Title Slide", master = "Office Theme")

  # --- B. Filters for Current Hospital ---
  h_data <- poster_summary_data |> dplyr::filter(hospital_name == hosp)
  row1 <- h_data[1, ]

  # --- C. Header Date Range Information, shows the reporting period ---
  date_txt <- paste0(format(row1$period_start, "%b %y"), " to ", format(row1$period_end, "%b %y"))
  title_txt <- paste0(hosp, " (", date_txt, ")")


  if ("ph_hosp_date_range" %in% valid_labels) {
    current_poster <- officer::ph_with(current_poster, value = title_txt, location = officer::ph_location_label(ph_label = "ph_hosp_date_range"))
  }

  # --- D. Insert Indicators ---
  for (ind in unique(h_data$indicator_id)) {
    ppt_ph_value <- paste0("ph_", ind, "_value")
    ppt_ph_desc <- paste0("ph_", ind, "_desc")

    ind_row <- h_data |> dplyr::filter(indicator_id == ind)

    # Insert Value Text
    if (ppt_ph_value %in% valid_labels) {
      current_poster <- officer::ph_with(current_poster, value = ind_row$value, location = officer::ph_location_label(ph_label = ppt_ph_value))
    }

    # Insert Description Text
    if (ppt_ph_desc %in% valid_labels) {
      current_poster <- officer::ph_with(current_poster, value = ind_row$desc, location = officer::ph_location_label(ph_label = ppt_ph_desc))
    }
  }

  # Add mins text to NCR2
  current_poster <- officer::ph_with(current_poster, value = "mins", location = officer::ph_location_label(ph_label = "ph_NCR2_mins"))

  # --- E. Logo / Map Insertion ---
  # Priority: 1. Official Logo (PNG), 2. Generated Map (PNG)

  # Clean hospital name (snake_case) for filename matching
  clean_name <- tolower(gsub("[^[:alnum:]]", "_", hosp))
  clean_name <- gsub("__", "_", clean_name)

  # Construct paths using clean name
  logo_path <- file.path(logo_folder, paste0(clean_name, ".png"))
  map_path <- file.path(map_folder, paste0(clean_name, "_map.png"))


  if ("ph_hosp_logo" %in% valid_labels) {
    if (file.exists(logo_path)) {
      current_poster <- safe_ph_with(current_poster, officer::external_img(logo_path), "ph_hosp_logo")
    } else if (file.exists(map_path)) {
      # Provide styling feedback to let valid placeholder know we are filling it with a map
      current_poster <- safe_ph_with(current_poster, officer::external_img(map_path), "ph_hosp_logo")
    }
  }

  # --- F. Footer / Production Info ---
  prod_text <- paste0("Produced on ", format(Sys.Date(), "%d %B %Y"))
  current_poster <- safe_ph_with(current_poster, prod_text, "ph_poster_production_date")

  # --- G. Save Output ---
  # HQIU Style: Lowercase filenames
  out_name <- tolower(paste0(
    as.Date(input_period_end, format = "%Y-%m-%d") |> format.Date(format = "%Y%m"),
    "_",
    gsub(" ", "_", hosp),
    "_pci_poster.pptx"
  ))
  out_file <- file.path(output_poster_dir, out_name)

  print(current_poster, target = out_file)
  message(paste("Saved:", out_file))
}

message("All posters generated successfully.")
