################################################################################
##          Uses National Cardiac Registry (NCR) styling (Grey land, NCR Red dots)
##          and a custom Project Logo Pin for the focal point.
##
## Author: Richard Gillett, Head of Healthcare Quality Intelligence Unit (HQIU)
## Date: January 2026
##
## Overview:
## 1. Reads a reference list of hospitals (csv).
## 2. Geocodes addresses using OpenStreetMap (Nominatim).
## 3. Caches results to 'ncr_geo_cache.csv'.
## 4. Uses ggplot2 + sf + ggimage to draw:
##    - Australia base map (National Cardiac Registry (NCR) Grey #E6E7E8).
##    - Scatter points for all hospitals (NCR Red #D1202F).
##    - Custom Image Pin for the target hospital.
## 5. Exports PNG files for use in the Poster Generator.
################################################################################

# -----------------------------------------------------------------------------
# 1. Library Maintenance and Setup
# -----------------------------------------------------------------------------
# Load required geospatial and plotting libraries
library(sf)             # Simple Features for spatial data
library(tidygeocoder)   # For geocoding hospital addresses
library(rnaturalearth)  # For base map data (Australia)
library(ggplot2)        # For map plotting
library(readr)          # For efficient CSV reading
library(dplyr)          # For data wrangling
library(ggimage)        # For custom image markers

# Set global timeout for HTTP requests (120 seconds)
# This affects R's internal download.file and similar functions
options(timeout = 120)

# Also set libcurl timeout through environment (backup approach)
Sys.setenv("LIBCURL_TIMEOUT" = "120")

# Define Visual Constants (NCR Style Guide)
NCR_LAND    <- "#E6E7E8"
NCR_PRIMARY <- "#D1202F" # NCR Red (Primary)

# -----------------------------------------------------------------------------
# 2. Paths and Data Loading
# -----------------------------------------------------------------------------
# Check if running from project root or inside the directory
# Robust check: Look for the reference file or map_generator itself if referenced via path
if (file.exists(file.path("reference_files", "hospitals.csv"))) {
  base_dir <- "."
} else if (file.exists(file.path("pci_poster", "reference_files", "hospitals.csv"))) {
  base_dir <- "pci_poster"
} else {
  # Fallback to local if we are INSIDE the folder structure (e.g. setwd to reference_files)
  # But assuming standard execution:
  if(file.exists("map_generator.R")) {
     base_dir <- "."
  } else {
     base_dir <- "pci_poster"
  }
}

# Set reference directories for the reference files, maps and images

ref_dir  <- file.path(base_dir, "reference_files")
map_dir  <- file.path(base_dir, "hospital_specific_maps")
images_dir <- file.path(base_dir, "images")

# Ensure output directory exists
if (!dir.exists(map_dir)) {
  dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
}

# Input file: participating hospitals
hospital_list_file <- file.path(ref_dir, "hospitals.csv")

# Cache file: stored coordinates from previous runs
# This stops us from having to geocode every time we run the script
cache_file         <- file.path(ref_dir, "ncr_geo_cache.csv")

# LOGO PIN ASSET
# this is the 'pin' on the map which shows where the hospital is
# whihch is used if we don't have the logo

pin_path <- file.path(images_dir, "cardiac_pin.png")

if (!file.exists(pin_path)) {
   warning("Cardiac Pin image not found in images folder! Generating fallback maps.")
   use_custom_pin <- FALSE
} else {
   use_custom_pin <- TRUE
}

# -----------------------------------------------------------------------------
# 3. Geocoding Logic (with Caching)
# -----------------------------------------------------------------------------
message("Reading hospital list...")
hospitals_df <- read_csv(hospital_list_file, show_col_types = FALSE)

# Attempt to load cache
if (file.exists(cache_file)) {
    geo_cache <- read_csv(cache_file, show_col_types = FALSE)
} else {
    geo_cache <- data.frame(hospital_name = character(), lat = numeric(), long = numeric())
}

# Identify new hospitals that are not in the cache
new_hospitals <- hospitals_df %>%
    filter(!hospital_name %in% geo_cache$hospital_name)

if (nrow(new_hospitals) > 0) {
    message(paste("Geocoding", nrow(new_hospitals), "new sites..."))
    
    # Try geocoding with error handling for network issues
    geocoded_results <- tryCatch({
        # Geocode using OSM (Nominatim) with extended timeout
        new_hospitals %>%
            geocode(address = address, method = 'osm', lat = lat, long = long, 
                    verbose = TRUE, custom_query = list(timeout = 120))
    }, error = function(e) {
        warning(paste("Geocoding failed:", e$message))
        warning("Proceeding with cached data only. Maps will only be generated for cached hospitals.")
        return(NULL)
    })
    
    if (!is.null(geocoded_results) && nrow(geocoded_results) > 0) {
        # Merge and update cache
        full_cache <- bind_rows(geo_cache, geocoded_results) %>%
            filter(!is.na(lat)) # Drop failed geocodes
            
        write_csv(full_cache, cache_file)
        message("Cache updated.")
        
        # Use the full updated set for plotting
        plot_data <- full_cache
    } else {
        message("Using cached geodata only (geocoding unavailable).")
        plot_data <- geo_cache
    }
} else {
    message("Using cached geodata.")
    plot_data <- geo_cache
}

# Convert to SF object for consistent spatial plotting
sites_sf <- st_as_sf(plot_data, coords = c("long", "lat"), crs = 4326)

# -----------------------------------------------------------------------------
# 4. Base Map Preparation
# -----------------------------------------------------------------------------
# Fetch map of Australia
# Try to get states (requires rnaturalearthhires), fallback to country outline
oz_states <- tryCatch({
  ne_states(country = "australia", returnclass = "sf")
}, error = function(e) {
  message("High-res state data not available. Falling back to country outline.")
  ne_countries(country = "australia", returnclass = "sf", scale = "medium")
})

# -----------------------------------------------------------------------------
# 5. Map Generation Loop
# -----------------------------------------------------------------------------
message("Generating maps...")

for (i in 1:nrow(plot_data)) {
    current_site <- plot_data[i, ]
    site_name    <- current_site$hospital_name
    
    # Sanitize filename
    clean_name <- tolower(gsub("[^[:alnum:]]", "_", site_name))
    clean_name <- gsub("__", "_", clean_name)
    out_path   <- file.path(map_dir, paste0(clean_name, "_map.png"))
    
    # Prepare plot data for the single focal point
    focal_df <- data.frame(
      long = current_site$long,
      lat  = current_site$lat,
      image = pin_path
    )
    
    # --- Plotting Logic ---
    p <- ggplot() +
      # A. Base Layer: Australia (NCR Grey)
      geom_sf(data = oz_states, fill = NCR_LAND, color = "white", size = 0.2) +
      
      # B. Context Layer: All other hospitals (NCR Red, size increased 33%)
      geom_sf(data = sites_sf, color = NCR_PRIMARY, size = 2.7, alpha = 0.5) +
      
      # C. Focal Layer: CUSTOM IMAGE PIN
      # Use geom_image if available, else fallback
      {
         if (use_custom_pin) {
             geom_image(data = focal_df, aes(x = long, y = lat, image = image), size = 0.18)
         } else {
             geom_point(data = focal_df, aes(x = long, y = lat), color = "red", size = 5)
         }
      } +

      # D. Theme and Layout
      theme_void() + # Remove axes and backgrounds for clean insertion
      coord_sf(xlim = c(112, 154), ylim = c(-45, -10)) # Focus on Australia Mainframe
      
    # --- Export ---
    # Save as PNG with transparent background
    ggsave(out_path, plot = p, width = 6, height = 6, dpi = 300, bg = "transparent")
    
    message(paste("Saved map for:", site_name))
}

message("All maps generated successfully.")
