# Cardiac Outcomes Registry (NCR) Reporting

Automated generation of hospital-specific quality improvement posters for the Western Australia National Cardiac Registry (NCR), maintained by the Healthcare Quality Intelligence Unit (HQIU).

## Overview
This project processes raw PCI data and generates PowerPoint posters for each hospital, visualizing key clinical quality indicators (NCR1-NCR11) such as "Door-to-Device Time", "Mortality", and "DAPT Prescription Rates".

It uses a **Two-Stage Pipeline**:
1.  **Data Processing** (`process_ncr_data.R`): Cleans raw data, applies strict clinical rules (e.g., STEMI logic), and acts as the "Source of Truth".
2.  **Poster Generation** (`poster_generator.R`): Visualizes the processed summary data into PowerPoint slides.

---

## 1. Quick Start

### Prerequisites
*   R (Version 4.0+)
*   Required Packages: `dplyr`, `readr`, `lubridate`, `stringr`, `writexl`, `officer`, `scales`, `here`

### Step 1: Place Data
Ensure your raw data file is named `pci_data.csv` and placed in the project root folder.
*If no file is found, the script will automatically generate dummy test data (`_files/fake_pci_data.csv`).*

### Step 2: Run Processing Script
Open `process_ncr_data.R` and click **Source** (or run via terminal).
```bash
Rscript process_ncr_data.R
```
**What this does:**
*   Reads `pci_data.csv`.
*   Applies inclusions/exclusions (e.g., NCR1 only includes STEMI patients).
*   Calculates derived fields (Robust Date-Time combinations).
*   **Outputs**: `_files/pci_data_processed.csv` (Row-level flags) and `_files/cardiac_indicators_summary.xlsx` (Aggregated counts).

### Step 3: Run Poster Generator
Open `poster_generator.R` and click **Source** (or run via terminal).
```bash
Rscript poster_generator.R
```
**What this does:**
*   Reads the summary Excel file from Step 2.
*   Loads the `poster_template.pptx`.
*   Inserts hospital-specific data, logos, and narrative text.
*   **Outputs**: Final PowerPoint files in `_files/posters/`.

---

## 2. Key Features & Logic

### Stricter Logic Compliance
*   **STEMI Indicators (NCR1/NCR2)**: Strictly filtered to patients where `acst == "3"` (STEMI). Non-STEMI cases are excluded.
*   **Mortality (NCR8)**: Checks for death *within* 30 days of procedure (`date_of_death <= procedure_date + 30`).
*   **Date-Time Handling**: Uses robust `combine_datetime()` logic to merge separate Date and Time columns, preventing errors around midnight crossings.

### Customization
*   **Logos**: Place hospital logo PNGs in `hospital_logos/` (Filename format: `hospital_name.png`).
*   **Maps**: Place custom map PNGs in `hospital_specific_maps/` (Filename format: `hospital_name_map.png`).

---

## 3. Contact
**Department**: WA Health, HQIU
**Author**: Richard Gillett, Head of Healthcare Quality Intelligence Unit (HQIU)
