################################################################################
## Name: utilities.R
## Purpose: Helpful utility functions used in the reports
################################################################################

# Prepare PCI Data #############################################################
prepare_pci_data <- function(pci_data) {

  # Filter out records that are missing hid
  pci_data <- pci_data |>
    dplyr::filter(!is.na(hid))

  # Set the period start and end dates
  pci_data <- pci_data |>
    dplyr::mutate(period_start = lubridate::floor_date(dop, unit = "month"),
                  period_end = lubridate::ceiling_date(period_start, unit = "month")-1)

  # Creating variables for demographics
  pci_data <- pci_data |>
    dplyr::mutate(
      # Assign text or category labels to demographic columns
      # Sex
      sex_cat = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
      # Age
      age_cat = dplyr::case_when(
        age < 50 ~ "< 50 yrs",
        age >= 51 & age <= 60 ~ "51-60 yrs",
        age >= 61 & age <= 70 ~ "61-70 yrs",
        age >= 71 & age <= 80 ~ "71-80 yrs",
        age > 80 ~ "> 80 yrs"
      ),
      # Set as factor so that label ordering will be respected
      age_cat = factor(age_cat, levels = c("< 50 yrs", "51-60 yrs",
                                           "61-70 yrs", "71-80 yrs", "> 80 yrs")),
      inds_cat = factor(inds, levels = c(0, 1, 2, 3, -1), labels =  c("Neither", "Aboriginal",
                                                                      "Torres Strait Islander", "Both",
                                                                     "Unknown")),
      acs_cat = dplyr::case_when(
        acst == 3 ~ "STEMI",
        acst %in% c(1,2) ~ "ACS",
        acs == 0 ~ "Elective/Other"
      ),
      bmi = wkg/(htm/100)^2,
      severe_obesity = dplyr::if_else(bmi >= 35, 1, 0),
      pvd = (pvd1 == 1 | pvd2 == 1),
      pvd = dplyr::if_else(is.na(pvd), 0, as.numeric(pvd)),
      los = difftime(dod, doa, units = "days"),
      los_cat = dplyr::case_when(
        los == 0 ~ "Same-day Discharge",
        los >= 1 & los <= 5 ~ "1-5 days",
        los > 5 ~ "6+ days"
      ),
      los_cat = factor(los_cat, levels = c("Same-day Discharge", "1-5 days", "6+ days"))
    )

  #Creating required columns for Indicator calculations
  pci_data <- pci_data |>
    dplyr::mutate(
      #Create dbd based on whether the tbd time occurred before/after top time
      dbd = dplyr::case_when(
        tbd <= top ~ dop + lubridate::days(1),
        tbd > top ~ dop,
        tbd = NA ~ NA),
      #Calculate column required for ECG to PCI time
      ecgdb = difftime(tbd |> as.numeric(), tecgd |> as.numeric(), units = "mins") + difftime(dbd |> as.numeric(), decgd |> as.numeric(), units = "mins"),
      #Calculate column required for Door to PCI time
      dbdt = difftime(tbd |> as.numeric(), toa |> as.numeric(), units = "mins") + difftime(dbd |> as.numeric(), doa |> as.numeric(), units = "mins"),
      #Calculate column required for Symptoms to Door time
      symptom_to_door = difftime(toa |> as.numeric(), tso |> as.numeric(), units = "mins") + difftime(doa |> as.numeric(), dso |> as.numeric(), units = "mins"),
      #Calculate if patient was an inpatient at time of ACS
      inp = dplyr::case_when(
        acs == "1" & symptom_to_door > 0 ~ "0",
        acs == "1" & symptom_to_door <= 0 ~ "1",
        acs == "0" ~ NA))

  # Phase 1: Hospital Classifications and Additional Variables
  # Hospital volume classification
  hospital_volumes <- pci_data |>
    dplyr::group_by(hid) |>
    dplyr::summarise(
      procedures = dplyr::n(),
      date_range_days = as.numeric(max(dop, na.rm = TRUE) - min(dop, na.rm = TRUE)),
      annual_volume = dplyr::if_else(date_range_days > 0,
                                      procedures * 365 / date_range_days,
                                      procedures),
      .groups = "drop"
    ) |>
    dplyr::mutate(hospital_volume_cat = dplyr::case_when(
      annual_volume < 250 ~ "Low (<250)",
      annual_volume <= 500 ~ "Medium (250-500)",
      annual_volume > 500 ~ "High (>500)",
      TRUE ~ "Unknown"
    ))

  pci_data <- pci_data |>
    dplyr::left_join(hospital_volumes |> dplyr::select(hid, hospital_volume_cat, annual_volume),
                     by = "hid")

  # Arterial access route (pel = Percutaneous Entry Location)
  pci_data <- pci_data |>
    dplyr::mutate(
      access_route = dplyr::case_when(
        # pel == 1 ~ "Brachial", # Brachial is unused
        pel == 2 ~ "Radial",
        pel == 3 ~ "Femoral",
        TRUE ~ "Unknown"
      ),
      access_route = factor(access_route, levels = c(
        "Radial",
        "Femoral",
        # "Brachial",
        "Unknown"
        )),
      # Radial access flag for analysis
      radial_access = dplyr::if_else(pel == 2, 1, 0, missing = 0)
    )

  # Pre-hospital notification
  pci_data <- pci_data |>
    dplyr::mutate(
      prehosp_notif = dplyr::if_else(phn == 1, 1, 0, missing = 0)
    )

  # Drug-eluting stent usage (lr*_sit fields)
  pci_data <- pci_data |>
    dplyr::mutate(
      des_used = dplyr::if_else(
        lr1_sit %in% c(2, 3) | lr2_sit %in% c(2, 3) |
        lr3_sit %in% c(2, 3) | lr4_sit %in% c(2, 3) |
        lr5_sit %in% c(2, 3),
        1, 0, missing = 0
      ),
      # BMS only (excludes DES and mixed)
      bms_only = dplyr::if_else(
        (lr1_sit == 1 | is.na(lr1_sit)) &
        (lr2_sit == 1 | is.na(lr2_sit)) &
        (lr3_sit == 1 | is.na(lr3_sit)) &
        (lr4_sit == 1 | is.na(lr4_sit)) &
        (lr5_sit == 1 | is.na(lr5_sit)) &
        (lr1_sit == 1 | lr2_sit == 1 | lr3_sit == 1 |
         lr4_sit == 1 | lr5_sit == 1),
        1, 0, missing = 0
      ),
      # In-stent restenosis
      any_isr = dplyr::if_else(
        lr1_isr == 1 | lr2_isr == 1 | lr3_isr == 1 |
        lr4_isr == 1 | lr5_isr == 1,
        1, 0, missing = 0
      ),
      # Number of lesions treated
      num_lesions = (!is.na(lr1_lesion)) + (!is.na(lr2_lesion)) +
                    (!is.na(lr3_lesion)) + (!is.na(lr4_lesion)) +
                    (!is.na(lr5_lesion)),
      multi_vessel = dplyr::if_else(num_lesions >= 2, 1, 0)
    )

  # Additional timing variables
  pci_data <- pci_data |>
    dplyr::mutate(
      # First medical contact to device time
      fmc_to_device = difftime(tbd |> as.numeric(), tfmc |> as.numeric(), units = "mins") +
                      difftime(dbd |> as.numeric(), dfmc |> as.numeric(), units = "mins"),
      # Symptom onset to reperfusion time
      symptom_to_reperfusion = difftime(tbd |> as.numeric(), tso |> as.numeric(), units = "mins") +
                               difftime(dbd |> as.numeric(), dso |> as.numeric(), units = "mins"),
      # Out-of-hours procedure (weekends or outside 8am-6pm)
      out_of_hours = dplyr::if_else(
        lubridate::wday(dop) %in% c(1, 7) |  # Saturday=7, Sunday=1
        top < hms::as_hms("08:00:00") |
        top > hms::as_hms("18:00:00"),
        1, 0, missing = 0
      )
    )

  # Primary PCI identification
  # Using clinical definition: STEMI + PCI within 12 hours + not inter-hospital transfer
  # (Assumes no lysis at referring hospital for non-transferred patients)
  pci_data <- pci_data |>
    dplyr::mutate(
      primary_pci = dplyr::if_else(
        acst == 3 &                   # STEMI
        pci == 1 &                    # PCI performed
        symptom_to_door < 720 &       # Within 12 hours
        iht == 0,                     # Not inter-hospital transfer
        1, 0, missing = 0
      )
    )

  #Time from diagnostic ECG to PCI mediated reperfusion
  pci_data <- pci_data |>
    dplyr::mutate(NCR1_den = dplyr::if_else(pci == "1" &
                  inp == "0" &
                  iht == "0" &
                  ecgdb > 0 &
                  symptom_to_door < 720, 1, 0))

  #Time from door to PCI mediated reperfusion
  pci_data <- pci_data |>
    dplyr::mutate(NCR2_den = dplyr::if_else(pci == "1" &
                  inp == "0" &
                  iht == "0" &
                  dbdt > 0 &
                  symptom_to_door < 720, 1, 0))

  #Peri-PCI stroke
  pci_data <- pci_data |>
  dplyr::mutate(NCR3_den = dplyr::if_else(ihstr == 0 | ihstr == 1, 1,0))

  #In-hospital major bleeding
  pci_data <- pci_data |>
    dplyr::mutate(NCR4_den = dplyr::if_else(stringr::str_detect(ihbl, "[012345678]"), 1, 0, missing = 0),
                  NCR4_num = dplyr::if_else(NCR4_den == 1 &
                                            stringr::str_detect(ihbl, "[34578]"), 1, 0, missing = 0))

  #In-hospital mortality
  pci_data <- pci_data |>
    dplyr::mutate(NCR5_den = dplyr::if_else(stringr::str_detect(dis, "[123456]"), 1, 0, missing = 0),
                  NCR5_num = dplyr::if_else(dis == "6", 1, 0))

  #30 day unplanned cardiac readmission rate after PCI
  pci_data <- pci_data |>
    dplyr::mutate(NCR6_den = dplyr::if_else(
        stringr::str_detect(dis, "[12345]") &
        (stat30 == "1" | stat30 == "0") &
        (crh30 == "1"  | crh30 == "0"), 1, 0),
                  NCR6_num = dplyr::if_else(NCR6_den == 1 &
                                crh30 == "1" &
                                pc30 == "0", 1, 0))

  #Unplanned revascularisation within 30 days
  pci_data <- pci_data |>
    dplyr::mutate(NCR7_den = dplyr::if_else(
      (ihpci == "0" | ihpci == "1") &
      (ihcab == "0" | ihcab == "1"), 1, 0, missing = 0),
                  NCR7_num = dplyr::if_else(NCR7_den == 1 &
                    (ihpci == "1" & ihpcip == "0") |
                    (ihcab == "1" & ihpcab == "0" & ihtvcab == "1") |
                    (pc30 == "0" & pci30 == "1") |
                    (pc30 == "0" & cab30 == "1"), 1, 0, missing = 0))

  #30 day mortality after PCI
  pci_data <- pci_data |>
    dplyr::mutate(NCR8_den = dplyr::if_else(
      stringr::str_detect(dis, "[123456]"), 1, 0, missing = 0),
                  NCR8_num = dplyr::if_else((dop + 30) <= dmort30 |
                                            dis == "6", 1, 0, missing = 0))

  #Patients without contraindication discharged on lipid-lowering therapy
  pci_data <- pci_data |>
    dplyr::mutate(NCR9_den = dplyr::if_else(stringr::str_detect(dis, "[12345]") &
                                            ((dstp == "0" | dstp == "1") |
                                            (doll == "0" | doll == "1")), 1, 0, missing = 0),
                  NCR9_num = dplyr::if_else(NCR9_den == 1 &
                                            (dstp == "1" | doll == "1"), 1, 0, missing = 0))

  #Patients referred to cardiac rehabilitation or other secondary prevention program
  pci_data <- pci_data |>
    dplyr::mutate(NCR10_den = dplyr::if_else(stringr::str_detect(dis, "[12345]") &
                                             (crehab == "-1" | crehab == "1" | crehab == "0"), 1, 0, missing = 0),
                  NCR10_num = dplyr::if_else(NCR10_den == 1 &
                                             crehab == "1", 1, 0, missing = 0))

  #Proportion of patients without a clear and documented contraindication for aspirin and/or a P2Y12 inhibitor, discharged on DAPT
  pci_data <- pci_data |>
    dplyr::mutate(NCR11_den = dplyr::if_else(stringr::str_detect(dis, "[12345]") &
                                             ((dasp == "0" | dasp == "1") |
                                             (doap == "0" | doap == "1")), 1, 0, missing = 0),
                  NCR11_num = dplyr::if_else(NCR9_den == 1 &
                                             (dasp == "1" & doap == "1"), 1, 0, missing = 0))

  #Composite Outcomes
  pci_data <- pci_data |>
    dplyr::mutate(
      # MACE: Major Adverse Cardiac Events (Death OR in-hospital MI OR unplanned revascularisation)
      MACE = dplyr::if_else(
        NCR5_num == 1 | ihmi == "1" | NCR7_num == 1,
        1, 0, missing = 0
      ),
      # MACCE: Major Adverse Cardiac and Cerebrovascular Events (MACE OR stroke)
      MACCE = dplyr::if_else(
        MACE == 1 | ihstr == 1,
        1, 0, missing = 0
      )
    )

  pci_data
}

# Controlcharts helper functions to have consistent formatting #################
controlcharts_spc <- function(
    data,
    numerator,
    denominator,
    month,
    title = NULL,
    chart_type = "p",
    improvement_direction = "decrease",
    height = 340,
    width = 370,
    xlimit_label = NULL,
    ylimit_label = NULL,
    ylimit_sig_figs = 0,
    ll_truncate = NULL,
    ylimit_tick_count = 5,
    upper_padding = 30
) {
  # Pre-aggregate data for SPC chart
  data_summary <- data |>
    dplyr::filter(!is.na({{month}})) |>
    dplyr::group_by({{month}}) |>
    dplyr::summarise(
      numerator = sum({{numerator}}, na.rm = TRUE),
      denominator = sum({{denominator}}, na.rm = TRUE)
    ) |>
    purrr::set_names(c("month", "numerator", "denominator"))

  # Create initial SPC chart to identify patterns
  spc_chart_init <- controlcharts::spc(
    data = data_summary,
    numerators = numerator,
    denominators = denominator,
    keys = month,
    spc_settings = list(
      chart_type = chart_type,
      multiplier = 1,
      sig_figs = 1
    ),
    outlier_settings = list(
      process_flag_type = "deterioration",
      improvement_direction = improvement_direction,
      astronomical = TRUE,
      shift = TRUE,
      two_in_three = TRUE,
      trend = TRUE
    )
  )

  # Parse the patterns to identify the last pattern
  spc_chart_init_data <-
    spc_chart_init$limits[, c('date', 'numerator', 'denominator', 'astpoint',
                              'trend', 'shift', 'two_in_three')]
  spc_chart_init_data$date <-
    as.Date(spc_chart_init_data$date, format = "%d/%m/%Y")

  last_pattern <- function(x) {
    output <- rep(NA, length(x))

    match_pattern <- seq_len(length(x))[x != "none"]
    if (length(match_pattern) > 0) {
      last_pattern_index <- max(match_pattern)
      output[last_pattern_index] <- x[last_pattern_index]
    }
    output
  }
  spc_chart_init_data$last_astpoint <- last_pattern(spc_chart_init_data$astpoint) #nolint
  spc_chart_init_data$last_trend <- last_pattern(spc_chart_init_data$trend)
  spc_chart_init_data$last_shift <- last_pattern(spc_chart_init_data$shift)
  spc_chart_init_data$last_two_in_three <- last_pattern(spc_chart_init_data$two_in_three) #nolint

  # Assign pattern labels to the identified patterns
  spc_chart_init_data$pattern_labels <-
    apply(spc_chart_init_data[, c('numerator', 'denominator',
                                  'last_astpoint', 'last_trend',
                                  'last_shift', 'last_two_in_three')],
          1,
          function(x) {
            patterns <- c()
            if (!is.na(x["last_astpoint"])) patterns <- c(patterns, "\u2252")
            if (!is.na(x["last_trend"])) patterns <- c(patterns, "\u24e3")
            if (!is.na(x["last_two_in_three"])) patterns <- c(patterns, "\u2154") #nolint
            if (!is.na(x["last_shift"])) patterns <- c(patterns, "\u24e2")
            if (!is.na(x["last_astpoint"])) {
              if (length(unique(data_summary$denominator)) == 1) {
                patterns <- c(patterns, paste0("(", x["numerator"], ")"))

              } else {
                patterns <- c(patterns, paste0("(", x["numerator"], "/", trimws(x["denominator"]), ")"))
              }
            }
            if (length(patterns) > 0) {
              paste0(patterns, collapse = "")
            } else {
              ""
            }
          })

  # Label positions
  spc_chart_init_data <- spc_chart_init_data |>
    dplyr::mutate(
      label_position = ifelse(
        numerator/denominator <= sum(numerator) / sum(denominator),
        'bottom',
        'top'
      )
    )

  # Re-render the spc chart with the pattern_labels
  controlcharts::spc(
    data = spc_chart_init_data,
    numerators = numerator,
    denominators = denominator,
    keys = date,
    labels = pattern_labels,
    title = list(text = title, y = 30),
    height = height,
    width = width,
    canvas_settings = list(
      upper_padding = upper_padding,
      lower_padding = 0,
      left_padding = 10,
      right_padding = 30
    ),
    spc_settings = list(
      chart_type = chart_type,
      multiplier = 1,
      sig_figs = 1,
      ll_truncate = ll_truncate
    ),
    outlier_settings = list(
      process_flag_type = "both",
      improvement_direction = improvement_direction,
      astronomical = TRUE,
      shift = TRUE,
      two_in_three = TRUE,
      trend = TRUE
    ),
    nhs_icon_settings = list(
      show_variation_icons = TRUE,
      flag_last_point = TRUE,
      variation_icons_locations = "Top Right",
      variation_icons_scaling = 1.5
    ),
    scatter_settings = list(
      size = 2.5
    ),
    x_axis_settings = list(
      xlimit_ticks = TRUE,
      xlimit_tick_count = 5,
      xlimit_tick_font = "'Arial', sans-serif",
      xlimit_tick_size = 10,
      xlimit_tick_colour = "#000000",
      xlimit_tick_rotation = -35,
      xlimit_label = xlimit_label,
      xlimit_label_font = "'Arial', sans-serif",
      xlimit_label_size = 12,
      xlimit_label_colour = "#000000"
    ),
    y_axis_settings = list(
      ylimit_sig_figs = ylimit_sig_figs,
      ylimit_l = NULL,
      ylimit_u = NULL,
      ylimit_ticks = TRUE,
      ylimit_tick_count = ylimit_tick_count,
      ylimit_tick_font = "'Arial', sans-serif",
      ylimit_tick_size = 12,
      ylimit_tick_colour = "#000000",
      ylimit_tick_rotation = 0,
      ylimit_label = ylimit_label,
      ylimit_label_font = "'Arial', sans-serif",
      ylimit_label_size = 12,
      ylimit_label_colour = "#000000"
    ),
    date_settings = list(
      date_format_day = "(blank)",
      date_format_month = "Mon",
      date_format_year = "YY",
      date_format_delim = " ",
      date_format_locale = "en-GB"
    ),
    label_settings = list(
      show_labels = TRUE,
      label_position = spc_chart_init_data$label_position,
      label_y_offset = 10 * sapply(spc_chart_init_data$label_position,
                                   function(x) ifelse(x == "top", 1, -1)),
      label_line_offset = 5,
      label_angle_offset = 0,
      label_font = "'Arial', sans-serif",
      label_size = 15,
      label_colour = "#000000",
      label_line_max_length = 30,
      label_marker_show = TRUE,
      label_marker_offset = 5,
      label_marker_size = 3,
      label_marker_colour = "#000000",
      label_marker_outline_colour = "#000000"
    )
  )
}

controlcharts_funnel <- function(
  data,
  numerator,
  denominator,
  group,
  group_name,
  title = NULL,
  labels = NULL,
  chart_type = "PR",
  improvement_direction = "increase",
  height = 340,
  width = 370,
  xlimit_label = NULL,
  ylimit_label = NULL,
  ylimit_sig_figs = 0,
  ylimit_tick_count = 5,
  upper_padding = 30
) {
  # Pre-aggregate data for SPC chart
  fp_data <- data |>
    dplyr::filter(!is.na({{group}})) |>
    dplyr::group_by({{group}}, {{group_name}}) |>
    dplyr::summarise(
      num = sum({{numerator}}, na.rm = TRUE),
      den = sum({{denominator}}, na.rm = TRUE)
    ) |>
    # Replace column names to use common names
    purrr::set_names(c("group", "group_name", "num", "den"))
  fp_data <- fp_data |>
    dplyr::mutate(
      # Assign labels
      label = ifelse(group %in% labels, stringr::str_split_i(group_name, " ", -1), ""),
      # Assign label position
      label_position = ifelse(
        group %in% labels,
        ifelse(
          num/den <= (sum(fp_data$num) / sum(fp_data$den)),
          'bottom',
          'top'
        ),
        NA
      )
    ) |>
    dplyr::mutate(
      # Assign label y offset
      label_y_offset = ifelse(
        group %in% labels,
        ifelse(label_position == 'top', 20, -20),
        NA
      ),
      # Assign label angle offset
      label_angle_offset = ifelse(
        group %in% labels,
        45 * ifelse(label_position == 'top', -1, 1) *
          ifelse(den <= max(den)/2, 1,-1),
        NA
      )
    ) |>
    dplyr::mutate(
      shape = ifelse(group %in% labels, "Triangle", "Circle"),
      size = ifelse(group %in% labels, 2.5, 2.5)
    )

  # Add funnel chart
  controlcharts::funnel(
    data = fp_data,
    numerators = num,
    denominators = den,
    keys = group,
    labels = label,
    title = list(text = title, y = 30),
    height = height,
    width = width,
    canvas_settings = list(
      upper_padding = upper_padding,
      lower_padding = 0,
      left_padding = 0,
      right_padding = 30
    ),
    funnel_settings = list(
      chart_type = chart_type,
      od_adjust = "no",
      multiplier = 1,
      sig_figs = 1
    ),
    outlier_settings = list(
      process_flag_type = "both",
      improvement_direction = improvement_direction,
      three_sigma = TRUE
    ),
    scatter_settings = list(
      shape = fp_data$shape,
      size = fp_data$size
    ),
    x_axis_settings = list(
      xlimit_ticks = TRUE,
      xlimit_tick_count = 3,
      xlimit_tick_font = "'Arial', sans-serif",
      xlimit_tick_size = 12,
      xlimit_tick_colour = "#000000",
      xlimit_ticks = FALSE,
      xlimit_label = xlimit_label,
      xlimit_label_font = "'Arial', sans-serif",
      xlimit_label_size = 12,
      xlimit_label_colour = "#000000"
    ),
    y_axis_settings = list(
      ylimit_sig_figs = ylimit_sig_figs,
      ylimit_l = NULL,
      ylimit_u = NULL,
      ylimit_ticks = TRUE,
      ylimit_tick_count = ylimit_tick_count,
      ylimit_tick_font = "'Arial', sans-serif",
      ylimit_tick_size = 12,
      ylimit_tick_colour = "#000000",
      ylimit_label = ylimit_label,
      ylimit_label_font = "'Arial', sans-serif",
      ylimit_label_size = 12,
      ylimit_label_colour = "#000000"
    ),
    label_settings = list(
      show_labels = TRUE,
      label_position = fp_data$label_position,
      label_y_offset = fp_data$label_y_offset,
      label_line_offset = 5,
      label_angle_offset = fp_data$label_angle_offset,
      label_font = "'Arial', sans-serif",
      label_size = 12,
      label_colour = "#000000",
      label_line_max_length = 40,
      label_marker_show = TRUE,
      label_marker_offset = 0,
      label_marker_size = 3,
      label_marker_colour = "#000000",
      label_marker_outline_colour = "#000000"
    )
  )
}

no_numerator_available_plot <- function(title, text = "No data available") {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 0.5, size = 12, face = "bold", family = "sans"),
      plot.margin = ggplot2::margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm")
    ) +
    ggplot2::annotate("text", x = 0.5, y = 0.3, label = text, size = 4, hjust = 0.5, vjust = 0.3)
}

# Convert Word to PDF ----------------------------------------------------------
# Helper function to convert docx to pdf, dodging bugs in doconv library
doconv_to_pdf_fixed <- function(
    input,
    output = gsub("\\.[[:alnum:]]+$", ".pdf", input)
) {

  # Extract file name
  file_name <- gsub(".*[\\/]", "", input)

  # Create temporary directory
  new_temp_dir <- tempdir()

  # Copy docx to temporary directory
  file.copy(from = input, to = new_temp_dir, overwrite = TRUE)

  # Rename to temp.docx, as the doconv function cannot handle names with spaces
  file.rename(
    from = file.path(new_temp_dir, file_name),
    to = file.path(new_temp_dir, "temp.docx")
  )

  # Convert the file to pdf
  doconv::to_pdf(
    input = file.path(new_temp_dir, "temp.docx")
  )

  # Rename the pdf file to the original file name and copy to output directory
  file.rename(
    from = file.path(new_temp_dir, "temp.pdf"),
    to = file.path(new_temp_dir, gsub("\\.[[:alnum:]]+$", ".pdf", file_name))
  )
  file.copy(
    from = file.path(new_temp_dir, gsub("\\.[[:alnum:]]+$", ".pdf", file_name)),
    to = output, overwrite = TRUE
  )

  # Clean up and remove temporary files
  file.remove(file.path(new_temp_dir, "temp.docx"))
  file.remove(
    file.path(new_temp_dir, gsub("\\.[[:alnum:]]+$", ".pdf", file_name))
  )

  # Suppress any return values
  invisible()
}
