print_ggplot <- function(x) {
  if (knitr::is_html_output()) {
    plotly::ggplotly(x)
  } else {
    x
  }
}

spc_example_data <- data.frame(
  shorthospitalname = 'Hospital',
  descriptionshort = 'Example Indicator',
  numerator = c(20,25,17,22,18,18,20,29,18,23,17,18,15,18,17,21,30,19,15,17,22,24,20,13,14,18,14,15,21,27,19,24,25,15,26,22),
  denominator = c(86,91,99,110,96,110,96,97,105,87,94,89,102,106,107,95,106,132,95,117,97,99,108,106,101,96,96,98,105,117,94,77,97,90,106,107),
  period_start = seq(from = as.Date('2019-01-01'), to = as.Date('2021-12-01'), by = 'month'),
  period_end = seq(from = as.Date('2019-02-01'), to = as.Date('2022-01-01'), by = 'month')-1,
  spccharttype = 'p',
  multiplier = 1,
  betteris = 'Lower',
  y_axis_label = 'Proportion'
)
spc_example_plotly <- function(spc_example_data) {
  controlcharts::spc(data = spc_example_data,
                     keys = period_end,
                     numerators = numerator,
                     denominators = denominator,
                     spc_settings = list(chart_type = "p"),
                     y_axis_settings = list(ylimit_label = "Proportion",
                                            ylimit_sig_figs = 0,
                                            ylimit_label_size = 14),
                     outlier_settings = list(improvement_direction = "decrease",
                                             astronomical = TRUE,
                                             shift = TRUE,
                                             trend = TRUE,
                                             two_in_three = TRUE),
                     canvas_settings = list(upper_padding = 40),
                     nhs_icon_settings = list(show_variation_icons = TRUE),
                     date_settings = list(date_format_day = "(blank)",
                                          date_format_month = "Mon",
                                          date_format_year = "YY"))
}
# Example data for different patterns
spc_example_data_astro <- spc_example_data
spc_example_data_astro[32,3] <- 30

spc_example_data_trend <- spc_example_data
spc_example_data_trend[30, 3] <- 26
spc_example_data_trend[31, 3] <- 26

spc_example_data_twothree <- spc_example_data
spc_example_data_twothree[33, 3] <- 29

spc_example_data_shift <- spc_example_data

spc_example_data_shift[29, 3] <- 26
spc_example_data_shift[31, 3] <- 21
spc_example_data_shift[34, 3] <- 21
spc_example_data_shift[36, 3] <- 25

fpl_example_data <- data.frame(
  establishment = rep(1:10, each = 12),
  shorthospitalname = c(rep(paste0('Hospital ', 1:9), each = 12), rep('Outlier', each = 12)),
  threeletteracronym = c(rep(paste0('Hospital ', 1:9), each = 12), rep('Outlier', each = 12)),
  hsp_name_short = 'HSP',
  descriptionshort = 'Example Indicator',
  numerator = c(do.call(c, lapply(c(1,1,1,2,3,5,7,9,10), function(x) rpois(12, x * 2))), rpois(12, 6 * 3.5)),
  denominator = c(do.call(c, lapply(c(1,1,1,2,3,5,7,9,10), function(x) rpois(12, x * 10))), rpois(12, 6 * 10)),
  period_start = seq(from = as.Date('2021-01-01'), to = as.Date('2021-12-01'), by = 'month'),
  period_end = seq(from = as.Date('2021-02-01'), to = as.Date('2022-01-01'), by = 'month')-1,
  funnelcharttype = 'PR',
  multiplier = 1,
  y_axis_label = 'Proportion',
  betteris = 'Lower',
  indicator = 'Indicator')

funnel_example_plotly <- function(fpl_example_data) {
  controlcharts::funnel(data = fpl_example_data,
                        keys = establishment,
                        numerators = numerator,
                        denominators = denominator,
                        y_axis_settings = list(ylimit_label = "Proportion",
                                               ylimit_sig_figs = 1,
                                               ylimit_label_size = 14),
                        x_axis_settings = list(xlimit_label = "Number of patients",
                                               xlimit_label_size = 14),
                        outlier_settings = list(improvement_direction = "decrease",
                                                three_sigma = TRUE))
}


#Set the datatypes of incoming NCR fields
#c = Character, d = Double, l = Logical, t = Time
#Other data types can be found at ??readr::cols
colTypes <- readr::cols(
hname = "c",
pcode = "c",
seifa = "c",
aria = "c",
hid = "c",
clin_id = "c",
patientid = "c",
sex = "d",
inds = "d",
doa = readr::col_date(format = '%d/%m/%Y'),
toa = "t",
age = "d",
htm = "d",
wkg = "d",
dop = readr::col_date(format = '%d/%m/%Y'),
top = "t",
po = "c",
smk = "c",
db = "d",
dbm = "c",
pvd1 = "c",
pvd2 = "c",
pcabg = "d",
dpcabg = readr::col_date(format = '%d/%m/%Y'),
ppci = "d",
dppci = readr::col_date(format = '%d/%m/%Y'),
pcr = "d",
npcr = "c",
eftp = "c",
def = readr::col_date(format = '%d/%m/%Y'),
eft = "c",
ef = "d",
efes = "c",
egfri = "d",
shock = "d",
oca = "d",
pint = "c",
acs = "c",
dso = readr::col_date(format = '%d/%m/%Y'),
tso = "t",
ntso = "c",
acst = "c",
iht = "c",
phn = "c",
tbd = "t",
spr = "c",
dfmc = readr::col_date(format = '%d/%m/%Y'),
tfmc = "t",
decgd = readr::col_date(format = '%d/%m/%Y'),
tecgd = "t",
pci = "c",
pel = "c",
gos = "c",
fut = "c",
adr = "c",
adrt_1 = "c",
adrt_2 = "c",
adrt_3 = "c",
adrt_4 = "c",
adrt_5 = "c",
adrt_6 = "c",
adrt_7 = "c",
adrt_8 = "c",
adrt_9 = "c",
pintr = "c",
vsr = "c",
vsrt = "c",
lr1_lesion = "c",
lr1_isr = "c",
lr1_isrst = "c",
lr1_lst = "c",
lr1_sit = "c",
lr2_lesion = "c",
lr2_isr = "c",
lr2_isrst = "c",
lr2_lst = "c",
lr2_sit = "c",
lr3_lesion = "c",
lr3_isr = "c",
lr3_isrst = "c",
lr3_lst = "c",
lr3_sit = "c",
lr4_lesion = "c",
lr4_isr = "c",
lr4_isrst = "c",
lr4_lst = "c",
lr4_sit = "c",
lr5_lesion = "c",
lr5_isr = "c",
lr5_isrst = "c",
lr5_lst = "c",
lr5_sit = "c",
tak = "d",
dap = "d",
tft = "d",
ihmi = "c",
ihpci = "c",
ihpcip = "c",
ihtvr = "c",
ihtlr = "c",
ihcab = "c",
ihpcab = "c",
ihtvcab = "c",
ihstr = "d",
ihstrt = "c",
ihbl = "d",
ihblsite = "c",
ihst = "c",
dis = "c",
dod = readr::col_date(format = '%d/%m/%Y'),
crehab = "c",
mortc = "c",
dasp = "c",
doap = "c",
dstp = "c",
doll = "c",
dfu30 = readr::col_date(format = '%d/%m/%Y'),
stat30 = "c",
dmort30 = readr::col_date(format = '%d/%m/%Y'),
mort30r = "c",
mi30 = "c",
st30 = "c",
nstr = "c",
nstrt = "c",
crh30 = "c",
rhdte = readr::col_date(format = '%d/%m/%Y'),
pc30 = "c",
pci30 = "c",
tvr30 = "c",
tlr30 = "c",
cab30 = "c",
tvcab30 = "c",
crh30_2 = "c",
rhdte_2 = readr::col_date(format = '%d/%m/%Y'),
pc30_2 = "c",
pci30_2 = "c",
tvr30_2 = "c",
tlr30_2 = "c",
cab30_2 = "c",
tvcab30_2 = "c",
crh30_3 = "c",
rhdte_3 = readr::col_date(format = '%d/%m/%Y'),
pc30_3 = "c",
pci30_3 = "c",
tvr30_3 = "c",
tlr30_3 = "c",
cab30_3 = "c",
tvcab30_3 = "c",
crh30_4 = "c",
rhdte_4 = readr::col_date(format = '%d/%m/%Y'),
pc30_4 = "c",
pci30_4 = "c",
tvr30_4 = "c",
tlr30_4 = "c",
cab30_4 = "c",
tvcab30_4 = "c",
crh30_5 = "c",
rhdte_5 = readr::col_date(format = '%d/%m/%Y'),
pc30_5 = "c",
pci30_5 = "c",
tvr30_5 = "c",
tlr30_5 = "c",
cab30_5 = "c",
tvcab30_5 = "c",
crh30_6 = "c",
rhdte_6 = readr::col_date(format = '%d/%m/%Y'),
pc30_6 = "c",
pci30_6 = "c",
tvr30_6 = "c",
tlr30_6 = "c",
cab30_6 = "c",
tvcab30_6 = "c"
)

# Helper: Create comparison column for tables
create_comparison_col <- function(data) {

  switch(report_context,
    "hospital" = dplyr::mutate(data,
      comparison_group = factor(
        dplyr::if_else(hid == target_id, "Target Hospital", "Other Hospitals"),
        levels = c("Target Hospital", "Other Hospitals")
      )),
    "clinician" = dplyr::mutate(data,
      comparison_group = factor(
        dplyr::if_else(clin_id == target_id, "Target Clinician", "Other Clinicians"),
        levels = c("Target Clinician", "Other Clinicians")
      )),
    "statewide" = data
  )
}

# Helper: Get table data with comparison column if needed
get_table_data <- function(data) {
  if (report_context == "statewide") return(data)
  create_comparison_col(data)
}

# Helper: Get the 'by' column for tables
get_table_by_col <- function(default_col) {
  if (report_context == "statewide") return(default_col)
  "comparison_group"
}

# Helper: Add an Overall column only for statewide context
add_overall_if_statewide <- function(tbl, report_context) {
  if (identical(report_context, "statewide")) {
    return(gtsummary::add_overall(tbl))
  }
  tbl
}

# Helper: Build outcomes table with outcomes as columns and characteristics as rows
build_outcome_table <- function(data,
                                group_col,
                                outcomes,
                                outcome_labels = NULL,
                                comparison_col = NULL,
                                group_label = NULL) {
  if (!is.character(group_col) || length(group_col) != 1) {
    stop("group_col must be a single string")
  }
  if (!group_col %in% names(data)) stop("group_col not found in data")
  if (!is.character(outcomes) || length(outcomes) == 0) stop("outcomes must be a character vector")
  missing_outcomes <- setdiff(outcomes, names(data))
  if (length(missing_outcomes) > 0) {
    stop("outcomes not found in data: ", paste(missing_outcomes, collapse = ", "))
  }
  if (!is.null(comparison_col) && !comparison_col %in% names(data)) {
    stop("comparison_col not found in data")
  }

  format_rate <- function(x) {
    n <- sum(x == 1, na.rm = TRUE)
    denom <- sum(!is.na(x))
    if (denom == 0) return("0 (NA)")
    p <- round(n / denom * 100, 1)
    sprintf("%d (%.1f%%)", n, p)
  }

  group_cols <- c(comparison_col, group_col)
  summary_df <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(outcomes), format_rate),
      .groups = "drop"
    )

  if (!is.null(group_label)) {
    summary_df <- summary_df |>
      dplyr::rename(!!group_label := dplyr::all_of(group_col))
  }

  if (!is.null(comparison_col)) {
    summary_df <- summary_df |>
      dplyr::rename(Group = dplyr::all_of(comparison_col))
  }

  if (!is.null(outcome_labels)) {
    valid_labels <- outcome_labels[names(outcome_labels) %in% names(summary_df)]
    rename_map <- stats::setNames(names(valid_labels), valid_labels)
    summary_df <- summary_df |>
      dplyr::rename(!!!rename_map)
  }

  summary_df
}

# Helper: Get funnel keys column name
get_funnel_keys <- function() {
  switch(report_context,
    "clinician" = "clin_id",
    "hospital" = "hid",
    "statewide" = "hid"
  )
}

# Helper: Get funnel opacity vector for highlighting target
get_funnel_opacity <- function(data, key_col) {
  if (report_context == "statewide") return(NULL)

  key_values <- data[[key_col]]
  dplyr::if_else(key_values == target_id, 1, 0.2)
}

# Helper: Filter data for SPC charts (only target entity in non-statewide mode)
filter_for_spc <- function(data) {
  switch(report_context,
    "hospital" = dplyr::filter(data, hid == target_id),
    "clinician" = dplyr::filter(data, clin_id == target_id),
    "statewide" = data
  )
}

# Helper: Safe numeric conversion for aggregation
as_numeric_safe <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.integer(x))
  suppressWarnings(as.numeric(as.character(x)))
}

# Helper: Pre-aggregate for funnel plots (one row per key)
preaggregate_funnel_counts <- function(data, key_col, numerator_col, denominator_col = NULL) {
  if (!is.character(key_col) || length(key_col) != 1) stop("key_col must be a single string")
  if (!is.character(numerator_col) || length(numerator_col) != 1) stop("numerator_col must be a single string")
  if (!key_col %in% names(data)) stop("key_col not found in data")
  if (!numerator_col %in% names(data)) stop("numerator_col not found in data")
  if (!is.null(denominator_col) && (!is.character(denominator_col) || length(denominator_col) != 1)) {
    stop("denominator_col must be NULL or a single string")
  }
  if (!is.null(denominator_col) && !denominator_col %in% names(data)) stop("denominator_col not found in data")

  if (is.null(denominator_col)) {
    data |>
      dplyr::group_by(.data[[key_col]]) |>
      dplyr::summarise(
        !!numerator_col := sum(as_numeric_safe(.data[[numerator_col]]), na.rm = TRUE),
        den = dplyr::n(),
        .groups = "drop"
      )
  } else {
    data |>
      dplyr::group_by(.data[[key_col]]) |>
      dplyr::summarise(
        !!numerator_col := sum(as_numeric_safe(.data[[numerator_col]]), na.rm = TRUE),
        !!denominator_col := sum(as_numeric_safe(.data[[denominator_col]]), na.rm = TRUE),
        .groups = "drop"
      )
  }
}

# Helper: Pre-aggregate for SPC p-charts (one row per period)
preaggregate_spc_counts <- function(data, period_col = "period_end", numerator_col, denominator_col = NULL) {
  if (!is.character(period_col) || length(period_col) != 1) stop("period_col must be a single string")
  if (!is.character(numerator_col) || length(numerator_col) != 1) stop("numerator_col must be a single string")
  if (!period_col %in% names(data)) stop("period_col not found in data")
  if (!numerator_col %in% names(data)) stop("numerator_col not found in data")
  if (!is.null(denominator_col) && (!is.character(denominator_col) || length(denominator_col) != 1)) {
    stop("denominator_col must be NULL or a single string")
  }
  if (!is.null(denominator_col) && !denominator_col %in% names(data)) stop("denominator_col not found in data")

  if (is.null(denominator_col)) {
    data |>
      dplyr::group_by(.data[[period_col]]) |>
      dplyr::summarise(
        !!numerator_col := sum(as_numeric_safe(.data[[numerator_col]]), na.rm = TRUE),
        den = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data[[period_col]])
  } else {
    data |>
      dplyr::group_by(.data[[period_col]]) |>
      dplyr::summarise(
        !!numerator_col := sum(as_numeric_safe(.data[[numerator_col]]), na.rm = TRUE),
        !!denominator_col := sum(as_numeric_safe(.data[[denominator_col]]), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data[[period_col]])
  }
}

# Helper: Pre-aggregate for SPC i-charts (one row per period)
preaggregate_spc_median <- function(data, period_col = "period_end", value_col) {
  if (!is.character(period_col) || length(period_col) != 1) stop("period_col must be a single string")
  if (!is.character(value_col) || length(value_col) != 1) stop("value_col must be a single string")
  if (!period_col %in% names(data)) stop("period_col not found in data")
  if (!value_col %in% names(data)) stop("value_col not found in data")

  data |>
    dplyr::filter(!is.na(.data[[period_col]])) |>
    dplyr::group_by(.data[[period_col]]) |>
    dplyr::summarise(
      !!value_col := stats::median(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::arrange(.data[[period_col]])
}


merge_settings <- function(defaults, overrides) {
  if (is.null(overrides)) return(defaults)
  utils::modifyList(defaults, overrides)
}

pci_funnel <- function(
    data,
    numerator_col,
    denominator_col = NULL,
    key_col = NULL,
    y_axis_settings = list(),
  x_axis_settings = NULL,
    outlier_settings = list(),
  scatter_settings = NULL,
    highlight_target = TRUE,
    ...) {
  if (!is.character(numerator_col) || length(numerator_col) != 1) {
    stop("numerator_col must be a single string")
  }
  if (is.null(key_col)) key_col <- get_funnel_keys()
  if (!is.character(key_col) || length(key_col) != 1) stop("key_col must be a single string")

  funnel_data <- preaggregate_funnel_counts(
    data,
    key_col = key_col,
    numerator_col = numerator_col,
    denominator_col = denominator_col
  )

  denom_name <- if (is.null(denominator_col)) "den" else denominator_col
  opacity <- if (isTRUE(highlight_target)) get_funnel_opacity(funnel_data, key_col) else NULL

  if (!is.null(opacity)) {
    if (is.null(scatter_settings)) scatter_settings <- list()
    if (is.null(scatter_settings$opacity)) scatter_settings$opacity <- opacity
  }

  args <- list(
    data = funnel_data,
    keys = as.name(key_col),
    numerators = as.name(numerator_col),
    denominators = as.name(denom_name),
    y_axis_settings = y_axis_settings,
    outlier_settings = outlier_settings
  )

  if (!is.null(x_axis_settings)) args$x_axis_settings <- x_axis_settings
  if (!is.null(scatter_settings)) args$scatter_settings <- scatter_settings

  args <- c(args, list(...))
  do.call(controlcharts::funnel, args)
}

pci_spc <- function(
    data,
    numerator_col,
    denominator_col = NULL,
    period_col = "period_end",
    filter_target = TRUE,
    spc_settings = list(chart_type = "p"),
    y_axis_settings = list(),
    outlier_settings = list(),
    nhs_icon_settings = list(show_variation_icons = TRUE),
    date_settings = list(
      date_format_day = "(blank)",
      date_format_month = "Mon",
      date_format_year = "YY",
      date_format_delim = " "
    ),
    ...) {
  if (!is.character(numerator_col) || length(numerator_col) != 1) {
    stop("numerator_col must be a single string")
  }
  if (!is.character(period_col) || length(period_col) != 1) {
    stop("period_col must be a single string")
  }
  if (isTRUE(filter_target)) {
    data <- filter_for_spc(data)
  }

  spc_data <- preaggregate_spc_counts(
    data,
    period_col = period_col,
    numerator_col = numerator_col,
    denominator_col = denominator_col
  )

  denom_name <- if (is.null(denominator_col)) "den" else denominator_col
  default_outliers <- list(
    improvement_direction = "increase",
    astronomical = TRUE,
    shift = TRUE,
    trend = TRUE,
    two_in_three = TRUE
  )

  args <- list(
    data = spc_data,
    keys = as.name(period_col),
    numerators = as.name(numerator_col),
    denominators = as.name(denom_name),
    spc_settings = spc_settings,
    y_axis_settings = y_axis_settings,
    outlier_settings = merge_settings(default_outliers, outlier_settings),
    nhs_icon_settings = nhs_icon_settings,
    date_settings = merge_settings(
      list(
        date_format_day = "(blank)",
        date_format_month = "Mon",
        date_format_year = "YY",
        date_format_delim = " "
      ),
      date_settings
    )
  )

  args <- c(args, list(...))
  do.call(controlcharts::spc, args)
}
# Helper: Get report subtitle
get_report_subtitle <- function() {
  switch(report_context,
    "hospital" = paste0("Hospital ", target_id, " Performance Report"),
    "clinician" = paste0("Clinician ", target_id, " Performance Report"),
    "statewide" = "Statewide Performance Report"
  )
}

prepare_pci_data <- function(pci_data) {
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
      acst %in% c(1,2) ~ "NSTEMI",
      acs == 0 ~ "Non-ACS"
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
    ecgdb = difftime(tbd, tecgd, units = "mins") + difftime(dbd, decgd, units = "mins"),
    #Calculate column required for Door to PCI time
    dbdt = difftime(tbd, toa, units = "mins") + difftime(dbd, doa, units = "mins"),
    #Calculate column required for Symptoms to Door time
    symptom_to_door = difftime(toa, tso, units = "mins") + difftime(doa, dso, units = "mins"),
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
      pel == 1 ~ "Brachial",
      pel == 2 ~ "Radial",
      pel == 3 ~ "Femoral",
      TRUE ~ "Unknown"
    ),
    access_route = factor(access_route, levels = c("Radial", "Femoral", "Brachial", "Unknown")),
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
    fmc_to_device = difftime(tbd, tfmc, units = "mins") +
                    difftime(dbd, dfmc, units = "mins"),
    # Symptom onset to reperfusion time
    symptom_to_reperfusion = difftime(tbd, tso, units = "mins") +
                             difftime(dbd, dso, units = "mins"),
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
