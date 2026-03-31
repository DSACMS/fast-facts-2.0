# PROJECT:  fast-facts-2.0
# PURPOSE:  store utility functions
# AUTHOR:   A.Chafetz | CMS
# REF ID:   73eeb778772f
# LICENSE:  MIT
# DATE:     2026-03-27
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(scales, warn.conflicts = FALSE)

source("Scripts/color_system.R")


# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "73eeb778772f" #a reference to be placed in viz captions

path <- "Data/CMSFastFacts2025_508.xlsx"

## muli-year pull for sparklines
files <- list.files(dirname(path), recursive = TRUE, full.names = TRUE)

#get the latest release from each year (if multiple)
files <- tibble(files = files) |>
  mutate(
    release = files |>
      str_extract("[A-Za-z]{3}\\d{4}") |>
      str_replace("cts", "Jan") |>
      my()
  ) |>
  group_by(year(release)) |>
  filter(release == max(release)) |>
  ungroup() |>
  pull(files)


# Remove Footnotes -------------------------------------------------------

rm_notes <- function(df) {
  #rename column names with a footnote
  df <- rename_with(df, ~ str_remove(., "_\\d{1}$"))

  #remove string text with footnotes
  df <- df |>
    mutate(across(where(is.character), ~ str_remove(., "( \\d|\\d)$")))

  #remove units notes
  df <- df |>
    mutate(across(where(is.character), ~ str_remove(., " \\(.*\\)")))

  return(df)
}


# Create Release date from path ------------------------------------------

gen_release_dt <- function(df) {
  df |>
    mutate(
      release_date = path |>
        str_extract("[A-Za-z]{3}\\d{4}") |>
        str_replace("cts", "Jan") |>
        my()
    )
}

ff_release = path |>
  str_extract("[A-Za-z]{3}\\d{4}") |>
  str_replace("cts", "Jan") |>
  my()

# Import Beneficiaries Tab -----------------------------------------------

read_benes <- function(path) {
  read_excel(
    path,
    sheet = "Populations",
    range = "A3:E16",
    na = c("", "--"),
    .name_repair = make_clean_names
  ) |>
    select(-x) |>
    rename(category = 1) |>
    rename_with(~ str_remove(., "_\\d{1}$")) |>
    mutate(
      ff_release = path |>
        str_extract("[A-Za-z]{3}\\d{4}") |>
        str_replace("cts", "Jan") |>
        my(),
      category = str_remove(category, "\\d$"),
      group = case_when(
        category == "Parts A and/or B" ~ "Medicare",
        category == "Total" ~ "Medicaid & CHIP"
      ),
      .before = category
    ) |>
    fill(group) |>
    filter(!category %in% c(NA, "Medicaid & CHIP")) |>
    mutate(
      category = replace_values(
        category,
        "Total" ~ "Medicaid & CHIP",
        "Original Medicare Enrollment" ~ "Original Medicare",
        "MA Enrollment" ~ "Medicare Advantage"
      )
    ) |>
    pivot_longer(
      starts_with("cy_"),
      names_to = "year",
      names_prefix = "cy_",
      names_transform = list(year = as.integer)
    )
}


# Read Cost Sharing Tab --------------------------------------------------

read_costsharing <- function(path) {
  tab <- "Deductibles, Coins, Premiums"

  df_tab <- read_excel(
    path,
    sheet = tab,
    skip = 2,
    na = c("", "N/A"),
    .name_repair = make_clean_names
  ) |>
    rename(sub_category = 1)

  df_tab <- df_tab |>
    filter_out(is.na(sub_category)) |>
    filter_out(sub_category == "Inpatient Hospital") |>
    mutate(
      category = case_when(is.na(pick(2)[[1]]) ~ sub_category),
      .before = 1
    ) |>
    fill(category)

  df_tab <- df_tab |>
    pivot_longer(
      starts_with("cy_"),
      names_to = "year",
      names_prefix = "cy_",
      names_transform = list(year = as.integer),
      values_drop_na = TRUE
    )

  df_tab_prem_b <- df_tab |>
    filter(
      category == "Premiums",
      sub_category == "Part B"
    ) |>
    mutate(value = str_remove_all(value, "\\$")) |>
    separate_wider_delim(value, delim = "-", names = c("lower", "upper")) |>
    pivot_longer(
      c(lower, upper),
      names_to = "bound"
    )

  df_tab <- df_tab |>
    filter_out(
      category == "Premiums",
      sub_category == "Part B"
    ) |>
    bind_rows(df_tab_prem_b)

  df_tab <- df_tab |>
    mutate(value = as.numeric(value))

  df_tab <- df_tab |>
    mutate(
      metric = case_when(
        str_detect(sub_category, "Deductible|Coinsurance") ~ str_extract(
          sub_category,
          "Deductible|Coinsurance"
        ) |>
          tolower(),
        sub_category == "Initial Coverage Limit" ~ "coverage_limit",
        TRUE ~ "premium"
      )
    )

  df_tab <- df_tab |>
    mutate(
      area = "Medicare",
      topic = "Cost Sharing",
      source = basename(path),
      source_tab = tab,
      source_origin = "medicare.gov",
      period_type = "CY"
    )

  #reoder
  df_tab <- df_tab |>
    relocate(
      area,
      topic,
      category,
      sub_category,
      metric,
      period_type,
      year,
      value,
      bound,
      source,
      source_tab
    )

  return(df_tab)
}


# Read All Provider Tabs -------------------------------------------------

read_provider_tab <- function(path, tab) {
  n_skip <- tab |>
    recode_values(
      "Institutional Providers" ~ 2,
      "NonInstitutional Providers" ~ 3,
      "DMEPOS Providers" ~ 4
    )

  #import
  df_tab <- read_excel(
    path,
    sheet = tab,
    skip = n_skip,
    .name_repair = make_clean_names
  )

  #initial munging
  df_tab <- df_tab |>
    rename(
      category = 1,
      value = count
    ) |>
    filter_out(is.na(value)) |>
    select(-starts_with("x"))

  # |>
  #   mutate(
  #     category = tab,
  #     category = ifelse(
  #       category == "NonInstitutional Providers",
  #       "Non-Institutional Providers",
  #       category
  #     ),
  #     .before = 1
  #   )

  hospital_subset <- c(
    "Short Stay",
    "Psychiatric",
    "Rehabilitation",
    "Children's",
    "Long Term",
    "Critical Access",
    "Religious Non-Medical"
  )

  #clean up hospital disaggs
  df_tab <- df_tab |>
    filter(category != "Total Hospitals") |>
    mutate(
      sub_category = case_when(category %in% hospital_subset ~ category),
      category = ifelse(category %in% hospital_subset, "Hospitals", category),
      .after = 1
    )

  #add total to initutional providers which is missing
  if (tab == "Institutional Providers") {
    df_tab <- tibble(
      category = "Total Providers",
      sub_category = NA,
      value = sum(df_tab$value)
    ) |>
      bind_rows(df_tab)
  }

  period_info <- extract_sheet_year(path, tab)

  #add meta data
  df_tab <- df_tab |>
    mutate(
      source = basename(path),
      source_tab = tab,
      source_origin = "CMS/Office of Enterprise Data & Analytics",
      area = "Medicare",
      topic = "Providers",
      provider_type = tab |>
        str_remove(" Providers") |>
        str_replace("NonI", "Non-I"),
      metric = "count",
      year = period_info$year,
      period_type = period_info$period_type
    )

  #reoder
  df_tab <- df_tab |>
    relocate(
      area,
      topic,
      category,
      sub_category,
      provider_type,
      metric,
      period_type,
      year,
      value,
      source,
      source_tab
    )

  return(df_tab)
}

read_all_providers <- function(path) {
  tibble(
    path = rep(path, 3),
    tab = c(
      "Institutional Providers",
      "NonInstitutional Providers",
      "DMEPOS Providers"
    )
  ) |>
    pmap(read_provider_tab) |>
    bind_rows()
}


# Extract Year -----------------------------------------------------------

extract_sheet_year <- function(path, sheet, n_rows = 5) {
  suppressMessages(
    raw <- read_excel(
      path,
      sheet = sheet,
      range = cell_rows(1:n_rows),
      col_names = FALSE
    )
  )

  # Flatten all cells to a character vector, drop NAs
  cells <- raw |>
    unlist() |>
    as.character() |>
    purrr::discard(is.na)

  # Pattern 1: (Calendar|Fiscal) Year YYYY
  # \d{4} matches exactly 4 digits — handles "Calendar Year 20237" (footnote
  # digit appended) correctly by not requiring end-of-string after the year.
  m <- str_match(cells, "(Calendar|Fiscal)\\s+Year\\s+(\\d{4})")
  m <- m[!is.na(m[, 1]), , drop = FALSE]

  if (nrow(m) > 0) {
    return(list(
      sheet = sheet,
      year = as.integer(m[1, 3]),
      period_type = if_else(m[1, 2] == "Calendar", "CY", "FY"),
      period = paste(
        if_else(m[1, 2] == "Calendar", "CY", "FY"),
        as.integer(m[1, 3])
      )
    ))
  }

  # Pattern 2: (MM/YYYY) — Prepaid Contracts style
  m2 <- str_match(cells, "\\((\\d{2})/(\\d{4})\\)")
  m2 <- m2[!is.na(m2[, 1]), , drop = FALSE]

  if (nrow(m2) > 0) {
    return(list(
      sheet = sheet,
      year = as.integer(m2[1, 3]),
      period_type = "point-in-time",
      period = NA_character_
    ))
  }

  # No year found in header rows — year is likely in column headers
  list(
    sheet = sheet,
    year = NA_integer_,
    period_type = NA_character_,
    period = NA_character_
  )
}


# Import CMS Financial Data Tab ------------------------------------------

read_financial <- function(path) {
  tab <- "CMS Financial Data"

  #import
  df_tab <- read_excel(
    path,
    sheet = tab,
    skip = 2,
    col_names = c("sub_category", "value"),
    .name_repair = make_clean_names
  )

  #clean up notes
  df_tab <- df_tab |>
    filter_out(is.na(value)) |>
    rm_notes()

  #convert to correct units
  df_tab <- df_tab |>
    mutate(value = ifelse(sub_category == "FTE Employment", value, value * 1e6))

  #create area + category
  df_tab <- df_tab |>
    mutate(
      topic = ifelse(sub_category == "FTE Employment", "Staffing", "Financial"),
      area = "CMS",
      # ifelse(
      #   str_detect(sub_category, "Medicare|Medicaid|CHIP"),
      #   str_extract(sub_category, "Medicare|Medicaid|CHIP"),
      #   "CMS"
      # ),
      category = str_extract(
        sub_category,
        "(Federal Program Spending|Program Management|Fraud|FTE Employment)"
      ),
      metric = ifelse(
        sub_category == "FTE Employment",
        "count",
        "expenditures"
      ),
      sub_category = ifelse(
        sub_category == "FTE Employment",
        NA_character_,
        sub_category
      ),
      .before = 1
    ) |>
    fill(category)

  #remove totals
  df_tab <- df_tab |>
    filter_out(
      sub_category %in%
        c("Total Federal Program Spending", "Total Program Management")
    )

  period_info <- extract_sheet_year(path, tab)

  #add meta data
  df_tab <- df_tab |>
    mutate(
      source = basename(path),
      source_tab = tab,
      source_origin = tab,
      year = period_info$year,
      period_type = period_info$period_type
    )

  #reoder
  df_tab <- df_tab |>
    relocate(
      area,
      topic,
      category,
      sub_category,
      metric,
      period_type,
      year,
      value,
      source,
      source_tab
    )

  return(df_tab)
}


# Import NHE -------------------------------------------------------------

read_nhe <- function(path) {
  tab <- "NHE"

  #import
  df_tab <- read_excel(
    path,
    sheet = tab,
    skip = 2,
    col_names = c("sub_category", "x", "value")
  )

  #clean up notes
  df_tab <- df_tab |>
    select(-x) |>
    filter_out(is.na(value)) |>
    rm_notes()

  #adjust units
  df_tab <- df_tab |>
    mutate(
      value = ifelse(
        sub_category %in% c("% of GDP", "Per Capita"),
        value,
        value * 1e9
      )
    )

  #groupings
  df_tab <- df_tab |>
    mutate(
      area = case_when(
        str_detect(sub_category, ("Medicare|Medicaid|CHIP")) ~ "CMS",
        str_detect(sub_category, "Department") ~ "Federal",
        TRUE ~ "National"
      ),
      category = ifelse(
        sub_category %in% c("Total", "% of GDP", "Per Capita"),
        "National Health Expenditures",
        "Health Insurance"
      ),
      metric = case_when(
        sub_category == "% of GDP" ~ "pct_gdp",
        sub_category == "Per Capita" ~ "per_capita",
        TRUE ~ "expenditures"
      ),
      .before = 1
    )

  #remove total row
  df_tab <- df_tab |>
    filter_out(sub_category == "Health Insurance")

  period_info <- extract_sheet_year(path, tab)

  #add meta data
  df_tab <- df_tab |>
    mutate(
      topic = "NHE",
      source = basename(path),
      source_tab = tab,
      source_origin = "CMS/Office of the Actuary",
      year = period_info$year,
      period_type = period_info$period_type
    )

  #reoder
  df_tab <- df_tab |>
    relocate(
      area,
      topic,
      category,
      sub_category,
      metric,
      period_type,
      year,
      value,
      source,
      source_tab
    )

  return(df_tab)
}


# Import Medicaid & CHIP Expenditures ------------------------------------

read_medicaid_exp <- function(path) {
  tab <- "Medicaid & CHIP Expenditures"

  df_tab <- read_excel(
    path,
    sheet = tab,
    skip = 4,
    col_names = c("sub_category", "value")
  )

  df_tab <- df_tab |>
    filter(!is.na(value)) |>
    rm_notes()

  df_tab <- df_tab |>
    mutate(value = value * 1e9)

  #remove total and add other
  df_tab <- df_tab |>
    filter(sub_category != "All Services") |>
    bind_rows(
      tibble(
        sub_category = "Other Services",
        value = sum(df_tab[df_tab$sub_category == "All Services", ]$value) -
          sum(df_tab[df_tab$sub_category != "All Services", ]$value)
      )
    )

  period_info <- extract_sheet_year(path, tab)

  df_tab <- df_tab |>
    mutate(
      area = "Medicaid",
      topic = "Expenditures",
      category = "Payments (by Selected Type of Service)",
      metric = "expenditure",
      source = basename(path),
      source_tab = tab,
      source_origin = "CMS/Center for Medicaid and CHIP Services",
      year = period_info$year,
      period_type = period_info$period_type
    )

  #reoder
  df_tab <- df_tab |>
    relocate(
      area,
      topic,
      category,
      sub_category,
      metric,
      period_type,
      year,
      value,
      source,
      source_tab
    )

  return(df_tab)
}

# Read Medicare Utilization ----------------------------------------------

read_medicare_util <- function(path) {
  tab <- "Medicare Utilization"

  #import
  df_tab <- read_excel(
    path,
    sheet = tab,
    skip = 4,
    col_names = c("sub_category", "persons_served", "x", "payments")
  )

  #subset to actual data
  df_tab <- df_tab |>
    select(-x) |>
    filter_out(is.na(persons_served))

  #adjust units
  df_tab <- df_tab |>
    mutate(
      persons_served = persons_served * 1e6,
      payments = payments * 1e9
    ) |>
    pivot_longer(
      c(persons_served, payments),
      names_to = "metric"
    )

  df_tab <- df_tab |>
    mutate(
      category = str_extract(sub_category, "(Total|Part (A|B))"),
      category = ifelse(category == "Total", "Total (A and/or B)", category),
      sub_category = ifelse(
        sub_category %in% c("Part A", "Part B"),
        "Total",
        sub_category
      ),
      .before = 1
    ) |>
    fill(category)

  period_info <- extract_sheet_year(path, tab)

  df_tab <- df_tab |>
    mutate(
      area = "Medicare",
      topic = "Utilization",
      source = basename(path),
      source_tab = tab,
      source_origin = "CMS/Office of Enterprise Data & Analytics",
      year = period_info$year,
      period_type = period_info$period_type
    )

  #reoder
  df_tab <- df_tab |>
    relocate(
      area,
      topic,
      category,
      sub_category,
      metric,
      period_type,
      year,
      value,
      source,
      source_tab
    )

  return(df_tab)
}


# Read Medicare Part D ---------------------------------------------------

read_medicare_d <- function(path) {
  tab <- "Medicare Part D"

  #import
  df_tab <- read_excel(
    path,
    sheet = tab,
    skip = 3,
    col_names = c("sub_category", "value")
  )

  #subset to actual data
  df_tab <- df_tab |>
    filter_out(is.na(value))

  #adjust units
  df_tab <- df_tab |>
    mutate(
      value = ifelse(
        sub_category == "Utilizing Beneficiaries, in millions",
        value * 1e6,
        value * 1e9
      ),
      sub_category = str_remove(sub_category, ", in.*"),
      sub_category = str_remove(sub_category, "^Part D ")
    )

  df_tab <- df_tab |>
    mutate(
      metric = recode_values(
        sub_category,
        "Utilizing Beneficiaries" ~ "persons_served",
        "Prescription Drug Events" ~ "rx_events",
        default = "payments"
      ),
      sub_category = case_when(
        sub_category %in%
          c("Benefit Payments", "Administrative Expenses") ~ sub_category,
        TRUE ~ "Total"
      )
    )

  period_info <- extract_sheet_year(path, tab)

  df_tab <- df_tab |>
    mutate(
      area = "Medicare",
      topic = "Utilization",
      category = "Part D",
      source = basename(path),
      source_tab = tab,
      source_origin = "CMS/Office of Enterprise Data & Analytics/Office of the Actuary",
      year = period_info$year,
      period_type = period_info$period_type
    )

  #reoder
  df_tab <- df_tab |>
    relocate(
      area,
      topic,
      category,
      sub_category,
      metric,
      period_type,
      year,
      value,
      source,
      source_tab
    )

  return(df_tab)
}
