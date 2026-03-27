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


# Import NHE Tab ---------------------------------------------------------

read_nhe <- function(path) {
  read_excel(
    path,
    sheet = "NHE",
    col_names = c("category", "x", "value")
  ) |>
    select(-x) |>
    filter(
      category %in% v_insurance | str_detect(category, "Year"),
    ) |>
    mutate(
      year = case_when(is.na(value) ~ str_sub(category, -4)),
      .before = 1
    ) |>
    fill(year) |>
    filter_out(is.na(value))
}


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
  read_excel(
    path,
    sheet = "Deductibles, Coins, Premiums",
    range = "A3:C22",
    na = c("", "N/A"),
    .name_repair = make_clean_names
  ) |>
    rename(category = 1) |>
    filter_out(is.na(category)) |>
    filter_out(category == "Inpatient Hospital") |>
    mutate(
      ff_release = path |>
        str_extract("[A-Za-z]{3}\\d{4}") |>
        str_replace("cts", "Jan") |>
        my(),
      group = case_when(is.na(pick(2)[[1]]) ~ category),
      .before = 1
    ) |>
    fill(group) |>
    filter_out(group == category) |>
    pivot_longer(
      starts_with("cy_"),
      names_to = "year",
      names_prefix = "cy_",
      names_transform = list(year = as.integer)
    )
}


# Read All Provider Tabs -------------------------------------------------

read_provider_tab <- function(path, tab) {
  n_skip <- tab |>
    recode_values(
      "Institutional Providers" ~ 2,
      "NonInstitutional Providers" ~ 3,
      "DMEPOS Providers" ~ 4
    )

  # yr_row <- n_skip - 1

  # cy_year <- read_excel(
  #   path,
  #   sheet = tab,
  #   range = str_glue("A{yr_row}"),
  #   col_names = "title"
  # ) |>
  #   pull() |>
  #   str_extract("\\d{4}") |>
  #   as.integer()

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

  period_info <- extract_sheet_year(path, sheet)

  #add meta data
  df_tab <- df_tab |>
    mutate(
      source = basename(path),
      source_tab = tab,
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
      year = as.integer(m[1, 3]),
      period_type = if_else(m[1, 2] == "Calendar", "CY", "FY")
    ))
  }

  # Pattern 2: (MM/YYYY) — Prepaid Contracts style
  m2 <- str_match(cells, "\\((\\d{2})/(\\d{4})\\)")
  m2 <- m2[!is.na(m2[, 1]), , drop = FALSE]

  if (nrow(m2) > 0) {
    return(list(
      year = as.integer(m2[1, 3]),
      period_type = "point-in-time"
    ))
  }

  # No year found in header rows — year is likely in column headers
  list(year = NA_integer_, period_type = NA_character_)
}
