# PROJECT:  fast-facts-2.0
# PURPOSE:  orchestrate machine readable data structure
# AUTHOR:   A.Chafetz | CMS
# REF ID:   a4561869cb18
# LICENSE:  MIT
# DATE:     2026-03-19
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "a4561869cb18" #a reference to be placed in viz captions

#data file
path <- "Data/CMSFastFacts2025_508.xlsx"

excel_sheets(path)

# Populations ------------------------------------------------------------

curr_sht <- "Populations"

df_pop_medicare <- read_excel(
  path,
  sheet = curr_sht,
  range = "A3:E10",
  .name_repair = make_clean_names
)

df_pop_medicare <- df_pop_medicare |>
  select(-x) |> #drop blank col
  rename_with(~ str_remove(.x, "_\\d$")) |> #remove footnote suffix
  rename(category = 1) |> # bad practice, but renaming by position
  mutate(area = "Medicare", .before = 1) |>
  mutate(
    sub_category = case_when(
      category %in% c("Aged", "Disabled", "MA Enrollment") ~ category,
      category %in%
        c("Parts A and/or B", "MA & Other Health Plan Enrollment") ~ "Total"
    ),
    category = case_when(
      category %in% c("Aged", "Disabled") ~ "Parts A and/or B",
      category == "MA Enrollment" ~ "MA & Other Health Plan Enrollment",
      TRUE ~ category
    ),
    .after = category
  )

df_pop_medicare <- df_pop_medicare |>
  pivot_longer(
    cols = starts_with("cy_"),
    names_to = "year",
    names_prefix = "cy_"
  )

df_pop_medicare <- df_pop_medicare |>
  mutate(
    units = "beneficiaries, millions (avg monthly)",
    source_tab = curr_sht,
    .after = last_col()
  )

df_pop_medicaid <- read_excel(
  path,
  sheet = curr_sht,
  range = "A12:E16",
  na = c("", "--"),
  .name_repair = make_clean_names
)

df_pop_medicaid <- df_pop_medicaid |>
  select(-x) |> #drop blank col
  rename_with(~ str_remove(.x, "_\\d$")) |> #remove footnote suffix
  rename(category = 1) |> # bad practice, but renaming by position
  mutate(area = "Medicaid", .before = 1) |>
  mutate(
    category = str_remove(category, "\\d$"),
    sub_category = category,
    category = "Total",
    .after = category
  )

df_pop_medicaid <- df_pop_medicaid |>
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    names_prefix = "x"
  )

df_pop_medicaid <- df_pop_medicaid |>
  mutate(
    units = "beneficiaries, millions",
    source_tab = curr_sht,
    .after = last_col()
  )


# Deductibles, Coins, Premiums -------------------------------------------

curr_sht <- "Deductibles, Coins, Premiums"

df_cost_sharing <- read_excel(
  path,
  sheet = curr_sht,
  range = "A3:C22",
  na = c("", "N/A"),
  .name_repair = make_clean_names
)

df_cost_sharing <- df_cost_sharing |>
  rename(sub_category = 1) |>
  filter(sub_category != "Inpatient Hospital") |>
  mutate(
    area = "Medicare",
    category = case_when(is.na(cy_2024) ~ sub_category),
    .before = 1
  ) |>
  fill(category, .direction = "down") |>
  filter_out(is.na(cy_2024))

df_cost_sharing <- df_cost_sharing |>
  pivot_longer(
    starts_with("cy"),
    names_to = "year",
    names_prefix = "cy_",
  )

df_cost_sharing_prem <- df_cost_sharing |>
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

df_cost_sharing <- df_cost_sharing |>
  filter_out(
    category == "Premiums",
    sub_category == "Part B"
  ) |>
  bind_rows(df_cost_sharing_prem)

df_cost_sharing <- df_cost_sharing |>
  mutate(
    units = "amount, USD",
    source_tab = curr_sht,
    .after = last_col()
  )


# Utilization ------------------------------------------------------------

curr_sht <- "Medicare Utilization"

df_utilization_ab <- read_excel(
  path,
  sheet = curr_sht,
  range = "A3:D16",
  .name_repair = make_clean_names
)

df_utilization_ab <- df_utilization_ab |>
  select(-x_2) |>
  rename(sub_category = 1) |>
  mutate(
    area = "Medicare",
    category = case_when(
      sub_category %in% c("Total", "Part A", "Part B") ~ sub_category
    ),
    .before = 1
  ) |>
  fill(category, .direction = "down") |>
  filter_out(is.na(sub_category)) |>
  mutate(sub_category = case_when(category != sub_category ~ sub_category))

df_utilization_ab <- df_utilization_ab |>
  pivot_longer(
    c(persons_served, program_payments),
    names_to = "units"
  )

df_utilization_ab <- df_utilization_ab |>
  mutate(
    year = 2023, ## HARD CODED!!
    units = ifelse(
      units == "persons_served",
      "persons served, millions",
      "program payments, billions"
    ),
    source_tab = curr_sht,
    .after = last_col()
  )


curr_sht <- "Medicare Part D"

df_utilization_d <- read_excel(
  path,
  sheet = curr_sht,
  range = "A4:B9",
  col_names = c("units", "value")
)

df_utilization_d <- df_utilization_d |>
  filter_out(is.na(units)) |>
  mutate(
    area = "Medicare",
    category = "Part D",
    units = str_remove(units, "Part D "),
    units = ifelse(
      str_detect(units, "ions", negate = TRUE),
      paste0(units, ", in billions"),
      units
    )
  )

df_utilization_d <- df_utilization_d |>
  mutate(
    year = 2023, ## HARD CODED!!
    source_tab = curr_sht,
    .after = last_col()
  )


# Medicaid & CHIPS Expenditures ------------------------------------------

curr_sht <- "Medicaid & CHIP Expenditures"

df_exp_medicaid <- read_excel(
  path,
  sheet = curr_sht,
  range = "A5:B11",
  col_names = c("sub_category", "value")
)

df_exp_medicaid <- df_exp_medicaid |>
  mutate(
    sub_category = str_remove(sub_category, "\\d$"),
    area = "Medicaid",
    category = "All Services",
    ,
    sub_category = ifelse(sub_category == category, "Total", sub_category),
    .before = 1
  )

df_exp_medicaid <- df_exp_medicaid |>
  mutate(
    year = 2023, ## HARD CODED!!
    source_tab = curr_sht,
    .after = last_col()
  )
