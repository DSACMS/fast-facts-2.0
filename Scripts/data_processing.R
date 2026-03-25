# PROJECT:  fast-facts-2.0
# PURPOSE:  data processing for dashboard pages
# AUTHOR:   A.Chafetz | CMS
# REF ID:   4b4e2514
# LICENSE:  MIT
# DATE:     2026-03-20
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

# GLOBAL VARIABLES --------------------------------------------------------

#data file
path <- "Data/CMSFastFacts2025_508.xlsx"

#review excel sheets
# excel_sheets(path)

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


# CONTEXT TAB ------------------------------------------------------------

# import sheet - NHE
df_nhe <- read_excel(
  path,
  sheet = "NHE",
  range = "A3:C12",
  col_names = c("category", "x", "value")
)

#extract NHE share of GDP
nhe_gdp_share <- df_nhe |>
  filter(category == "% of GDP") |>
  pull()

#extract NHE per capita
nhe_pc <- df_nhe |>
  filter(category == "Per Capita") |>
  pull()

#identify list of NHE spending group that need to be included from table
v_insurance <- c(
  "Private Health Insurance",
  "Medicare",
  "Medicaid (Title XIX)",
  "CHIP (Title XIX & XXI)",
  "Department of Defense",
  "Department of Veterans Affairs"
)

#filter down
df_insurance <- df_nhe |>
  filter(category %in% v_insurance)

#check all NHE categories make it in
if (nrow(df_insurance) != length(v_insurance)) {
  stop(
    "Mismatch: number of rows in df_insurance does not equal length of v_insurance"
  )
}

#create a dataframe of health spending by type
df_insurance <- df_insurance |>
  select(-x) |>
  mutate(category = fct_lump_prop(category, .1, w = value)) |>
  count(category, wt = value, name = "value") |>
  mutate(share = value / sum(value))

## import sheet - CMS Financial Data
df_cms <- read_excel(
  path,
  sheet = "CMS Financial Data",
  range = "A3:B12",
  col_names = c("category", "value")
)

#extract numbers for BAN in tab
bans <- c(
  nhe_total = df_nhe |> filter(category == "Total") |> pull(),
  health_insurance = df_nhe |> filter(category == "Health Insurance") |> pull(),
  fed_spend = df_cms |> filter(str_detect(category, "Total Federal")) |> pull()
)

# financial data for plot
df_spend <- df_cms |>
  filter(str_detect(category, "Total (Federal|Program) |FTE", negate = TRUE)) |>
  mutate(
    category = str_remove(category, " (\\d|\\(.*)$"),
    group = case_when(
      category %in% c("Total Appropriation", "Other Sources") ~ "PM",
      str_detect(category, "Fraud") ~ "Fraud",
      TRUE ~ "Spend"
    ),
    .before = 1
  ) |>
  group_by(group) |>
  mutate(share = value / sum(value)) |>
  ungroup()

# FTEs
fte <- df_cms |>
  filter(str_detect(category, "FTE")) |>
  pull()

#identify source years
nhe_yr <- read_excel(
  path,
  sheet = "NHE",
  range = "A1",
  col_names = "title"
) |>
  pull() |>
  str_extract("(Fiscal|Calendar) Year .*") |>
  str_replace("Fiscal Year", "FY") |>
  str_replace("Calendar Year", "CY")

fed_spend_yr <- read_excel(
  path,
  sheet = "CMS Financial Data",
  range = "A1",
  col_names = "title"
) |>
  pull() |>
  str_extract("(Fiscal|Calendar) Year .*") |>
  str_replace("Fiscal Year", "FY") |>
  str_replace("Calendar Year", "CY")

#combine years
years <- c(
  nhe_yr = nhe_yr,
  fed_spend_yr = fed_spend_yr
)


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


df_insurance_trend <- files |>
  set_names() |>
  map(read_nhe) |>
  list_rbind()
# list_rbind(names_to = "source") |>
# mutate(source = basename(source))

df_insurance_trend <- df_insurance_trend %>%
  arrange(category, year) %>%
  group_by(category) %>%
  summarise(
    latest_year = max(year),
    latest_value = value[which.max(year)],
    trend = list(value), # ordered by year (arranged above)
    .groups = "drop"
  ) %>%
  arrange(desc(latest_value))

#bundle tab datapoints/frames
context <- list(
  bans = bans,
  years = years,
  nhe_gdp_share = nhe_gdp_share,
  nhe_pc = nhe_pc,
  df_insurance = df_insurance,
  df_insurance_trend = df_insurance_trend,
  df_spend = df_spend,
  fte = fte
)

# export
write_rds(context, "Dataout/context.rds")


# BENEFICIARIES TAB ------------------------------------------------------

# import sheet - Medicaid & CHIP Expenditures
df_medicaid_exp <- read_excel(
  path,
  sheet = "Medicaid & CHIP Expenditures",
  range = "A5:B11",
  col_names = c("category", "value")
)

df_medicaid_exp <- df_medicaid_exp |>
  mutate(
    category = str_remove(category, "(\\d|\\(.*)$"),
  )

# import sheet - Medicare Utilization
df_medicare_util <- read_excel(
  path,
  sheet = "Medicare Utilization",
  range = "A7:D16",
  col_names = c("category", "beneficiaries", "x", "payments")
)


df_medicare_util_d <- read_excel(
  path,
  sheet = "Medicare Part D",
  range = "A4:B9",
  col_names = c("category", "value")
)


df_medicare_util <- df_medicare_util |>
  select(-x) |>
  filter_out(is.na(beneficiaries)) |>
  mutate(
    group = case_when(str_detect(category, "Part") ~ category),
    .before = 1
  ) |>
  fill(group)

df_medicare_util_d <- df_medicare_util_d |>
  filter(str_detect(category, "Beneficiaries|Expenditures")) |>
  mutate(
    group = "Part D",
    category = category |>
      str_extract("Beneficiaries|Expenditures") |>
      tolower()
  ) |>
  pivot_wider(
    names_from = category
  ) |>
  rename(payments = expenditures) |>
  mutate(category = "Part D", .after = group)


df_medicare_util <- df_medicare_util |>
  bind_rows(df_medicare_util_d)


df_medicare_util <- df_medicare_util |>
  pivot_longer(
    c(beneficiaries, payments),
    names_to = "metric"
  ) |>
  group_by(metric) |>
  # mutate(zscore = scale(value)) |>
  mutate(zscore = (value - mean(value)) / sd(value)) |>
  ungroup()


df_medicare_util <- df_medicare_util |>
  mutate(
    lab_ben = case_when(
      metric == "beneficiaries" ~ str_glue(
        "{category}\n{label_number(suffix = 'm')(value)}"
      )
    ),
    lab_exp = case_when(
      metric == "payments" ~ label_number(1, prefix = "$", suffix = "B")(value)
    ),
  )

df_medicare_util |>
  filter(
    str_detect(category, "^Part"),
    metric == "beneficiaries"
  ) |>
  select(category, value)


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

df_benes_trend <- files |>
  keep(~ str_extract(.x, "\\d{4}") |> as.integer() >= 2023) |>
  set_names() |>
  map(read_benes) |>
  list_rbind()

#keep latest observation for each year
df_benes_trend <- df_benes_trend |>
  group_by(group, category, year) |>
  filter(ff_release == max(ff_release)) |>
  ungroup()


#extract numbers for BAN in tab
benes_bans <- df_benes_trend |>
  filter(
    category %in% c("Parts A and/or B", "Part D (MAPD+PDP)", "Medicaid & CHIP"),
    year == max(year)
  ) |>
  mutate(
    name = case_when(
      category == "Parts A and/or B" ~ "medicare_ab",
      category == "Part D (MAPD+PDP)" ~ "medicare_d",
      category == "Medicaid & CHIP" ~ "medicaid"
    )
  ) |>
  select(name, value) |> # replace `value` with your actual value column name
  deframe()


ban_year <- df_benes_trend |>
  filter(
    category %in% c("Parts A and/or B", "Part D (MAPD+PDP)", "Medicaid & CHIP"),
    year == max(year)
  ) |>
  distinct(year) |>
  pull()


#combine years
benes_years <- c(
  ban_year = ban_year
)

df_medicare_type_trend <- df_benes_trend |>
  filter(category %in% c("Original Medicare", "Medicare Advantage")) |>
  pivot_wider(names_from = category, values_from = value) |>
  clean_names()


df_medicaid_type_trends <- df_benes_trend |>
  filter(
    group == "Medicaid & CHIP",
    category != "Medicaid & CHIP",
    !is.na(value)
  ) |>
  group_by(category) |>
  mutate(
    category = recode_values(
      category,
      "Dual Eligible (includes Aged, Disabled & ESRD)" ~ "Dual Eligible",
      "Medicaid Expansion Adults" ~ "ME Adults",
      default = category
    ),
    val_pt = case_when(year == min(year) | year == max(year) ~ value),
    lab_start = case_when(
      year == min(year) ~ label_number(1, suffix = "m")(value)
    ),
    lab_end = case_when(
      year == max(year) ~ label_number(1, suffix = "m")(value)
    ),
  ) |>
  ungroup()


#bundle tab data points/frames
beneficiaries <- list(
  bans = benes_bans,
  years = benes_years,
  df_medicare_util = df_medicare_util,
  df_medicaid_exp = df_medicaid_exp,
  df_medicare_type_trend = df_medicare_type_trend,
  df_medicaid_type_trends = df_medicaid_type_trends
)

# export
write_rds(beneficiaries, "Dataout/beneficiaries.rds")
