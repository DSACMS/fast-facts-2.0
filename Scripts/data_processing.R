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
