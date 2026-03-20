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

#bundle tab datapoints/frames
context <- list(
  bans = bans,
  years = years,
  nhe_gdp_share = nhe_gdp_share,
  nhe_pc = nhe_pc,
  df_insurance = df_insurance
)

# export
write_rds(context, "Dataout/context.rds")
