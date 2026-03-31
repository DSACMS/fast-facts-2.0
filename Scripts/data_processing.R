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
library(scales)

#add colors

source("Scripts/functions.R")
source("Scripts/color_system.R")

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
df_nhe <- read_nhe(path)

#extract NHE total
nhe_total <- df_nhe |>
  filter(
    year == max(year),
    category == "National Health Expenditures",
    sub_category == "Total"
  ) |>
  mutate(
    value_fmt = label_number(.1, prefix = "$", scale_cut = cut_short_scale())(
      value
    )
  ) |>
  pull()

#extract NHE share of GDP
df_nhe_gdp_share <- df_nhe |>
  filter(
    year == max(year),
    category == "National Health Expenditures",
    sub_category == "% of GDP"
  ) |>
  mutate(
    value_fmt = label_percent(1)(value),
    value_sqrt = sqrt(value * 100)
  ) |>
  select(category, sub_category, year, value, value_fmt, value_sqrt)

#extract NHE per capita
df_nhe_pc <- df_nhe |>
  filter(
    year == max(year),
    category == "National Health Expenditures",
    sub_category == "Per Capita"
  ) |>
  mutate(
    value_fmt = label_comma(1, prefix = "$")(value),
    n_icons = round(value / 1000)
  ) |>
  select(category, sub_category, year, value, value_fmt, n_icons)

#insurnace
df_insurance <- df_nhe |>
  filter(
    year == max(year),
    category == "Health Insurance"
  )

#extact health insurnace total
health_insurance <- df_insurance |>
  count(wt = value) |>
  mutate(
    value_fmt = label_number(.1, prefix = "$", scale_cut = cut_short_scale())(n)
  ) |>
  pull()


#create a dataframe of health spending by type
df_insurance <- df_insurance |>
  select(category, sub_category, year, value) |>
  mutate(
    value_fmt = label_number(.1, prefix = "$", scale_cut = cut_short_scale())(
      value
    ),
    share = value / sum(value),
    fill_color = case_when(
      str_detect(sub_category, "Medicare") ~ ff_colors$base[["azure"]],
      str_detect(sub_category, "Medicaid") ~ ff_colors$base[["teal"]],
      str_detect(sub_category, "CHIP") ~ ff_colors$base[["plum"]],
      TRUE ~ "#A6A6A6"
    )
  )

## import sheet - CMS Financial Data
df_cms <- read_financial(path)

#extract fed spending
fed_spend = df_cms |>
  filter(
    year == max(year),
    category == "Federal Program Spending"
  ) |>
  count(wt = value) |>
  mutate(
    value_fmt = label_number(.1, prefix = "$", scale_cut = cut_short_scale())(n)
  ) |>
  pull()

#extract numbers for BAN in tab
bans <- c(
  nhe_total = nhe_total,
  health_insurance = health_insurance,
  fed_spend = fed_spend
)

# financial data for plot
df_spend <- df_cms |>
  filter(
    year == max(year),
    topic == "Financial"
  ) |>
  group_by(category) |>
  mutate(
    share = value / sum(value),
    squares = round(share * 100),
    value_fmt = label_number(.1, prefix = "$", scale_cut = cut_short_scale())(
      value
    ),
    fill_color = recode_values(
      sub_category,
      "Medicare Benefits" ~ ff_colors$base[["azure"]],
      "Total Medicaid" ~ ff_colors$base[["teal"]],
      "CHIP" ~ ff_colors$base[["plum"]],
      "Other Spending" ~ ff_colors$scales$charcoal[["200"]]
    )
  ) |>
  ungroup() |>
  select(category, sub_category, value, value_fmt, share, squares, fill_color)

# FTEs
fte <- df_cms |>
  filter(
    year == max(year),
    str_detect(category, "FTE")
  ) |>
  mutate(
    value_fmt = label_comma()(value),
    n_icons = round(value / 1e3)
  ) |>
  select(category, year, value, value_fmt, n_icons)

#identify source years
nhe_yr <- extract_sheet_year(path, "NHE")

fed_spend_yr <- extract_sheet_year(path, "CMS Financial Data")

#combine years
years <- c(
  nhe_yr = nhe_yr$period,
  fed_spend_yr = fed_spend_yr$period
)


# df_insurance_trend <- files |>
#   set_names() |>
#   map(read_nhe) |>
#   list_rbind()
# # list_rbind(names_to = "source") |>
# # mutate(source = basename(source))

# df_insurance_trend <- df_insurance_trend %>%
#   arrange(category, year) %>%
#   group_by(category) %>%
#   summarise(
#     latest_year = max(year),
#     latest_value = value[which.max(year)],
#     trend = list(value), # ordered by year (arranged above)
#     .groups = "drop"
#   ) %>%
#   arrange(desc(latest_value))

#bundle tab datapoints/frames
context <- list(
  bans = bans,
  years = years,
  df_nhe_gdp_share = df_nhe_gdp_share,
  df_nhe_pc = df_nhe_pc,
  df_insurance = df_insurance,
  df_spend = df_spend,
  fte = fte
)

# export
write_rds(context, "Dataout/context.rds")


# BENEFICIARIES TAB ------------------------------------------------------

# import sheet - Medicaid & CHIP Expenditures
df_medicaid_exp <- read_medicaid_exp(path)


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
    fill_color = recode_values(
      category,
      "Children" ~ ff_colors$base[["plum"]],
      "Dual Eligible (includes Aged, Disabled & ESRD)" ~ ff_colors$scales$teal[[
        "200"
      ]],
      "Medicaid Expansion Adults" ~ ff_colors$scales$teal[["900"]],
    ),
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


# COST SHARING TAB -------------------------------------------------------

cs_ban <- df_medicare_util |>
  filter(group == category) |>
  mutate(
    value = ifelse(
      metric == "beneficiaries",
      label_number(suffix = "M")(value),
      label_number(1, prefix = "$", suffix = "B")(value)
    )
  ) |>
  unite(label, c(category, metric)) |>
  select(label, value) |>
  mutate(label = label |> str_replace_all(" ", "_") |> tolower()) |>
  # mutate(label = str_glue("{category}\n{ifelse(metric == 'beneficiaries', 'Persons Served', 'Program Payments')}")) |>
  deframe()

cs_yr <- read_excel(
  path,
  sheet = "Medicare Utilization",
  range = "A2",
  col_names = "title"
) |>
  pull() |>
  str_extract("(Fiscal|Calendar) Year .*") |>
  str_replace("Fiscal Year", "FY") |>
  str_replace("Calendar Year", "CY")


df_cs_trend <- files |>
  keep(~ str_extract(.x, "\\d{4}") |> as.integer() >= 2023) |>
  set_names() |>
  map(read_costsharing) |>
  list_rbind()

#keep latest observation for each year
df_cs_trend <- df_cs_trend |>
  arrange(group, category, year, ff_release) |>
  group_by(group, category, year) |>
  filter(ff_release == max(ff_release)) |>
  ungroup()


#
df_cs_premb <- df_cs_trend |>
  filter(
    group == "Premiums",
    category == "Part B"
  ) |>
  mutate(value = str_remove_all(value, "\\$")) |>
  separate_wider_delim(value, delim = "-", names = c("lower", "upper")) |>
  pivot_longer(
    c(lower, upper),
    names_to = "bound"
  )

df_cs_trend <- df_cs_trend |>
  filter_out(
    group == "Premiums",
    category == "Part B"
  ) |>
  bind_rows(df_cs_premb)

df_cs_trend <- df_cs_trend |>
  mutate(value = as.numeric(value))

df_cs_trend <- df_cs_trend |>
  filter(year >= max(year) - 1) |>
  mutate(
    ln_group = ifelse(
      !is.na(bound),
      str_glue("{group} {category} {bound}"),
      str_glue("{group} {category}")
    ),
    fill_color = ifelse(
      year == max(year),
      ff_colors$scales$cobolt["200"],
      "white"
    ),
    order = ifelse(year == max(year), value, 0)
  ) |>
  group_by(ln_group) |>
  mutate(
    mid_pt = mean(value, na.rm = TRUE),
    delta = value / lag(value) - 1,
    delta_lab = label_percent(1, style_positive = "plus")(delta)
  ) |>
  ungroup()


#bundle tab data points/frames
cost_sharing <- list(
  bans = cs_ban,
  years = cs_yr,
  df_cs_trend = df_cs_trend
)

# export
write_rds(cost_sharing, "Dataout/cost_sharing.rds")


# PROVIDERS --------------------------------------------------------------

df_providers <- read_all_providers(path)

ban_providers <- df_providers |>
  filter(
    topic == "Providers",
    str_detect(category, "Total"),
    year == max(year)
  ) |>
  unite(period, c(period_type, year), sep = " ") |>
  mutate(
    value = ifelse(
      value > 1e6,
      label_number(.1, scale_cut = cut_short_scale())(value),
      label_number(1, scale_cut = cut_short_scale())(value)
    )
  ) |>
  select(provider_type, period, value)

ban_providers_years <- ban_providers |>
  select(provider_type, period) |>
  deframe()

ban_providers <- ban_providers |>
  select(provider_type, value) |>
  deframe()

# hospitals subset
df_hospital_subset <- df_providers |>
  filter(
    topic == "Providers",
    category == "Hospitals",
    year == max(year)
  ) |>
  select(sub_category, value) |>
  mutate(share = value / sum(value))

#provider coutns
df_provider_counts <- df_providers |>
  filter(
    topic == "Providers",
    # provider_type %in% c("Institutional", "Non-Institutional"),
    str_detect(category, "Total", negate = TRUE),
    year == max(year)
  ) |>
  count(provider_type, category, wt = value, name = "value") |>
  group_by(provider_type) |>
  mutate(share = value / sum(value)) |>
  ungroup()

#bundle tab data points/frames
providers <- list(
  bans = ban_providers,
  years = ban_providers_years,
  df_provider_counts = df_provider_counts,
  df_hospital_subset = df_hospital_subset
)

# export
write_rds(providers, "Dataout/providers.rds")
