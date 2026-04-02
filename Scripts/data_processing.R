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
library(janitor, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)

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


# IMPORT DATA ------------------------------------------------------------

df_ff <- read_parquet("Dataout/Fast_Facts_std.rds")


# CONTEXT TAB ------------------------------------------------------------

#extract NHE total
nhe_total <- df_ff |>
  filter(
    is_latest == TRUE,
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
df_nhe_gdp_share <- df_ff |>
  filter(
    is_latest == TRUE,
    category == "National Health Expenditures",
    sub_category == "% of GDP"
  ) |>
  mutate(
    value_fmt = label_percent(1)(value),
    value_sqrt = sqrt(value * 100)
  ) |>
  select(category, sub_category, year, value, value_fmt, value_sqrt)

#extract NHE per capita
df_nhe_pc <- df_ff |>
  filter(
    is_latest == TRUE,
    category == "National Health Expenditures",
    sub_category == "Per Capita"
  ) |>
  mutate(
    value_fmt = label_comma(1, prefix = "$")(value),
    n_icons = round(value / 1000)
  ) |>
  select(category, sub_category, year, value, value_fmt, n_icons)

#extact health insurnace total
health_insurance <- df_ff |>
  filter(
    is_latest == TRUE,
    category == "Health Insurance",
  ) |>
  count(wt = value) |>
  mutate(
    value_fmt = label_number(.1, prefix = "$", scale_cut = cut_short_scale())(n)
  ) |>
  pull()

#create a dataframe of health spending by type
df_insurance <- df_ff |>
  filter(
    is_latest == TRUE,
    category == "Health Insurance",
  ) |>
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

#extract fed spending
fed_spend <- df_ff |>
  filter(
    is_latest == TRUE,
    category == "Federal Program Spending"
  ) |>
  count(wt = value) |>
  mutate(
    value_fmt = label_number(.1, prefix = "$", scale_cut = cut_short_scale())(n)
  ) |>
  pull()

#extract numbers for BAN in tab
bans_nhe <- c(
  nhe_total = nhe_total,
  health_insurance = health_insurance,
  fed_spend = fed_spend
)

# financial data for plot
df_spend <- df_ff |>
  filter(
    topic == "Financial",
    is_latest == TRUE,
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
fte <- df_ff |>
  filter(
    is_latest == TRUE,
    category == "FTE Employment"
  ) |>
  mutate(
    value_fmt = label_comma()(value),
    n_icons = round(value / 1e3)
  ) |>
  select(category, year, value, value_fmt, n_icons)

#combine years
years <- c(
  nhe_yr = extract_sheet_year(path, "NHE")$period,
  fed_spend_yr = extract_sheet_year(path, "CMS Financial Data")$period
)


#bundle tab datapoints/frames
context <- list(
  bans = bans_nhe,
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

df_ff |>
  filter(
    topic %in% c("Medicare Utilization", "Medicare Part D"),
    is_latest == TRUE
  )


df_medicare_util <- df_ff |>
  filter(
    topic == "Utilization",
    is_latest == TRUE,
    metric %in% c("persons_served", "payments")
  ) |>
  filter_out(
    category == "Total (A and/or B)" |
      sub_category %in% c("Benefit Payments", "Administrative Expenses")
  ) |>
  filter_out(
    category %in% c("Part A", "Part B") & sub_category == "Total"
  )

#create z-score for plotting
df_medicare_util <- df_medicare_util |>
  group_by(metric) |>
  mutate(zscore = (value - mean(value)) / sd(value)) |>
  ungroup()


df_medicare_util <- df_medicare_util |>
  mutate(
    lab_ben = case_when(
      metric == "persons_served" ~ str_glue(
        "{category}\n{label_number(1, scale_cut =  cut_short_scale())(value)}"
      )
    ),
    lab_exp = case_when(
      metric == "payments" ~ label_number(
        1,
        prefix = "$",
        scale_cut = cut_short_scale()
      )(value)
    ),
  )


#extract numbers for BAN in tab
df_benes <- df_ff |>
  filter(
    topic == "Enrollment",
    category %in% c("Parts A and/or B", "Part D", "Medicaid & CHIP"),
    sub_category == "Total",
    is_latest == TRUE
  ) |>
  mutate(
    name = case_when(
      category == "Parts A and/or B" ~ "medicare_ab",
      category == "Part D" ~ "medicare_d",
      category == "Medicaid & CHIP" ~ "medicaid"
    ),
    value_fmt = label_number(1, scale_cut = cut_short_scale())(value)
  )

benes_bans <- df_benes |>
  select(name, value_fmt) |>
  deframe()


#combine years
benes_years <- c(
  ban_year = unique(df_benes$year)
)

#Orig v MA trend <<<<<<---- LEFT OFF HERE
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
