# PROJECT:  fast-facts-2.0
# PURPOSE:  data processing for dashboard pages
# AUTHOR:   A.Chafetz | CMS
# REF ID:   4b4e2514
# LICENSE:  MIT
# DATE:     2026-03-20
# UPDATED:  2026-04-27

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)

#add colors

source("Scripts/98_color_system.R")
source("Scripts/99_functions.R")

# GLOBAL VARIABLES --------------------------------------------------------

#data output directory
dir_out <- "Dataout"

#path to data file
(path <- list.files(dir_out, ".parquet", full.names = TRUE))

# IMPORT DATA ------------------------------------------------------------

#read in Fast Facts structured dataset
df_ff <- read_parquet(path)


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
  select(category, sub_category, data_year, value, value_fmt, value_sqrt)

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
  select(category, sub_category, data_year, value, value_fmt, n_icons)

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
  select(category, sub_category, data_year, value) |>
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
  select(category, data_year, value, value_fmt, n_icons)

#combine years
years <- c(
  nhe_yr = df_ff |>
    filter(
      is_latest == TRUE,
      category == "National Health Expenditures"
    ) |>
    distinct(period_type, data_year) |>
    unite(period, c(period_type, data_year), sep = " ") |>
    pull(),
  fed_spend_yr = df_ff |>
    filter(
      is_latest == TRUE,
      topic == "Financial"
    ) |>
    distinct(period_type, data_year) |>
    unite(period, c(period_type, data_year), sep = " ") |>
    pull()
)

#gather sources for footnote
v_context_sources <- df_ff |>
  filter(
    is_latest == TRUE,
    category %in%
      c(
        "National Health Expenditures",
        "Health Insurance",
        "Federal Program Spending",
        "FTE Employment"
      ),
    is_latest == TRUE
  ) |>
  distinct(source_origin) |>
  pull() |>
  sort() |>
  paste0(collapse = ", ")

v_context_footnote <- str_glue(
  "CMS Fast Facts {format(max(df_ff$release_date), '%B %Y')} Release ",
  "&bull; Data sources: {v_context_sources}"
)


#bundle tab datapoints/frames
context <- list(
  bans = bans_nhe,
  years = years,
  df_nhe_gdp_share = df_nhe_gdp_share,
  df_nhe_pc = df_nhe_pc,
  df_insurance = df_insurance,
  df_spend = df_spend,
  fte = fte,
  footnote = v_context_footnote
)

# export
write_rds(context, "Dataout/context.rds")


# BENEFICIARIES TAB ------------------------------------------------------

#Medicaid & CHIP expenditures
df_medicaid_exp <- df_ff |>
  filter(
    topic == "Expenditures",
    category == "Payments (by Selected Type of Service)",
    is_latest
  ) |>
  select(metric, category, sub_category, data_year, value) |>
  mutate(
    sub_category = str_replace(sub_category, "Laboratory", "Lab"),
    sub_category = str_replace(sub_category, "and", "&"),
    value_fmt = label_number(1, prefix = "$", scale_cut = cut_short_scale())(
      value
    )
  )

#medicare utilization
df_medicare_util <- df_ff |>
  filter(
    topic == "Utilization",
    is_latest == TRUE,
    metric %in% c("persons_served", "payments")
  ) |>
  filter_out(
    category == "Total (A and/or B)" |
      sub_category %in% c("Benefit Payments", "Administrative Expenses")
  ) #|>
# filter_out(
#   category %in% c("Part A", "Part B") & sub_category == "Total"
# )

#create z-score for plotting
df_medicare_util <- df_medicare_util |>
  group_by(metric) |>
  mutate(zscore = (value - mean(value)) / sd(value)) |>
  ungroup()


df_medicare_util <- df_medicare_util |>
  mutate(
    lab_exp = case_when(
      metric == "payments" ~ label_number(
        1,
        prefix = "$",
        scale_cut = cut_short_scale()
      )(value)
    ),
    lab_ben = case_when(
      metric == "persons_served" ~ str_glue(
        "{sub_category} ",
        "{label_number(1, scale_cut =  cut_short_scale())(value)}"
      )
    ),
    lab_pos = ifelse(metric == "persons_served", -1, 1.5)
  )

df_medicare_util <- df_medicare_util |>
  select(category, sub_category, metric, value) |>
  pivot_wider(
    names_from = metric
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
benes_years <- df_benes |>
  filter(is_latest) |>
  distinct(area, period_type, data_year) |>
  mutate(
    area = area |>
      str_extract("Medic(are|aid)") |>
      tolower()
  ) |>
  unite(period, c(period_type, data_year), sep = " ") |>
  deframe()

#Orig v MA trend
df_medicare_trend <- df_ff |>
  filter(
    topic == "Enrollment",
    sub_category %in% c("Original Medicare Enrollment", "MA Enrollment"),
    data_year >= 2020
  ) |>
  select(sub_category, metric, data_year, value) |>
  mutate(
    sub_category = sub_category |>
      str_remove(" Enrollment") |>
      str_replace("Original Medicare", "orig") |>
      tolower()
  ) |>
  group_by(sub_category) |>
  mutate(
    delta = (value / lag(value, order_by = data_year)) - 1
    # delta_lab = label_percent(1, style_positive = "plus")(delta)
  ) |>
  ungroup() |>
  pivot_wider(
    names_from = sub_category,
    values_from = c(value, delta)
  ) |>
  mutate(
    lab_orig = case_when(
      data_year == min(data_year) | data_year == max(data_year) ~ label_number(
        .1,
        scale_cut = cut_short_scale()
      )(value_orig)
    ),
    lab_ma = case_when(
      data_year == min(data_year) | data_year == max(data_year) ~ label_number(
        .1,
        scale_cut = cut_short_scale()
      )(value_ma)
    ),
    lab_orig = ifelse(
      data_year == max(data_year),
      str_glue(
        "{lab_orig} ",
        "({label_percent(1, style_positive = 'plus')(delta_orig)} prior)"
      ),
      lab_orig
    ),
    lab_ma = ifelse(
      data_year == max(data_year),
      str_glue(
        "{lab_ma} ({label_percent(1, style_positive = 'plus')(delta_ma)} prior)"
      ),
      lab_ma
    ),
    lab_orig_cat = case_when(data_year == 2022 ~ "Original Medicare"),
    lab_ma_cat = case_when(data_year == 2022 ~ "Medicare Advantage")
  )


#disagg groups
disagg_medicare <- c("Aged", "Disabled")
disaggs_medicaid <- c("Children", "Medicaid Expansion Adults", "Dual Eligible")

#diaggregate trends
df_disagg_trend <- df_ff |>
  filter(
    topic == "Enrollment",
    (area == "Medicaid & CHIP" & sub_category %in% disaggs_medicaid) |
      (area == "Medicare" & sub_category %in% disagg_medicare),
    data_year >= 2020
  ) |>
  select(area, metric, sub_category, period_type, data_year, value) |>
  mutate(
    sub_category = ifelse(
      sub_category == "Medicaid Expansion Adults",
      "ME Adults",
      sub_category
    ),
    fill_color = recode_values(
      sub_category,
      "Children" ~ ff_colors$base[["plum"]],
      "Dual Eligible" ~ ff_colors$scales$teal[["200"]],
      "ME Adults" ~ ff_colors$scales$teal[["900"]],
      "Aged" ~ ff_colors$scales$cobolt[["900"]],
      "Disabled" ~ ff_colors$scales$cobolt[["200"]]
    )
  ) |>
  group_by(sub_category) |>
  mutate(
    val_pt = case_when(
      data_year == min(data_year) | data_year == max(data_year) ~ value
    ),
    lab_val = case_when(
      data_year %in% c(min(data_year), max(data_year)) ~ label_number(
        1,
        scale_cut = cut_short_scale()
      )(value)
    )
  ) |>
  ungroup()

#gather sources for footnote
v_benes_sources <- df_ff |>
  filter(
    topic %in% c("Expenditures", "Utilization", "Enrollment"),
    is_latest == TRUE
  ) |>
  distinct(source_origin) |>
  mutate(
    source_origin = str_remove(
      source_origin,
      "Office of Enterprise Data & Analytics/"
    )
  ) |>
  pull() |>
  sort() |>
  paste0(collapse = ", ")

v_benes_footnote <- str_glue(
  "CMS Fast Facts {format(max(df_ff$release_date), '%B %Y')} Release ",
  "&bull; Data sources: {v_benes_sources}"
)


#bundle tab data points/frames
beneficiaries <- list(
  bans = benes_bans,
  years = benes_years,
  df_medicare_util = df_medicare_util,
  df_medicaid_exp = df_medicaid_exp,
  df_medicare_trend = df_medicare_trend,
  df_disagg_trend = df_disagg_trend,
  footnote = v_benes_footnote
)

# export
write_rds(beneficiaries, "Dataout/beneficiaries.rds")


# COST SHARING TAB -------------------------------------------------------

#subset data for cost sharing data
df_cs_trend <- df_ff |>
  filter(topic == "Cost Sharing") |>
  filter(data_year >= max(data_year) - 1) |>
  unite(period, c(period_type, data_year), sep = " ") |>
  select(topic, category, sub_category, metric, period, value, bound)

#create necessary fields for viz
df_cs_trend <- df_cs_trend |>
  mutate(
    ln_group = ifelse(
      !is.na(bound),
      str_glue("{category} {sub_category} {bound}"),
      str_glue("{category} {sub_category}")
    ),
    fill_color = ifelse(
      period == max(period),
      ff_colors$scales$cobolt["200"],
      "white"
    ),
    order = ifelse(period == max(period), value, 0)
  ) |>
  group_by(ln_group) |>
  mutate(
    mid_pt = mean(value, na.rm = TRUE),
    delta = value / lag(value) - 1,
    delta_lab = label_percent(1, style_positive = "plus")(delta)
  ) |>
  ungroup()

df_cs_trend <- df_cs_trend |>
  mutate(
    metric_lab = case_when(
      metric == "coinsurance" ~ "Coinsurance (Part A)",
      sub_category %in%
        c(
          "Out-of-Pocket Threshold",
          "Initial Coverage Limit"
        ) ~ "Other (Part D)",
      TRUE ~ str_glue("{str_to_title(metric)}s")
    ),
    metric_lab = factor(
      metric_lab,
      c("Premiums", "Coinsurance (Part A)", "Deductibles", "Other (Part D)")
    ),
    sub_category = case_when(
      !is.na(bound) ~ str_glue("{sub_category} ({bound} bound)"),
      metric == "deductible" &
        category == "Part A" ~ "Part A (Inpatient Hospital)",
      metric == "deductible" & category == "Part D" ~ "Part D (Maximum)",
      metric == "deductible" ~ category,
      TRUE ~ str_remove(sub_category, "Coinsurance/")
    )
  )

df_cs_trend <- df_cs_trend |>
  mutate(
    val_curr = case_when(
      period == max(period) ~ label_comma(1, prefix = "$")(value)
    )
  ) |>
  group_by(category, sub_category, metric) |>
  fill(val_curr, .direction = "updown") |>
  ungroup() |>
  mutate(
    sub_category = ifelse(
      str_detect(sub_category, "upper"),
      str_glue("{sub_category} [{max(df_cs_trend$period)} = {val_curr}]"),
      str_glue("{sub_category} [{val_curr}]")
    )
  )

#gather sources for footnote
v_costsharing_sources <- df_ff |>
  filter(
    topic %in% c("Cost Sharing"),
    is_latest == TRUE
  ) |>
  distinct(source_origin) |>
  pull() |>
  sort() |>
  paste0(collapse = ", ")

v_costsharing_footnote <- str_glue(
  "CMS Fast Facts {format(max(df_ff$release_date), '%B %Y')} Release ",
  "&bull; Data sources: {v_costsharing_sources}"
)


#bundle tab data points/frames
cost_sharing <- list(
  df_cs_trend = df_cs_trend,
  footnote = v_costsharing_footnote
)

# export
write_rds(cost_sharing, "Dataout/cost_sharing.rds")


# PROVIDERS --------------------------------------------------------------

ban_providers <- df_ff |>
  filter(
    topic == "Providers",
    str_detect(category, "Total"),
    is_latest == TRUE
  ) |>
  unite(period, c(period_type, data_year), sep = " ") |>
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
df_hospital_subset <- df_ff |>
  filter(
    topic == "Providers",
    category == "Hospitals",
    is_latest == TRUE,
  ) |>
  select(sub_category, value) |>
  mutate(
    share = value / sum(value),
    squares = round(share * 100)
  )

if (sum(df_hospital_subset$squares) > 100) {
  df_hospital_subset <- df_hospital_subset |>
    mutate(
      squares = ifelse(
        squares == max(squares),
        squares - (sum(df_hospital_subset$squares) - 100),
        squares
      )
    )
}

# add color
df_hospital_subset <- df_hospital_subset |>
  bind_cols(
    tibble(
      fill_color = c(
        ff_colors$base[(n_distinct(df_hospital_subset$sub_category) - 1):1],
        ff_colors$scales$charcoal[["200"]]
      )
    )
  )

#provider coutns
df_provider <- df_ff |>
  filter(
    topic == "Providers",
    str_detect(category, "Total", negate = TRUE),
    is_latest == TRUE,
  ) |>
  count(provider_type, category, wt = value, name = "value") |>
  group_by(provider_type) |>
  mutate(share = value / sum(value)) |>
  ungroup() |>
  mutate(
    value_fmt = label_number(1, scale_cut = cut_short_scale())(value),
    category = category |>
      fct_reorder(value) |>
      fct_relevel("All Other Providers")
  )

#instutional providers
df_provider_inst <- df_provider |>
  filter(provider_type == "Institutional")

#non-instutional providers
df_provider_noninst <- df_provider |>
  filter(provider_type == "Non-Institutional")

#DMEPOS providers table
df_providers_dmepos_tbl <- df_provider |>
  filter(provider_type == "DMEPOS")

df_providers_dmepos_tbl <- df_providers_dmepos_tbl |>
  arrange(desc(category)) |>
  bind_cols(
    tibble(
      fill_color = c(
        ff_colors$scales$green[4:1],
        rep(
          ff_colors$scales$charcoal[["200"]],
          nrow(df_providers_dmepos_tbl) - 4
        )
      )
    )
  ) |>
  select(-c(provider_type, share, value_fmt)) |>
  relocate(fill_color, .before = 1)

#DMEPOS providers viz
df_providers_dmepos <- df_provider |>
  filter(provider_type == "DMEPOS") |>
  mutate(
    category = fct_lump_n(
      category,
      5,
      w = share,
      other_level = "All Other DMEPOS Providers"
    )
  ) |>
  count(provider_type, category, wt = value, name = "value") |>
  mutate(
    share = value / sum(value),
    category = category |>
      fct_reorder(share) |>
      fct_relevel("All Other DMEPOS Providers"),
    fill_color = c(
      unname(ff_colors$scales$green[4:1]),
      ff_colors$scales$charcoal[["200"]]
    )
  )

#gather sources for footnote
v_providers_sources <- df_ff |>
  filter(
    topic %in% c("Providers"),
    is_latest == TRUE
  ) |>
  distinct(source_origin) |>
  pull() |>
  sort() |>
  paste0(collapse = ", ")

v_providers_footnote <- str_glue(
  "CMS Fast Facts {format(max(df_ff$release_date), '%B %Y')} Release ",
  "&bull; Data sources: {v_providers_sources}"
)

#bundle tab data points/frames
providers <- list(
  bans = ban_providers,
  years = ban_providers_years,
  df_provider_inst = df_provider_inst,
  df_provider_noninst = df_provider_noninst,
  df_providers_dmepos = df_providers_dmepos,
  df_providers_dmepos_tbl = df_providers_dmepos_tbl,
  df_hospital_subset = df_hospital_subset,
  footnote = v_providers_footnote
)

# export
write_rds(providers, "Dataout/providers.rds")
