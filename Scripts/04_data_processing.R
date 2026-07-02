# PROJECT:  fast-facts-2.0
# PURPOSE:  data processing for dashboard pages
# AUTHOR:   A.Chafetz | CMS
# REF ID:   4b4e2514
# LICENSE:  MIT
# DATE:     2026-03-20
# UPDATED:  2026-07-02

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

#path to main FF data file
path <- list.files(dir_out, "FastFactsPlus.*.parquet", full.names = TRUE)

# IMPORT DATA ------------------------------------------------------------

#read in Fast Facts structured dataset
df_ff <- read_parquet(path)


# HEALTH SPENDING TAB ----------------------------------------------------

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
    fill_color = ff_colors$base[["green"]]
  ) |>
  select(category, sub_category, data_year, value, value_fmt, fill_color)

df_nhe_gdp_share <- df_nhe_gdp_share |>
  bind_rows(
    df_nhe_gdp_share |>
      mutate(
        category = "Other",
        value = 1 - value,
        value_fmt = label_percent(1)(value),
        fill_color = ff_colors$scales$warmgray[["100"]]
      )
  )


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

#other NHE subcategories
df_nhe_sources <- df_ff |>
  filter(
    is_latest == TRUE,
    category == "National Health Expenditures",
    sub_category != "Total",
    metric == "expenditures"
  )

#format for plotting
df_nhe_sources <- df_nhe_sources |>
  mutate(
    sub_category = recode_values(
      sub_category,
      "National Health Insurance Expenditures" ~ "National Health\nInsurance Expenditures",
      "Other Third Party Payers and Programs" ~ "Other Third-Party\nPayers and Programs",
      "Government Public Health Activities" ~ "Government Public\nHealth Activities",
      default = sub_category
    ),
    value_format = fmt_dynamic(value),
    fill_color = case_when(
      # sub_category == "National Health\nInsurance Expenditures" ~ "#6E6E6E",
      TRUE ~ ff_colors$base[["green"]]
    )
  )

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
    value_fmt = fmt_dynamic(value),
    share = value / sum(value),
    fill_color = case_when(
      str_detect(sub_category, "Medicare") ~ ff_colors$base[["azure"]],
      str_detect(sub_category, "Medicaid") ~ ff_colors$base[["teal"]],
      str_detect(sub_category, "CHIP") ~ ff_colors$base[["plum"]],
      TRUE ~ ff_colors$scales$warmgray[["500"]]
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
    category = ifelse(
      category == "Fraud",
      "Health Care Fraud & Abuse Control",
      category
    ),
    category = str_glue("{category} ({fmt_dynamic(sum(value))})")
  ) |>
  ungroup() |>
  mutate(
    category = fct_reorder(category, value, sum),
    sub_category = ifelse(
      str_detect(category, "Fraud"),
      "Total Funding",
      sub_category
    ),
    value_fmt = fmt_billions(value),
    share = ifelse(category == "Health Care Fraud & Abuse Control", NA, share),
    share_fmt = label_percent(1)(share),
    fill_color = recode_values(
      sub_category,
      "Medicare Benefits" ~ ff_colors$base[["azure"]],
      "Total Medicaid" ~ ff_colors$base[["teal"]],
      "CHIP" ~ ff_colors$base[["plum"]],
      "Other Spending" ~ ff_colors$scales$charcoal[["200"]],
      default = "#015390"
    ),
    sub_category = ifelse(
      str_detect(category, "Federal Program"),
      str_glue("{sub_category} ({share_fmt})"),
      sub_category
    ),
    sub_category = fct_reorder(sub_category, value, sum)
  ) |>
  select(category, sub_category, value, share, value_fmt, share_fmt, fill_color)

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
  "CMS Fast Facts {format(max(df_ff$release_date, na.rm = TRUE), '%B %Y')} Release ",
  "&bull; Data sources: {v_context_sources}"
)


#bundle tab datapoints/frames
context <- list(
  bans = bans_nhe,
  years = years,
  df_nhe_gdp_share = df_nhe_gdp_share,
  df_nhe_pc = df_nhe_pc,
  df_nhe_sources = df_nhe_sources,
  df_insurance = df_insurance,
  df_spend = df_spend,
  fte = fte,
  footnote = v_context_footnote
)

# export
write_rds(context, "Dataout/context.rds")


# ENROLLMENT TAB ---------------------------------------------------------

#extract numbers for BAN in tab
df_enrollment <- df_ff |>
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

enrollment_bans <- df_enrollment |>
  select(name, value_fmt) |>
  deframe()


#ban years
enrollment_years <- df_enrollment |>
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
    sub_category %in%
      c("Original Medicare Enrollment", "MA & Other Health Plan Enrollment")
  ) |>
  select(sub_category, data_year, value) |>
  mutate(
    sub_category = sub_category |>
      str_extract("MA|Orig") |>
      tolower()
  )

df_medicare_trend <- df_medicare_trend |>
  group_by(data_year) |>
  mutate(
    share = value / sum(value)
  ) |>
  ungroup()

df_medicare_trend <- df_medicare_trend |>
  mutate(
    end_pt = case_when(data_year %in% range(data_year) ~ value),
    end_labs = case_when(
      data_year %in% range(data_year) ~
        str_glue(
          "{label_number(.1, scale_cut = cut_short_scale())(value)} ({label_percent(1)(share)})"
        )
    ),
    lab_cat = case_when(data_year == 2016 ~ sub_category),
    lab_cat = recode_values(
      lab_cat,
      "orig" ~ "Original Medicare",
      "ma" ~ "Medicare Advantage"
    ),
    lab_cat_pos = case_when(
      !is.na(lab_cat) & sub_category == "orig" ~ value + 5e6,
      !is.na(lab_cat) & sub_category == "ma" ~ value - 5e6
    ),
    fill_color = ifelse(
      sub_category == "orig",
      ff_colors$scales$cobolt[["900"]],
      ff_colors$scales$cobolt[["200"]]
    )
  )

df_medicare_trend <- df_medicare_trend |>
  group_by(data_year) |>
  mutate(
    lab_pos = ifelse(
      value == max(value, na.rm = TRUE),
      value + 3e6,
      value - 3e6
    )
  ) |>
  ungroup()


#disagg groups
subpop_medicare <- c("Aged", "Disabled")
subpop_medicaid <- c("Children", "Medicaid Expansion Adults", "Dual Eligible")

#diaggregate trends
df_disagg_trend <- df_ff |>
  filter(
    topic == "Enrollment",
    (area == "Medicare" & sub_category %in% subpop_medicare) |
      (area == "Medicaid & CHIP" & sub_category %in% subpop_medicaid)
  ) |>
  select(area, metric, sub_category, period_type, data_year, value) |>
  mutate(
    fill_color = recode_values(
      sub_category,
      "Children" ~ ff_colors$base[["plum"]],
      "Dual Eligible" ~ ff_colors$scales$teal[["200"]],
      "Medicaid Expansion Adults" ~ ff_colors$scales$teal[["900"]],
      "Aged" ~ ff_colors$scales$cobolt[["900"]],
      "Disabled" ~ ff_colors$scales$cobolt[["200"]],
      default = ff_colors$scales$charcoal[['200']]
    )
  ) |>
  group_by(area, sub_category) |>
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
v_enrollment_sources <- df_ff |>
  filter(
    topic %in% c("Enrollment"),
    is_latest == TRUE
  ) |>
  add_row(source_origin = "CMS/Office of Enterprise Data & Analytics") |>
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

v_enrollment_footnote <- str_glue(
  "CMS Fast Facts {format(max(df_ff$release_date, na.rm = TRUE), '%B %Y')} Release ",
  "&bull; Data sources: {v_enrollment_sources}"
)


#bundle tab data points/frames
enrollment <- list(
  bans = enrollment_bans,
  years = enrollment_years,
  # df_medicare_util = df_medicare_util,
  # df_medicaid_exp = df_medicaid_exp,
  df_medicare_trend = df_medicare_trend,
  df_disagg_trend = df_disagg_trend,
  footnote = v_enrollment_footnote
)

# export
write_rds(enrollment, "Dataout/enrollment.rds")


# UTILIZATION TAB --------------------------------------------------------

#extract numbers for BAN in tab
df_utilization_bans <- df_ff |>
  filter(
    topic == "Utilization",
    category %in% c("Total (A and/or B)", "Part D"),
    sub_category == "Total",
    is_latest == TRUE,
    metric %in% c("persons_served", "payments")
  ) |>
  mutate(
    category = ifelse(
      category == "Total (A and/or B)",
      "Medicare Total",
      category
    )
  ) |>
  unite(period, c(period_type, data_year), sep = " ") |>
  select(category, metric, period, value) |>
  mutate(
    value = ifelse(
      metric == "payments",
      label_number(1, prefix = "$", scale_cut = cut_short_scale())(value),
      label_number(1, scale_cut = cut_short_scale())(value)
    )
  ) |>
  unite(category, c(category, metric))

df_medicaid_exp_ban <- df_ff |>
  filter(
    topic == "Expenditures",
    category == "Payments (by Selected Type of Service)",
    is_latest
  ) |>
  unite(period, c(period_type, data_year), sep = " ") |>
  count(area, period, wt = value, name = "value") |>
  rename(category = area) |>
  mutate(
    value = label_number(1, prefix = "$", scale_cut = cut_short_scale())(value)
  )


df_utilization_bans <- df_utilization_bans |>
  bind_rows(df_medicaid_exp_ban) |>
  mutate(
    category = category |>
      str_replace_all(" ", "_") |>
      str_remove("&_") |>
      tolower()
  )

utilization_bans <- df_utilization_bans |>
  select(-period) |>
  deframe()

utilization_years <- df_utilization_bans |>
  select(-value) |>
  deframe()


#medicare utilization
df_medicare_util <- df_ff |>
  filter(
    topic == "Utilization",
    category != "Part D",
    sub_category != "Total",
    is_latest == TRUE
  )

#use deduplicated HHA values (instead of Part A and B shown in FF)
df_medicare_util <- df_medicare_util |>
  filter_out(
    category %in% c("Part A", "Part B"),
    sub_category == "Home Health Agency"
  )

#setup formatting for viz
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
  select(category, sub_category, metric, data_year, value) |>
  pivot_wider(
    names_from = metric
  ) |>
  mutate(
    fill_shape = recode_values(
      category,
      "Part A" ~ 21L,
      "Part B" ~ 23L,
      default = 22L,
    )
  )

#pull HHA year to add to caption if needed
v_hha_yr <- df_medicare_util |>
  filter(sub_category == "Home Health Agency") |>
  pull(data_year)

#add caption if HHA is a different year than Fast Facts
v_hha_caption <- ifelse(
  length(unique(df_medicare_util$data_year)) > 1,
  str_glue("Note: Values for Home Health Agency are for CY {v_hha_yr}"),
  NULL
)

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

#part D
df_part_d <- df_ff |>
  filter(
    topic == "Utilization",
    category == "Part D"
  ) |>
  filter_out(
    sub_category == "Total",
    metric %in% c("payments") #"persons_served"
  )

df_part_d <- df_part_d |>
  mutate(
    sub_category = case_when(
      sub_category == "Total" & metric == "payments" ~ "Total Expenditures",
      metric == "persons_served" ~ "Utilizing Beneficiaries",
      metric == "rx_events" ~ "Prescription Drug Events",
      TRUE ~ sub_category
    ),
    group = ifelse(
      str_detect(sub_category, "Exp|Pay"),
      "Expenditures",
      "Utilization"
    ),
    end_point = case_when(data_year %in% range(data_year) ~ value),
    point_value_lab = case_when(
      data_year %in% range(data_year) & group == "Utilization" ~
        label_number(accuracy = .1, scale_cut = cut_short_scale())(value),
      data_year %in% range(data_year) & group == "Expenditures" ~
        label_number(
          prefix = "$",
          accuracy = .1,
          scale_cut = cut_short_scale()
        )(value)
    ),
    lab_offset = ifelse(data_year == max(data_year), -.2, 1.2)
  ) |>
  select(
    group,
    sub_category,
    data_year,
    value,
    end_point,
    point_value_lab,
    lab_offset
  )


#gather sources for footnote
v_utilization_sources <- df_ff |>
  filter(
    topic %in% c("Expenditures", "Utilization"),
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

v_enrollment_footnote <- str_glue(
  "CMS Fast Facts {format(max(df_ff$release_date, na.rm = TRUE), '%B %Y')} Release ",
  "&bull; Data sources: {v_utilization_sources}"
)


#bundle tab data points/frames
utilization <- list(
  bans = utilization_bans,
  years = utilization_years,
  df_medicare_util = df_medicare_util,
  v_hha_caption = v_hha_caption,
  df_medicaid_exp = df_medicaid_exp,
  df_part_d = df_part_d,
  footnote = v_enrollment_footnote
)

# export
write_rds(utilization, "Dataout/utilization.rds")

# COST SHARING TAB -------------------------------------------------------

#subset data for cost sharing data
df_cs_trend <- df_ff |>
  filter(topic == "Cost Sharing") |>
  select(topic, category, sub_category, metric, data_year, value, bound)

#create necessary fields for viz
df_cs_trend <- df_cs_trend |>
  mutate(
    ln_group = ifelse(
      !is.na(bound),
      str_glue("{category} {sub_category} {bound}"),
      str_glue("{category} {sub_category}")
    ),
    order = ifelse(data_year == max(data_year), value, 0)
  )

df_cs_trend <- df_cs_trend |>
  mutate(
    metric_lab = case_when(
      metric == "coinsurance" ~ "Part A Coinsurance", #"Coinsurance (Part A)"
      sub_category %in%
        c(
          "Out-of-Pocket Threshold",
          "Initial Coverage Limit"
        ) ~ "Part D Out-of-Pocket Threshold",
      TRUE ~ str_glue("{str_to_title(metric)}s")
    ),
    metric_lab = factor(
      metric_lab,
      c(
        "Premiums",
        "Deductibles",
        "Part A Coinsurance", #"Coinsurance (Part A)"
        "Part D Out-of-Pocket Threshold"
      )
    ),
    sub_category = recode_values(
      sub_category,
      "Coinsurance/Day (Days 61-90)" ~
        "Coinsurance/Inpatient Hospital Day (Days 61-90)",
      "Coinsurance/SNF Day (Days 21-100)" ~
        "Coinsurance/Skilled Nursing Facility Day (Days 21-100)",
      "Coinsurance/LTR Day" ~
        "Coinsurance/Long Term Reserve Day",
      default = sub_category
    ),
    sub_category = case_when(
      !is.na(bound) ~ str_glue("{sub_category}<br>*{str_to_title(bound)}*"),
      metric == "deductible" &
        category == "Part A" ~ "Part A<br>*Inpatient Hospital*",
      metric == "deductible" & category == "Part D" ~ "Part D<br>*Maximum*",
      metric == "deductible" ~ category,
      TRUE ~ sub_category |>
        str_remove("Coinsurance/") |>
        str_replace(" \\(", "<br>*") |>
        str_replace("\\)", "*")
    ),
    sub_category = ifelse(
      str_detect(sub_category, "<br>", negate = TRUE),
      str_glue("{sub_category}<br>"),
      sub_category
    )
  )

df_cs_trend <- df_cs_trend |>
  group_by(ln_group) |>
  mutate(
    val_curr = case_when(
      data_year == max(data_year) ~ label_comma(1, prefix = "$")(value),
    ),
    val_pt = case_when(data_year %in% range(data_year) ~ value),
    lab_val = case_when(
      data_year %in% range(data_year) ~ label_currency(
        # data_year %in% range(data_year) ~ label_number(
        #   1,
        #   prefix = "$",
        #   scale_cut = cut_short_scale()
      )(value)
    ),
    fill_color = case_when(
      category == "Part A" ~ ff_colors$scales$cobolt[["200"]],
      category == "Part B" ~ ff_colors$scales$cobolt[["700"]],
      category == "Part D" ~ ff_colors$scales$cobolt[["900"]],
      str_detect(sub_category, "Part A") ~ ff_colors$scales$cobolt[["200"]],
      str_detect(sub_category, "Part B") ~ ff_colors$scales$cobolt[["700"]],
    )
  ) |>
  ungroup() |>
  group_by(category, sub_category, metric) |>
  fill(val_curr, .direction = "updown") |>
  ungroup()

df_recent <- df_cs_trend |>
  filter(data_year >= max(data_year) - 1) |>
  distinct(category, sub_category)

df_cs_trend <- df_cs_trend |>
  inner_join(
    df_recent,
    by = join_by(category, sub_category)
  )

v_cs_cats <- c(
  "Part A<br>*Full*",
  "Part A<br>*Reduced*",
  "Part B<br>*Standard*",
  "Part B<br>*Maximum*",
  "Part A<br>*Inpatient Hospital*",
  "Part B<br>",
  "Part D<br>*Maximum*",
  "Inpatient Hospital Day<br>*Days 61-90*",
  "Long Term Reserve Day<br>",
  "Skilled Nursing Facility Day<br>*Days 21-100*",
  "Out-of-Pocket Threshold<br>"
  # "Part A (Full)",
  # "Part A (Reduced)",
  # "Part B (Standard)",
  # "Part B (Maximum)",
  # "Part A (Inpatient Hospital)",
  # "Part B",
  # "Part D (Maximum)",
  # "Day (Days 61-90)",
  # "LTR Day",
  # "SNF Day (Days 21-100)",
  # "Out-of-Pocket Threshold"
)

df_cs_trend <- df_cs_trend |>
  mutate(sub_category = factor(sub_category, v_cs_cats))

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
  "CMS Fast Facts {format(max(df_ff$release_date, na.rm = TRUE), '%B %Y')} Release ",
  "&bull; Data sources: {v_costsharing_sources}"
)


#bundle tab data points/frames
cost_sharing <- list(
  df_cs_trend = df_cs_trend,
  footnote = v_costsharing_footnote
)

# export
write_rds(cost_sharing, "Dataout/cost_sharing.rds")


# PROVIDERS TAB ----------------------------------------------------------

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
    value_fmt = label_number(1, scale_cut = cut_short_scale())(value),
    share_fmt = label_percent(1)(share),
    fill_color = ff_colors$scales$saffron[["500"]]
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
    share_fmt = label_percent(1)(share),
    fill_color = recode_values(
      provider_type,
      "Non-Institutional" ~ ff_colors$scales$cobolt[["700"]],
      "Institutional" ~ ff_colors$scales$cobolt[["200"]],
      "DMEPOS" ~ ff_colors$scales$cobolt[["500"]]
    ),
    fill_color = ifelse(
      category == "Hospitals",
      ff_colors$scales$saffron[["500"]],
      fill_color
    ),
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

#DMEPOS providers viz
df_providers_dmepos <- df_provider |>
  filter(provider_type == "DMEPOS")

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
  "CMS Fast Facts {format(max(df_ff$release_date, na.rm = TRUE), '%B %Y')} Release ",
  "&bull; Data sources: {v_providers_sources}"
)

#bundle tab data points/frames
providers <- list(
  bans = ban_providers,
  years = ban_providers_years,
  df_provider_inst = df_provider_inst,
  df_provider_noninst = df_provider_noninst,
  df_providers_dmepos = df_providers_dmepos,
  df_hospital_subset = df_hospital_subset,
  footnote = v_providers_footnote
)

# export
write_rds(providers, "Dataout/providers.rds")
