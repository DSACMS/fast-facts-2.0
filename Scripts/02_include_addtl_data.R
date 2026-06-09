# PROJECT:  fast-facts-2.0
# PURPOSE:  append additional datasets to FF files
# AUTHOR:   A.Chafetz | CMS
# REF ID:   42ad981d
# LICENSE:  MIT
# DATE:     2026-05-29
# UPDATED:  2026-06-08

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(arrow, warn.conflicts = FALSE)

source("Scripts/99_functions.R")

# GLOBAL VARIABLES --------------------------------------------------------

#data output directory
dir_out <- "Dataout"

#path to main FF data file
path <- list.files(dir_out, ".parquet", full.names = TRUE)

#temp dir for unzipping Medicare historic files
dir_temp <- tempdir()

#unzip Medicare historic files (also zipped)
map(
  .x = list.files("Data", "CMS Program", full.names = TRUE),
  .f = ~ unzip(.x, exdir = dir_temp, junkpaths = TRUE)
)

#store paths for sub-zipped Excel files from data.cms.gov
path_medicare_prog_stats <- list.files(
  dir_temp,
  "Enroll.*.zip",
  full.names = TRUE
)

#path data.cms.gov download
path_medicare_monthly_enroll <- "Data/Medicare_Monthly_Enrollment_Jan_2026.zip"

#path to historic part a reduce premiums
path_addtl_premiums <- "Data/cms_newsroom_fact-sheets_premiums.csv"

#path for Medicaid historic data
path_medicaid <- list.files("Data", "mac", full.names = TRUE)

#unzip NHE by Type data
path_nhe_type <- list.files("Data", "nhe.*tables.zip", full.names = TRUE)

# IMPORT ------------------------------------------------------------------

#read in Fast Facts structured dataset
df_ff <- read_parquet(path)

#read in NHE data
df_nhe_type <- read_nhe_types(path_nhe_type)

#read in historic Medicare pop data
df_medicare_prog_stats <- path_medicare_prog_stats |>
  map(read_cms_prog_stats) |>
  list_rbind()

#read in newer Medicare historic pop data
df_medicare_monthly_enroll <- read_cms_gov_pop(path_medicare_monthly_enroll)

#read in historic Medicaid pop data
df_benes_medicaid <- path_medicaid |>
  map(read_medicaid_scorecard) |>
  list_rbind()

#additional data for reduce Part A premiums not included in FF
df_premium_a_reduced <- read_csv(path_addtl_premiums) |>
  mutate(data_year = as.integer(data_year))


# MUNGE -------------------------------------------------------------------

#combine rows from additional data sources
df_benes_addtl <-
  bind_rows(
    df_medicare_prog_stats,
    df_medicare_monthly_enroll,
    df_benes_medicaid,
    df_premium_a_reduced,
    df_nhe_type
  )

# CHECK ------------------------------------------------------------------

# df_ff_pops <- df_ff |>
#   filter(
#     source_tab == "Populations",
#     (area == "Medicare" & sub_category %in% c("Aged", "Disabled", "Original Medicare Enrollment", "MA & Other Health Plan Enrollment")) |
#     (area == "Medicaid & CHIP" & sub_category %in% c("Total","Children", "Medicaid Expansion Adults", "Dual Eligible"))
#   ) |>
#   count(area, sub_category, data_year, wt = value, name = "ff")

# df_check <- full_join(df_ff_pops, df_benes_addtl) |>
#   arrange(area, sub_category, data_year)

# # Row indices for each condition
# rows_missing_ff  <- which(is.na(df_check$ff))
# rows_diff        <- which(!is.na(df_check$ff) & abs(df_check$new - df_check$ff) / df_check$ff > 0.005)

# df_check |>
#   unite(sub_category, c(area, sub_category), sep = ": ") |>
#   gt::gt(groupname_col = "sub_category") |>
#   gt::fmt_missing() |>
#   gt::fmt_number(
#     columns = c("ff", "new"),
#     decimals = 1,
#     suffixing = TRUE
#   ) |>
#   # Condition 1: ff is missing → light green
#   gt::tab_style(
#     style = cell_fill(color = "#d4edda"),
#     locations = cells_body(rows = rows_missing_ff)
#   ) |>
#   # Condition 2: ff != new (and ff not missing) → light orange
#   gt::tab_style(
#     style = cell_fill(color = "#ffe5cc"),
#     locations = cells_body(rows = rows_diff, columns = c(ff, new))
#   )

# JOIN -------------------------------------------------------------------

# #subset additional data to only years (for each sub pop) not already in FF
# df_benes_addtl <- df_benes_addtl |>
#   anti_join(
#     df_ff |>
#       distinct(area, sub_category, data_year),
#     by = join_by(area, sub_category, data_year)
#   )

# #bind additional data onto current FF file
# df_ff <- bind_rows(df_ff, df_benes_addtl)

# drop subpop data from FF and use additional data points instead
df_ff <- df_ff |>
  anti_join(
    df_benes_addtl |>
      distinct(area, sub_category, data_year)
  ) |>
  bind_rows(df_benes_addtl)

# EXPORT -----------------------------------------------------------------

#export csv version
write_csv(df_ff, str_replace(path, "parquet", "csv"), na = "")

#export parquet version
write_parquet(df_ff, path)
