# PROJECT:  fast-facts-2.0
# PURPOSE:  setup working dataframe
# AUTHOR:   A.Chafetz | CMS
# REF ID:   ebcbe8b1
# LICENSE:  MIT
# DATE:     2026-04-02
# UPDATED:  2026-04-30

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(arrow, warn.conflicts = FALSE)

source("Scripts/99_functions.R")

# GLOBAL VARIABLES --------------------------------------------------------

#data file
dir_data <- "Data"

#output location
dir_out <- "Dataout"

#release filename/path
release <- file.path(
  dir_out,
  str_glue("CMSFastFacts_SD_{format.Date(today(), '%Y-%m')}")
)

# pull in all historic files as well as current
files <- list.files(dir_data, recursive = TRUE, full.names = TRUE) |>
  keep(~ str_detect(.x, "~\\$", negate = TRUE)) #drop any open files

#get the latest release from each year (some years have multiple releases)
files <- tibble(source = files) |>
  gen_release_dt() |>
  group_by(year(release_date)) |>
  filter(release_date == max(release_date)) |>
  ungroup() |>
  arrange(release_date) |>
  pull(source)


# IMPORT ------------------------------------------------------------------

#read in all tabs from Excel files and combine
df_ff <- files |>
  map(import_fast_facts) |>
  list_rbind()


# MUNGE -------------------------------------------------------------------

# add release date (if needed to filter down where there are overlapping years)
df_ff <- df_ff |>
  gen_release_dt()

#keep latest observations for each year
df_ff <- df_ff |>
  group_by(topic, area, category, sub_category, metric, bound, data_year) |>
  filter(release_date == max(release_date)) |>
  ungroup()

#identity the latest observation (metrics may have different most recent year)
df_ff <- df_ff |>
  group_by(topic, area) |> #including area which may be different for Enrollment
  mutate(is_latest = data_year == max(data_year)) |>
  ungroup()


# EXPORT -----------------------------------------------------------------

#export csv version
write_csv(df_ff, str_glue("{release}.csv"), na = "")

#export parquet version
write_parquet(df_ff, str_glue("{release}.parquet"))
