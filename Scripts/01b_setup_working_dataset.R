# PROJECT:  fast-facts-2.0
# PURPOSE:  append additional datasets to FF files
# AUTHOR:   A.Chafetz | CMS
# REF ID:   42ad981d
# LICENSE:  MIT
# DATE:     2026-05-29
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(arrow, warn.conflicts = FALSE)


# GLOBAL VARIABLES --------------------------------------------------------

#temp dir for unzipping
dir_temp <- tempdir()

#path to data file
(path <- list.files(dir_out, ".parquet", full.names = TRUE))

#unzip historic file
map(
  .x = list.files("Data", "CMS Program", full.names = TRUE),
  .f = ~ unzip(.x, exdir = dir_temp, junkpaths = TRUE)
)

#store paths for sub-zipped Excel files from data.cms.gov
path_zip <- list.files(dir_temp, "zip", full.names = TRUE)

#path data.cms.gov download
path_cms_benes <- "Data/Medicare_Monthly_Enrollment_Jan_2026.zip"

# IMPORT ------------------------------------------------------------------

# MUNGE -------------------------------------------------------------------

# VIZ ---------------------------------------------------------------------
