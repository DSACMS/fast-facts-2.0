# PROJECT:  fast-facts-2.0
# PURPOSE:  populate context page
# AUTHOR:   A.Chafetz | CMS
# REF ID:
# LICENSE:  CC0
# DATE:     2026-03-17
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
# library(glue)
library(glitr) ##install.packages('glitr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
library(scales, warn.conflicts = FALSE)
# library(systemfonts)
# library(tidytext)
# library(patchwork)
# library(ggtext)

# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "79990d790609" #a reference to be placed in viz captions

#data file
path <- "Data/CMSFastFacts2025_508.xlsx"
tab <- "NHE"

# IMPORT ------------------------------------------------------------------

df_nhe <- read_excel(
  path,
  sheet = tab,
  skip = 2,
  col_names = c("type", "drop", "value")
) |>
  select(-drop)


# MUNGE -------------------------------------------------------------------

# extract data source
source_nhe <- df_nhe |>
  filter(str_detect(type, "SOURCE.*")) |>
  pull(type)

# keep only data points
df_nhe <- df_nhe |>
  filter_out(is.na(value))


# Data point - National Health Expenditure
df_nhe |>
  filter(type == "Total") |>
  mutate(value = value / 1000)

# VIZ ---------------------------------------------------------------------

#extract Share of GDP value
df_viz_gdp <- df_nhe |>
  filter(type == "% of GDP") |>
  mutate(value = value * 100)

#viz for share of GDP
df_viz_gdp |>
  ggplot() +
  geom_rect(
    aes(
      xmin = 0,
      xmax = 10,
      ymin = 0,
      ymax = 10
    ),
    color = matterhorn,
    fill = NA
  ) +
  geom_rect(
    aes(
      xmin = 0,
      xmax = sqrt(value),
      ymin = 0,
      ymax = sqrt(value)
    )
  ) +
  coord_equal() +
  theme_void()
