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

# VIZ - SHARE OF GDP -----------------------------------------------------

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
      xmax = 12,
      ymin = 0,
      ymax = 100 / 12
    ),
    color = matterhorn,
    fill = "white"
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

si_save("Images/context_share-gdp.png")


# VIZ  - INSURANCE BREAKDOWN ---------------------------------------------

v_insurance <- c(
  "Private Health Insurance",
  "Medicare",
  "Medicaid (Title XIX)",
  "CHIP (Title XIX & XXI)",
  "Department of Defense",
  "Department of Veterans Affairs"
)

df_nhe |>
  filter(type %in% v_insurance) |>
  mutate(type = fct_lump_prop(type, .1, w = value)) |>
  count(type, wt = value, name = "value") |>
  mutate(share = value / sum(value)) |>
  ggplot(aes(share, "x", fill = fct_reorder(type, share))) +
  geom_col(color = "white") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

si_preview()
