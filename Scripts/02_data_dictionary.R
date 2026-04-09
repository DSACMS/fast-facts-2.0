# PROJECT:  fast-fasts-2.0
# PURPOSE:  generate a data dictionary
# AUTHOR:   A.Chafetz | CMS
# REF ID:   61fccb95aa0b
# LICENSE:  MIT
# DATE:     2026-04-03
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(arrow, warn.conflicts = FALSE)
library(pointblank)
library(pagedown)
library(htmltools)


# GLOBAL VARIABLES --------------------------------------------------------

#data output directory
dir_out <- "Dataout"

#path to data file
(path <- list.files(dir_out, ".parquet", full.names = TRUE))


# IMPORT ------------------------------------------------------------------

#read in Fast Facts structured dataset
df_ff <- read_parquet(path)


# FUNCTIONS --------------------------------------------------------------

# Function to build the character variable summary list
build_char_dict <- function(df, sample_n = 8) {
  df %>%
    select(where(is.character)) %>%
    map(function(col) {
      levels <- col |> na.omit(col) |> unique() |> sort()
      n_distinct <- length(levels)

      if (n_distinct > sample_n) {
        options_str <- paste0(
          paste(sample(levels, sample_n), collapse = " | "),
          ", ..."
        )
      } else {
        options_str <- paste(levels, collapse = " | ")
      }

      list(
        n_distinct = n_distinct,
        options = options_str
      )
    })
}


# MUNGE -------------------------------------------------------------------

char_dict <- build_char_dict(df_ff)

# categorical_levels <- df_ff %>%
#   select(where(is.character)) %>%
#   map(~ .x %>% na.omit() %>% unique() %>% sort())

# CREATE DICTIONARY ------------------------------------------------------

informant <- create_informant(
  tbl = df_ff,
  tbl_name = "CMS Fast Facts",
  label = str_glue(
    "Data Dictionary | {format.Date(max(df_ff$release_date),'%b %Y')} Release"
  )
)

informant <- informant |>
  info_tabular(
    "Overview" = c(
      'The CMS Fast Facts includes summary information on total program enrollment, utilization, expenditures, as well as total number of Medicare providers, including physicians by specialty area.',
      '',
      'This version of the CMS Fast Fact as a "standardized dataset", unifying the data structure from the 10+ Excel files so all the data can live in the same dataset. This makes it easier for analyst to work with the data for analysis and visualization purposes. Another change here as well as is this file contains historic data from CMS Fast Facts releases since 2020, not just the most recent release.',
      '',
      'The most recent release (PDF and Excel) as well as methodology can be found on [Data.CMS.gov](https://data.cms.gov/fact-sheet/cms-fast-facts) and the open source source code on [GitHub](https://github.com/DSACMS/fast-facts-2.0)'
    )
  ) |>
  info_tabular(
    "What's New in 2025" = c(
      "
      -   Medicare Populations, CY 2024
      -   Medicaid & CHIP Populations, CY 2024
      -   Medicare Deductibles, Coinsurance, Premiums, CY 2025
      -   Original Medicare Persons Served and Payments by Type of Service, CY 2023
      -   Medicare Part D Utilization and Expenditures, CY 2023
      -   Medicaid & CHIP Payments by Type of Service, FY 2023
      -   Medicare Institutional Providers, CY 2023
      -   Medicare Non-Institutional Providers by Specialty, CY 2023
      -   Medicare Durable Medical Equipment Prosthetics, Orthotics & Supplies (DMEPOS) Providers by Specialty, CY 2023
      -   Medicare Prepaid Contracts, February 2025
      -   National Health Expenditures, CY 2023
      -   CMS Financial Data, FY 2024
      "
    )
  )


informant <- informant |>
  info_columns(
    columns = area,
    info = str_glue(
      'Various "programatic" buckets the data are sorted into.<br>Levels ({pluck(char_dict, "area", "n_distinct")}): {pluck(char_dict, "area", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = topic,
    info = str_glue(
      'Key topical segments of the reported CMS data.<br>Levels ({pluck(char_dict, "topic", "n_distinct")}): {pluck(char_dict, "topic", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = category,
    info = str_glue(
      'Each topic is disaggregated into a number of different categories <br>Levels ({pluck(char_dict, "category", "n_distinct")}): {pluck(char_dict, "category", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = sub_category,
    info = str_glue(
      'Finer disaggregations of `category` <br>Levels ({pluck(char_dict, "sub_category", "n_distinct")}): {pluck(char_dict, "sub_category", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = provider_type,
    info = str_glue(
      'For providers only, specifies their "type" <br>Levels ({pluck(char_dict, "provider_type", "n_distinct")}): {pluck(char_dict, "provider_type", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = metric,
    info = str_glue(
      'Specifies what is being measured in `value` <br>Levels ({pluck(char_dict, "metric", "n_distinct")}): {pluck(char_dict, "metric", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = period_type,
    info = str_glue(
      'Specifies whether the reporting `year` is a calendar year (CY) or fiscal year (FY) <br>Levels ({pluck(char_dict, "period_type", "n_distinct")}): {pluck(char_dict, "period_type", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  # info_snippet(
  #   snippet_name = "min_year",
  #   fn = snip_lowest(column = "year")
  # ) |>
  # info_snippet(
  #   snippet_name = "max_year",
  #   fn = snip_highest(column = "year")
  # ) |>
  info_columns(
    columns = data_year,
    info = str_glue(
      'The associated year for the reported `value`.  Due to reporting lags, the `data_year` may be one or more years prior to the CMS Fast Facts`release_date`. Each `topic` in a given release may cover one or more years. <br>Levels: {min(df_ff$data_year)}-{max(df_ff$data_year)}'
    )
  ) |>
  info_columns(
    columns = value,
    info = "The value associated with the reported `metric`. Values have been adjusted from the CMS Fast Facts Excel file into their correct units. For example, for the financial data (`topic == 'Financial'`), the data are reported in millions of USD. In the Excel file, the CHIP value is `19,457`; in this dataset, that number has been converted to the correct units, `19,457,000,000`  ((CALC))"
  ) |>
  info_columns(
    columns = bound,
    info = str_glue(
      'Relevant for only Part B Premiums (`topic == "Cost Sharing"`): specifies whether the value is the upper or lower bound of the range <br>Levels ({pluck(char_dict, "bound", "n_distinct")}): {pluck(char_dict, "bound", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = source,
    info = str_glue(
      'Identifies the CMS Fast Facts Excel file where the data was sourced from. When there were multiple releases within a given year, the lastest release from that year was used.'
    )
  ) |>
  info_columns(
    columns = source_tab,
    info = str_glue(
      'Identifies specific sheet from the CMS Fast Facts Excel file where the data was sourced from. <br>Levels ({pluck(char_dict, "source_tab", "n_distinct")}): {pluck(char_dict, "source_tab", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = source_origin,
    info = str_glue(
      'Identifies where the data originated from within CMS. <br>Levels ({pluck(char_dict, "source_origin", "n_distinct")}): {pluck(char_dict, "source_origin", "options") |> paste(collapse = " | ")}'
    )
  ) |>
  info_columns(
    columns = release_date,
    info = 'The month and year of the release (a data sorted in ISO format, `YYYY-MM-DD`), extracted from the CMS Fast Facts Excel file name.',
  ) |>
  info_columns(
    columns = is_latest,
    info = 'Boolean that specifies if the reported value is from the latest reported year as this might be different across each `metric`.',
  )
# |>
# incorporate()

informant <- informant |>
  info_section(
    section_name = "methodology",
    creation = str_glue("Dataset generated {today()}"),
    # section_name = "source",
    "Source Reference Information & Methodology (Apr 2025 Release)" = c(
      "**Populations**",
      "_Medicare Enrollment_",
      "The data reported in the Medicare Populations table come from CMS administrative enrollment data for beneficiaries enrolled in the Medicare program.  The data are sourced from the [CMS Chronic Conditions Data Warehouse (CCW)](https://www2.ccwdata.org/web/guest/home/), a database with 100% of Medicare enrollment and claims data.",
      "The Medicare enrollment counts are determined using a person-year methodology. For each calendar year, total person-year counts are determined by summing the total number of months that each beneficiary is enrolled in Parts A and/or B during the year and dividing by 12. Using this methodology, a beneficiary’s partial-year enrollment may be counted in more than 1 category (i.e., entitlement type and health care delivery), where appropriate.",
      "_Medicaid & CHIP Enrollment_",
      "The Medicaid and Children’s Health Insurance Program (CHIP) enrollment counts come from the Medicaid & CHIP Scorecard.
    -  The total and children counts are sourced from the [State Medicaid and CHIP Applications Eligibility Determinations, and Enrollment Data](https://data.medicaid.gov/dataset/6165f45b-ca93-5bb5-9d06-db29c692a360/data?conditions%5B0%5D%5Bproperty%5D=report_date&conditions%5B0%5D%5Bvalue%5D=2021-04-01&conditions%5B0%5D%5Boperator%5D=%3D&conditions%5B1%5D%5Bproperty%5D=preliminary_updated&conditions%5B1%5D%5Bvalue%5D=U&conditions%5B1%5D%5Boperator%5D=%3D).
    -  The dual-eligible enrollment count are sourced from the MMCO Statistical & Analytic Reports: [Enrollment Snapshots of dual-eligible beneficiaries](https://www.cms.gov/Medicare-Medicaid-Coordination/Medicare-and-Medicaid-Coordination/Medicare-Medicaid-Coordination-Office/Analytics).
    -  The adult expansion count are sourced from the Medicaid Budget and Expenditure System’s [Medicaid Enrollment – New Adult Group data](https://data.medicaid.gov/dataset/6c114b2c-cb83-559b-832f-4d8b06d6c1b9/data?conditions%5b0%5d%5bresource%5d=t&conditions%5b0%5d%5bproperty%5d=enrollment_month&conditions%5b0%5d%5bvalue%5d%5b0%5d=10&conditions%5b0%5d%5bvalue%5d%5b1%5d=11&conditions%5b0%5d%5bvalue%5d%5b2%5d=12&conditions%5b0%5d%5boperator%5d=in&conditions%5b1%5d%5bresource%5d=t&conditions%5b1%5d%5bproperty%5d=enrollment_year&conditions%5b1%5d%5bvalue%5d=2020&conditions%5b1%5d%5boperator%5d==).",
      "",
      "**Medicare Deductibles, Coinsurance & Premiums**",
      "The Medicare deductible, coinsurance and premiums information reported in this table are sourced from the following:
    -   [Medicare Part A and Part B Costs])(https://www.medicare.gov/basics/costs/medicare-costs)
    -   [Medicare Part D Costs](https://www.medicare.gov/drug-coverage-part-d/costs-for-medicare-drug-coverage/)",
      "",
      "**Medicare Utilization and Payments by Type of Service**",
      "The counts and amounts in the table and charts are sourced from CMS administrative enrollment and claims data for fee-for-service beneficiaries enrolled in Medicare, available from the CCW database.",
      "",
      "**Medicare Part D Utilization and Expenditures**",
      "The utilization data (Utilizing Beneficiaries and Prescription Drug Events) for this table come from CMS administrative enrollment data for beneficiaries enrolled in the Medicare Part D program, available from the CCW database. The Part D expenditures data presented in the table are sourced from the [Medicare Trustees Report](https://www.cms.gov/oact/tr).",
      "",
      "**Medicaid & CHIP Payments by Selected Type of Service**",
      "The data presented in the table and chart come from the [Medicaid & CHIP Scorecard](https://www.medicaid.gov/state-overviews/scorecard/annual-medicaid-chip-expenditures/index.html). The Scorecard is sourced from the Medicaid Budget and Expenditure System/State Children's Health Insurance Program Budget and Expenditure System [(MBES/CBES)](https://www.medicaid.gov/medicaid/financial-management/state-expenditure-reporting-for-medicaid-chip/expenditure-reports-mbescbes/index.html).",
      "",
      "**Medicare Institutional Providers**",
      "The counts reported in this table are sourced from the [Provider of Services (POS)](https://data.cms.gov/provider-characteristics/hospitals-and-other-facilities/provider-of-services-file-hospital-non-hospital-facilities). The POS data contain characteristics of institutional providers, such as hospitals and other types of healthcare facilities.",
      "",
      "**Medicare Non-Institutional Providers by Specialty**",
      "The counts reported in this table come from Medicare fee-for-service Part B claims data, available from the CCW database, and represent professionals providing services to Original Medicare beneficiaries for all Part B non-institutional services (excluding DMEPOS services).",
      "",
      "**Medicare Durable Medical Equipment, Prosthetics, Orthotics and Supplies (DMPOS) Providers by Specialty**",
      "The counts reported in this table come from Medicare fee-for-service Part B claims data, available from the CCW database, and represent DMEPOS professionals providing services to Original Medicare beneficiaries for DMEPOS services.",
      "",
      "**Medicare Prepaid Contracts**",
      "The data for this table are sourced from the latest available [Monthly Contract and Enrollment Summary Report](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/Monthly-Contract-and-Enrollment-Summary-Report).",
      "",
      "**National Health Expenditures**",
      "The data for this table are sourced from the [National Health Expenditure Accounts (NHEA)](https://www.cms.gov/research-statistics-data-and-systems/statistics-trends-and-reports/nationalhealthexpenddata/nationalhealthaccountshistorical), which are the official estimates of total health care spending in the United States.",
      "",
      "**CMS Financial Data**",
      "The data for this table are sourced from the [Budget Appendix](https://www.whitehouse.gov/omb/budget/appendix/) of the President’s Budget of the United States Government, prepared by the Office of Management and Budget."
    )
  )


report <-
  get_informant_report(
    informant,
    title = "Data Dictionary for CMS Fast Facts"
  )

report

# EXPORT -----------------------------------------------------------------

# Inject CSS to prevent header repetition
css_override <- tags$style(HTML(
  "
  @media print {
    thead { display: table-row-group; }
    tfoot { display: table-row-group; }
    .your-footer-class { position: static !important; }

    /* Prevent text from running off the page */
    body, table, td, th, p, div {
      word-wrap: break-word;
      overflow-wrap: break-word;
      word-break: break-word;
      white-space: normal !important;
    }

    /* Ensure table columns don't exceed page width */
    table {
      table-layout: fixed;
      width: 100% !important;
    }

    td, th {
      max-width: 200px;   /* adjust as needed */
      overflow: hidden;
    }
  }
  }
"
))

final_html <- tagList(css_override, report)
save_html(final_html, "Documents/data_dictionary.html")

pagedown::chrome_print(
  input = "Documents/data_dictionary.html",
  output = "Documents/CMSFastFacts_data-dictionary.pdf",
  options = list(
    scale = 1,
    paperWidth = 11, # inches (default is 8.5)
    paperHeight = 17, # inches (default is 11)
    printBackground = TRUE
  )
)


unlink("Documents/data_dictionary.html")
