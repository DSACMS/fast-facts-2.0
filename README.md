# Fast Facts 2.0

Reimagining CMS Fast Facts communication of summary information

## About the Project

CMS makes a number of different data sources available to the public on [data.cms.gov](data.cms.gov). One of those sources is the annual [CMS Fast Facts](https://data.cms.gov/fact-sheet/cms-fast-facts) which is a "quick reference statistical summary on annual CMS program and financial data." This annual report contains 13 pages worth of largely tabular data. This project aims to condense the data views while making the infomation more digestible and actionable.

### Project Vision

The goal of this projects goal is to improve the communication of these important data components, making it easier for stakeholders and the public to digest and interpret the information.

## Core Team

A list of core team members responsible for the code and documentation in this repository can be found in [COMMUNITY.md](COMMUNITY.md).

## Local Development

### Dependencies

This product relies on using R and Quarto to extract, munging, and export the visualizations. In addition to having these tools, a variety of R libraries were used in the process. This product leverages a through git/GitHub, but also through creating a reproducible envionment using the [`renv` package](https://rstudio.github.io/renv/index.html), "[making] it easier to share ... code in such a way that everyone gets exactly the same package versions". Loading this R project will allow "renv will automatically bootstrap itself, downloading and installing the appropriate version of renv" and "ask them if they want to download and install all the packages it needs by running `renv::restore()`." As more packages are added, run "`renv::snapshot()` to record the latest package versions in your lockfile." 

- R >= 4.0
- Quarto >= 1.3
- various R packages (updated through `renv`)

### Rendering

The script to render the final output can be found in the `Scripts/` folder. 

- `01_setup_working_dataset.R` - Reads in each of the Excel tabs from all Fast Fast files in the `Data` folder to produce structured dataset to work from
- `02_data_dictionary.R` - Renders a data dictionary that updates with any inputs to the setup script.
- `03_data_processing.R` - Munges the data in a way that is needed for the creating visuals

Additional utilities can be found in the scripts with prefixes that start with 90.

- `98_color_system.R` - unified color system, base colors, scales, and plot elements
- `99_functions.R` - utility functions used throughout processing

With the scripts run, you can render the Quarto dashbaord file sitting in the base folder of the directory. Run `quarto render` to build.

## Repository Structure

Below is the folder structure for this repository, run with `r fs::dir_tree(recurse = 0)`.

```
.
├── CODEOWNERS.md
├── CODE_OF_CONDUCT.md
├── COMMUNITY.md
├── CONTRIBUTING.md
├── Data
│   ├── CMSFastFacts2026.xlsx
│   ├── Historic
├── Dataout
│   ├── CMSFastFacts_SD_2026-04.csv
│   ├── CMSFastFacts_SD_2026-04.parquet
│   ├── beneficiaries.rds
│   ├── context.rds
│   ├── cost_sharing.rds
│   └── providers.rds
├── Documents
│   ├── CMS Fast Facts_2026.pdf
│   └── CMSFastFacts_data-dictionary.pdf
├── LICENSE
├── README.md
├── SECURITY.md
├── Scripts
│   ├── 01_setup_working_dataset.R
│   ├── 02_data_dictionary.R
│   ├── 03_data_processing.R
│   ├── 98_color_system.R
│   └── 99_functions.R
├── assets
│   ├── CMS_logo_reverse.png
│   └── ff_format.scss
├── fast-facts.qmd
├── renv
│   ├── activate.R
│   ├── library
│   └── settings.json
├── renv.lock
└── repolinter.json
```

<!--
## Repository Structure

TODO: Including the repository structure helps viewers quickly understand the project layout. Using the "tree -d" command can be a helpful way to generate this information, but, be sure to update it as the project evolves and changes over time.

To install the tree command:
In the command line
- MacOS: 
```
brew install tree
```

- Linux: 
```
sudo apt-get update
sudo apt-get install tree
```

Windows:
```
choco install tree
```

**{list directories and descriptions}**

TODO: Add a 'table of contents" for your documentation. Tier 0/1 projects with simple README.md files without many sections may or may not need this, but it is still extremely helpful to provide "bookmark" or "anchor" links to specific sections of your file to be referenced in tickets, docs, or other communication channels.

**{list of .md at top directory and descriptions}**

-->

## Development and Software Delivery Lifecycle
This section provides an overview of how this project typically manages code changes and delivers software updates. It is intended to help contributors understand the general flow of work, not to set mandatory procedures. Programs and teams may adjust these practices to meet their own requirements, governance structures, or release schedules.

Project team members with write access work directly in this repository. External contributors follow the same general workflow but submit changes through a fork and cannot merge their own pull requests. Additional guidance for contributing is available in:
[CONTRIBUTING.md](./CONTRIBUTING.md).

This project aligns with the organization’s common approach to versioning, preparing releases, and communicating updates. Rather than restating those details here, please refer to the OSPO Release Guidelines:

[Release Guidelines (OSPO Guide)](https://dsacms.github.io/ospo-guide/outbound/release-guidelines/)

These guidelines outline agency-wide expectations for semantic versioning, release candidates, GitHub releases, and associated review and communication practices. Individual projects may follow this model in full or tailor it to their operational needs.

## Local Development

<!--- TODO - with example below:
This project is a monorepo with several apps. Please see the [api](./api/README.md) and [frontend](./frontend/README.md) READMEs for information on spinning up those projects locally. Also see the project [documentation](./documentation) for more info.
-->

## Coding Style and Linters

This project follows the [tidyverse style guide](https://style.tidyverse.org/) for R code. Style and lint checks are enforced via [`lintr`](https://lintr.r-lib.org/) and run automatically on each push or pull request to `main` via GitHub Actions.

Lint checks are configured in `.lintr` at the project root. Findings are reported as warnings and will not block merges, but contributors are expected to resolve lint issues before submitting a pull request.

To run linting locally before committing:

```r
# Lint a single file
lintr::lint("Scripts/my_script.R")

# Lint the full project
lintr::lint_dir()
```

Note: Some files (e.g. `Scripts/02_data_dictionary.R`) include inline `# nolint` annotations where long lines are intentional and unavoidable.

<!--
## Branching Model

TODO - with example below:
This project follows [trunk-based development](https://trunkbaseddevelopment.com/), which means:

* Make small changes in [short-lived feature branches](https://trunkbaseddevelopment.com/short-lived-feature-branches/) and merge to `main` frequently.
* Be open to submitting multiple small pull requests for a single ticket (i.e. reference the same ticket across multiple pull requests).
* Treat each change you merge to `main` as immediately deployable to production. Do not merge changes that depend on subsequent changes you plan to make, even if you plan to make those changes shortly.
* Ticket any unfinished or partially finished work.
* Tests should be written for changes introduced, and adhere to the text percentage threshold determined by the project.

This project uses **continuous deployment** using [Github Actions](https://github.com/features/actions) which is configured in the [./github/workflows](.github/workflows) directory.

Pull-requests are merged to `main` and the changes are immediately deployed to the development environment. Releases are created to push changes to production.
-->

## Contributing

Thank you for considering contributing to an Open Source project of the US Government! For more information about our contribution guidelines, see [CONTRIBUTING.md](CONTRIBUTING.md).


## Codeowners

The contents of this repository are managed by the CMS Open Source Program Office. Those responsible for the code and documentation in this repository can be found in [CODEOWNERS.md](CODEOWNERS.md)

## Community

The Fast Facts 2.0 team is taking a community-first and open source approach to the product development of this tool. We believe government software should be made in the open and be built and licensed such that anyone can download the code, run it themselves without paying money to third parties or using proprietary software, and use it as they will.

We know that we can learn from a wide variety of communities, including those who will use or will be impacted by the tool, who are experts in technology, or who have experience with similar technologies deployed in other spaces. We are dedicated to creating forums for continuous conversation and feedback to help shape the design and development of the tool.

We also recognize capacity building as a key part of involving a diverse open source community. We are doing our best to use accessible language, provide technical and process documents, and offer support to community members with a wide variety of backgrounds and skillsets.

### Community Guidelines

Principles and guidelines for participating in our open source community are can be found in [COMMUNITY.md](COMMUNITY.md). Please read them before joining or starting a conversation in this repo or one of the channels listed below. All community members and participants are expected to adhere to the community guidelines and code of conduct when participating in community spaces including: code repositories, communication channels and venues, and events.

<!--
## Governance
Information about how the **{project_name}** community is governed may be found in [GOVERNANCE.md](GOVERNANCE.md).

<!--
## Feedback
If you have ideas for how we can improve or add to our capacity building efforts and methods for welcoming people into our community, please let us know at **{contact email}**. If you would like to comment on the tool itself, please let us know by filing an **issue on our GitHub repository.**

## Glossary
Information about terminology and acronyms used in this documentation may be found in [GLOSSARY.md](GLOSSARY.md).
-->

## Policies

### Open Source Policy

We adhere to the [CMS Open Source
Policy](https://github.com/CMSGov/cms-open-source-policy). If you have any
questions, just [shoot us an email](mailto:opensource@cms.hhs.gov).

### Security and Responsible Disclosure Policy

_Submit a vulnerability:_ Vulnerability reports can be submitted through [Bugcrowd](https://bugcrowd.com/cms-vdp). Reports may be submitted anonymously. If you share contact information, we will acknowledge receipt of your report within 3 business days.

For more information about our Security, Vulnerability, and Responsible Disclosure Policies, see [SECURITY.md](SECURITY.md).

### Software Bill of Materials (SBOM)

A Software Bill of Materials (SBOM) is a formal record containing the details and supply chain relationships of various components used in building software.

In the spirit of [Executive Order 14028 - Improving the Nation’s Cyber Security](https://www.gsa.gov/technology/it-contract-vehicles-and-purchasing-programs/information-technology-category/it-security/executive-order-14028), a SBOM for this repository is provided here: https://github.com/DSACMS/fast-facts-2.0/network/dependencies.

For more information and resources about SBOMs, visit: https://www.cisa.gov/sbom.

## Public domain

This project is in the public domain within the United States, and copyright and related rights in the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/) as indicated in [LICENSE](LICENSE).

All contributions to this project will be released under the CC0 dedication. By submitting a pull request or issue, you are agreeing to comply with this waiver of copyright interest.
 
 ## Codeowners 
 The contents of this repository are managed by {responsible organization(s)}. Those responsible for the code and documentation in this repository can be found in [COMMUNITY.md](COMMUNITY.md). 
