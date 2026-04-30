# PROJECT:  fast-facts-2.0
# PURPOSE:  Run through scipts to render the full product
# AUTHOR:   A.Chafetz | CMS
# REF ID:   9c1b01c03846
# LICENSE:  MIT
# DATE:     2026-04-30
# UPDATED:

# RUN THROUGH SCRIPTS ----------------------------------------------------

#run scripts as needed
source("Scripts/01_setup_working_dataset.R")
source("Scripts/02_data_dictionary.R")
source("Scripts/03_data_processing.R")


# RENDER QUARTO ----------------------------------------------------------

#render html
quarto::quarto_render("fast-facts.qmd")

#export to pdf
pagedown::chrome_print(
  "fast-facts.html",
  "Documents/CMSFastFacts_2026.pdf"
)

# library(quarto)
# library(pagedown)

# render_cms_pdf <- function(qmd_file   = "fast-facts.qmd",
#                            output_pdf = "Documents/CMS_FastFacts_April2026.pdf") {

#   # 1. Render dashboard to HTML
#   quarto::quarto_render(qmd_file)
#   html_file <- sub("\\.qmd$", ".html", qmd_file)

#   # 2. Inject JS to force-show all dashboard pages before Chrome prints
#   #    Based on your HTML: inactive pages lack 'show' and 'active' classes
#   js_inject <- '
#   <script>
#     window.onbeforeprint = function() {

#       // Force all dashboard-page tab-panes to be visible
#       document.querySelectorAll(".dashboard-page.tab-pane").forEach(function(el) {
#         el.classList.add("show", "active");
#         el.style.display    = "block";
#         el.style.visibility = "visible";
#         el.style.opacity    = "1";
#         el.style.height     = "100vh";
#         el.style.position   = "relative";
#       });

#       // The outer content container must also flow as a block
#       var content = document.querySelector(".quarto-dashboard-content");
#       if (content) {
#         content.style.display  = "block";
#         content.style.height   = "auto";
#         content.style.overflow = "visible";
#       }

#       // Hide the navbar — not useful in a PDF
#       var header = document.getElementById("quarto-dashboard-header");
#       if (header) header.style.display = "none";
#     };
#   </script>'

#   html <- readLines(html_file, warn = FALSE)
#   html <- sub("</body>", paste0(js_inject, "\n</body>"), html)
#   writeLines(html, html_file)

#   # 3. Print to PDF
#   pagedown::chrome_print(
#     input  = html_file,
#     output = output_pdf,
#     options = list(
#       paperWidth      = 11,    # landscape letter — best for dashboard layouts
#       paperHeight     = 8.5,
#       marginTop       = 0.15,
#       marginBottom    = 0.15,
#       marginLeft      = 0.15,
#       marginRight     = 0.15,
#       printBackground = TRUE   # preserves your cosmo/scss colours and logo
#     )
#   )

#   message("PDF saved to: ", output_pdf)
# }

# render_cms_pdf()
