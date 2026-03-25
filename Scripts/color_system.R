# PROJECT:  fast-facts-2.0
# PURPOSE:  color system
# AUTHOR:   A.Chafetz | CMS
# REF ID:
# LICENSE:  MIT
# DATE:     2026-03-25
# UPDATED:

# ── Core Program Colors ──────────────────────────────────────────────────---
ff_cobolt <- "#0071BC"
ff_teal <- "#00A99D"
ff_plum <- "#7B4F9E"

# ── Extended Categorical Colors ──────────────────────────────────────────────
ff_saffron <- "#F7A11A"
ff_coral <- "#E05C3A"
ff_green <- "#4A8C6F"
ff_steel <- "#4A90C4"
ff_gold <- "#F2C94C"
ff_charcoal <- "#3D3D3D"
ff_warmgray <- "#9B9B9B"

# ── Tint & Shade Scales ───────────────────────────────────────────────────────

teal_scale <- c(
  "100" = "#B3E8E5",
  "200" = "#59CEC8",
  "500" = "#00A99D",
  "700" = "#007F75",
  "900" = "#00524C"
)

cobolt_scale <- c(
  "100" = "#B3D6F0",
  "200" = "#59A8DC",
  "500" = "#0071BC",
  "700" = "#00538C",
  "900" = "#003459"
)

plum_scale <- c(
  "100" = "#DDD0EC",
  "200" = "#AD8EC8",
  "500" = "#7B4F9E",
  "700" = "#5C3A77",
  "900" = "#3A244C"
)

saffron_scale <- c(
  "100" = "#FDE8B8",
  "200" = "#FAC464",
  "500" = "#F7A11A",
  "700" = "#C07A0D",
  "900" = "#7A4D08"
)

coral_scale <- c(
  "100" = "#F9D0C6",
  "200" = "#ED9A85",
  "500" = "#E05C3A",
  "700" = "#AA3F22",
  "900" = "#6E2613"
)

green_scale <- c(
  "100" = "#C8DFD5",
  "200" = "#87BAA5",
  "500" = "#4A8C6F",
  "700" = "#336652",
  "900" = "#1E3D31"
)

steel_scale <- c(
  "100" = "#C8DFF1",
  "200" = "#87BBE0",
  "500" = "#4A90C4",
  "700" = "#316A96",
  "900" = "#1C3F5C"
)

gold_scale <- c(
  "100" = "#FCF0C3",
  "200" = "#F7DC87",
  "500" = "#F2C94C",
  "700" = "#C49A1E",
  "900" = "#7A5E0B"
)

charcoal_scale <- c(
  "100" = "#C8C8C8",
  "200" = "#858585",
  "500" = "#3D3D3D",
  "700" = "#252525",
  "900" = "#111111"
)

warmgray_scale <- c(
  "100" = "#E8E8E8",
  "200" = "#C2C2C2",
  "500" = "#9B9B9B",
  "700" = "#6E6E6E",
  "900" = "#3F3F3F"
)

# ── Master CMS Color System ───────────────────────────────────────────────────

ff_colors <- list(
  # Base colors (named vector for quick categorical use)
  base = c(
    azure = "#0071BC",
    teal = "#00A99D",
    plum = "#7B4F9E",
    saffron = "#F7A11A",
    coral = "#E05C3A",
    green = "#4A8C6F",
    steel = "#4A90C4",
    gold = "#F2C94C",
    charcoal = "#3D3D3D",
    warmgray = "#9B9B9B"
  ),

  #chart elements
  elements = c(
    title = cobolt_scale[["900"]],
    subtitle = warmgray_scale[["700"]],
    axis = "#1A1A1A",
    annotation = "#1A1A1A"
  ),

  # Full scales (list of named vectors)
  scales = list(
    cobolt = cobolt_scale,
    teal = teal_scale,
    plum = plum_scale,
    saffron = saffron_scale,
    coral = coral_scale,
    green = green_scale,
    steel = steel_scale,
    gold = gold_scale,
    charcoal = charcoal_scale,
    warmgray = warmgray_scale
  )
)

rm(list = ls(pattern = "_scale$"))
