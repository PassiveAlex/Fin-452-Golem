# Load secrets from .env if present (local development)
if (file.exists(".env")) readRenviron(".env")

library(shiny)
library(arrow)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(slider)
library(zoo)
library(TTR)
library(scales)
library(forcats)
library(glue)
library(purrr)
library(DT)
library(RTL)
library(shinyjs)

# ── Market metadata ──────────────────────────────────────────────────────────
# rtl_prefix: the ticker prefix used in RTL::dflong series names
# unit:        price unit for display
# bbl_factor:  multiplier to convert native unit to $/bbl equivalent
#              (42 gal/bbl for HO and RBOB; 1 for crude; MMBtu/bbl ~5.8 for NG)
MARKETS <- list(
  CL   = list(label = "WTI Cushing",    rtl_prefix = "CL",  unit = "$/bbl",   color = "#1f77b4", bbl_factor = 1,    max_contracts = 36L, enabled = TRUE, base_prefix = NULL),
  BRN  = list(label = "Brent Crude",    rtl_prefix = "BRN", unit = "$/bbl",   color = "#ff7f0e", bbl_factor = 1,    max_contracts = 36L, enabled = TRUE, base_prefix = NULL),
  HTT  = list(label = "WTI Houston",    rtl_prefix = "HTT", unit = "$/bbl",   color = "#2ca02c", bbl_factor = 1,    max_contracts = 12L, enabled = TRUE, base_prefix = "CL"),
  HO   = list(label = "Heating Oil",    rtl_prefix = "HO",  unit = "$/gal",   color = "#d62728", bbl_factor = 42,   max_contracts = 18L, enabled = TRUE, base_prefix = NULL),
  RBOB = list(label = "RBOB Gasoline",  rtl_prefix = "RB",  unit = "$/gal",   color = "#9467bd", bbl_factor = 42,   max_contracts = 18L, enabled = TRUE, base_prefix = NULL),
  NG   = list(label = "Natural Gas",    rtl_prefix = "NG",  unit = "$/mmBtu", color = "#8c564b", bbl_factor = 5.8,  max_contracts = 36L, enabled = TRUE, base_prefix = NULL)
)

ENABLED_MARKETS <- names(Filter(function(m) m$enabled, MARKETS))

# EIA area choices: two-level list keyed by commodity then series_type.
# Used by mod_eia.R (area selector) and mod_forward_curve.R (overlay region).
# Only areas with confirmed working EIA API tickers are listed here.
# Crude PADD stocks use the updated W_EPC0_SAX_R{XX} format (pending re-verification).
EIA_AREA_CHOICES <- list(
  crude = list(
    stocks = c(
      "U.S."                = "US",
      "U.S. (Excl. SPR)"    = "US_excl_spr",
      "PADD 1 (East Coast)" = "PADD1",
      "PADD 2 (Midwest)"    = "PADD2",
      "PADD 3 (Gulf Coast)" = "PADD3",
      "PADD 4 (Rockies)"    = "PADD4",
      "PADD 5 (West Coast)" = "PADD5",
      "Cushing, OK"         = "Cushing"
    ),
    production = c(
      "U.S."                = "US"
    ),
    imports = c(
      "U.S."                = "US"
    ),
    exports = c(
      "U.S."                = "US"
    )
  ),
  distillate = list(
    stocks = c(
      "U.S."                = "US",
      "PADD 1 (East Coast)" = "PADD1",
      "PADD 2 (Midwest)"    = "PADD2",
      "PADD 3 (Gulf Coast)" = "PADD3",
      "PADD 4 (Rockies)"    = "PADD4",
      "PADD 5 (West Coast)" = "PADD5"
    )
  ),
  gasoline = list(
    stocks = c(
      "U.S."                = "US",
      "PADD 1 (East Coast)" = "PADD1",
      "PADD 2 (Midwest)"    = "PADD2",
      "PADD 3 (Gulf Coast)" = "PADD3",
      "PADD 4 (Rockies)"    = "PADD4",
      "PADD 5 (West Coast)" = "PADD5"
    )
  ),
  natural_gas = list(
    storage = c(
      "U.S. Lower 48"   = "US",
      "East Region"     = "East",
      "Midwest Region"  = "Midwest",
      "Mountain Region" = "Mountain",
      "Pacific Region"  = "Pacific",
      "South Central"   = "SouthCentral"
    ),
    production = c(
      "U.S."            = "US"
    ),
    consumption = c(
      "U.S. Industrial" = "Industrial",
      "U.S. Residential" = "Residential"
    )
  )
)

# Available series types per commodity (drives the Series Type selector in mod_eia).
# Only types with at least one confirmed working ticker are listed.
EIA_SERIES_TYPE_CHOICES <- list(
  crude       = c("Stocks"      = "stocks",  "Production" = "production",
                  "Imports"     = "imports", "Exports"    = "exports"),
  distillate  = c("Stocks"      = "stocks"),
  gasoline    = c("Stocks"      = "stocks"),
  natural_gas = c("Storage"     = "storage", "Production" = "production",
                  "Consumption" = "consumption")
)

# Mapping from futures market key to EIA commodity label used in eia_fundamentals.feather
MARKET_TO_EIA <- c(
  CL   = "crude",
  BRN  = "crude",
  HTT  = "crude",
  HO   = "distillate",
  RBOB = "gasoline",
  NG   = "natural_gas"
)

# Curve shape classification thresholds ($/bbl equivalent)
CURVE_THRESHOLDS <- c(steep_back = 3, mild_back = 0.5, mild_cont = -0.5, steep_cont = -3)

# Default rolling window (trading days)
DEFAULT_WINDOW <- 63L

# Vol annualisation factor
TRADING_DAYS <- 252L

# App theme
APP_THEME <- bslib::bs_theme(
  bootswatch  = "flatly",
  primary     = "#1f77b4",
  base_font   = bslib::font_google("Inter"),
  code_font   = bslib::font_google("Fira Code"),
  `enable-shadows` = TRUE
)
