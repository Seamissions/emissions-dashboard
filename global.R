# =============================================================================
# Name:           global.R
# Description:    Global setup file for the Seamissions Explorer Shiny app. 
#                 Loads essential libraries, initializes shared resources, 
#                 and prepares spatial and emissions datasets used throughout 
#                 the app. Centralizes code that does not need to be re-run 
#                 for each user session to improve performance and maintainability.
# 
# Inputs:         config.R                #  App-wide configuration settings
#                 infoPopup.R             #  Reusable UI component for info tooltips
#                 World_Fao_Zones.shp     #  FAO Major Fishing Region shapefile
#                 seafood_emissions_data.rds        #  Emissions by species/country/year
#                 top_flags.rds           #  Pre-filtered top emitting flags data
#
# Outputs:        Load logic and variables from a global scope among concurrent app users
# 
# Notes:          Reduce redundant code and increase the app's speed by re-using
#                 logic and variables from a global scope among concurrent app users.
#                 This file should only contain objects and expressions that do not 
#                 rely on user input and are safe to share across sessions.
# 
# =============================================================================


# Set up environment ----------------------------------------------------------

# ---- Source config file storing tokens ----
source("R/config.R")
source("R/infoPopup.R") 

# ---- Load libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyBS) 
library(shinycssloaders)
library(bs4Dash)
library(later)
library(tidyverse)
library(mapdeck)
library(markdown)
library(here)
library(sf)
library(RColorBrewer) # for colors
library(bslib) # for theme colors
library(scales)  # for commas in labels
library(rsconnect)
library(ggflags)
library(ggimage)
library(magick)

# ---- Define color palettes -------------------------------------------------
blue_palette <- colorRamp(c("#20404F", "#4C9EA6", "#67D6E0", "#76F3FF", "#A9F2FF", "#DAF3FF", "#F6F8FF"))((1:256) / 256)
orange_palette <- colorRamp(c("#7A5100", "#B97700", "#FFB300", "#FFD54F", "#FFEB99", "#FFF5CC", "#FFFEF2"))((1:256) / 256)


# Import & prep data -----------------------------------------------------------------

# Disable S2 geometry to avoid self-intersection errors
sf::sf_use_s2(FALSE)

# ---- Emissions map data ----

# Load emissions data
broadcasting_emissions <- readRDS("data/broadcasting_emissions.rds") |> 
  filter(emissions_co2_mt >= 200) |>
  mutate(tooltip_text = paste0(scales::comma(round(emissions_co2_mt, 0)), " MT CO₂"))


# Load emissions data
broadcasting_emissions_summary <- readRDS("data/broadcasting_emissions_summary.rds") 
nb_emissions_summary <- readRDS("data/nb_emissions_summary.rds") 

# Precompute values used in UI
year_min <- min(broadcasting_emissions$year, na.rm = TRUE)
year_max <- max(broadcasting_emissions$year, na.rm = TRUE)


# ---- Prep FAO data ----
fao_regions <- st_read("data/fao_region_shapefile/World_Fao_Zones.shp") |>
  st_transform(4326) |>
  st_make_valid()

fao_borders <- st_cast(fao_regions, "MULTILINESTRING") |>
  st_make_valid()

# Color palette for FAO zones
fao_zone_color <- grDevices::colorRamp(c("#CEECEB"))( (1:256)/256 )


# ---- Seafood explorer data ----

# ---- Read in top flags (countries) ----
top_flags <- readRDS("data/top_flags.rds") |>
  group_by(year) |>
  arrange(desc(sum_emissions), .by_group = TRUE) |>
  slice_head(n = 10) |>
  ungroup()

# --- Read in species data ----
seafood_emissions_data <- readRDS("data/seafood_emissions_data.rds")

# --- Read in top isscaap ----
top_isscaap <-readRDS("data/top_isscaap.rds")


