# --------------------------------------------------------------------------------
# ---- Global --------------------------------------------------------------------
# --------------------------------------------------------------------------------


# ---- Source config file storing tokens ----
source("config.R")
sf::sf_use_s2(FALSE)


# ---- Load libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(bs4Dash)
library(later) # to delay loader
library(gganimate)
library(plotly)
library(tidyverse)
library(mapdeck)
library(markdown)
library(geojsonsf)
library(here)
library(sf)
library(viridis)
library(RColorBrewer) # for colors
library(bslib) # for theme colors
library(scales)  # for commas in labels
library(rsconnect)
library(pryr)
library(ggflags)
#library(ggimage)

# ---- Load & prep data ---------------------------------------------------------------

# ---- Emissions map data ----

# Load emissions data
broadcasting_emissions <- readRDS("data/broadcasting_emissions.rds") |> 
  filter(emissions_co2_mt >= 200)

# Load FAO data
fao_regions <- st_read(here::here("data/fao_region_shapefile","World_Fao_Zones.shp")) |>
  st_transform(4326) |>
  st_make_valid()

fao_borders <- st_cast(fao_regions, "MULTILINESTRING") |>
  st_make_valid()

# Color palette for FAO zones
fao_zone_color <- grDevices::colorRamp(c("#CEECEB"))( (1:256)/256 )

# Precompute values used in UI
year_min <- min(broadcasting_emissions$year, na.rm = TRUE)
year_max <- max(broadcasting_emissions$year, na.rm = TRUE)

# ---- Seafood explorer data ----
top_flags <- readRDS("data/top_flags.rds") |>
  filter(year == 2016) |>
  head(10)

# --- Read in species data ----
species_data <- readRDS("data/species_data.rds") |>
  filter(year == 2016)

# --- Prep top isscaap data ----
top_isscaap <- species_data |>
  filter(year == 2016) |>
  group_by(isscaap_group, year, image) |>
  summarize(
    sum_emissions = sum(sum_emissions, na.rm = TRUE),
    total_catch = sum(total_catch, na.rm = TRUE),
    emissions_per_ton = sum_emissions / total_catch,
    .groups = "drop"
  ) |>
  slice_max(order_by = sum_emissions, n = 10, with_ties = FALSE)
