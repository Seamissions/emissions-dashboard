# --------------------------------------------------------------------------------
# ---- Global --------------------------------------------------------------------
# --------------------------------------------------------------------------------


# ---- Source config file storing tokens ----
source("R/config.R")
source("R/infoPopup.R")
sf::sf_use_s2(FALSE)


# ---- Load libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyBS) 
library(shinycssloaders)
library(bs4Dash)
library(later) # to delay loader
library(gganimate)
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
library(ggimage)

# ---- Load & prep data ---------------------------------------------------------------

# ---- Emissions map data ----

# Load emissions data
broadcasting_emissions <- readRDS("data/broadcasting_emissions.rds") |> 
  filter(emissions_co2_mt >= 200) |>
  mutate(tooltip_text = paste0(scales::comma(round(emissions_co2_mt, 0)), " MT CO₂"))

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
top_flags <- readRDS("data/top_flags.rds") |>
  group_by(year) |>
  arrange(desc(sum_emissions), .by_group = TRUE) |>
  slice_head(n = 10) |>
  ungroup()

# --- Read in species data ----
species_data <- readRDS("data/species_data.rds")

# --- Prep top isscaap data ----
top_isscaap <- species_data |>
  group_by(isscaap_group, year, image) |>
  summarize(
    sum_emissions = sum(sum_emissions, na.rm = TRUE),
    total_catch = sum(total_catch, na.rm = TRUE),
    emissions_per_ton = sum_emissions / total_catch,
    .groups = "drop"
  ) |>
  group_by(year) |>
  arrange(desc(sum_emissions), .by_group = TRUE) |>
  slice_head(n = 10) |>
  ungroup()


# ---- Define color palettes -------------------------------------------------
blue_palette <- colorRamp(c("#20404F", "#4C9EA6", "#67D6E0", "#76F3FF", "#A9F2FF", "#DAF3FF", "#F6F8FF"))((1:256) / 256)
orange_palette <- colorRamp(c("#7A5100", "#B97700", "#FFB300", "#FFD54F", "#FFEB99", "#FFF5CC", "#FFFEF2"))((1:256) / 256)

