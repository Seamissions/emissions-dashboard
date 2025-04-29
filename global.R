# --------------------------------------------------------------------------------
# ---- Global --------------------------------------------------------------------
# --------------------------------------------------------------------------------


# ---- Source config file storing tokens ----
source("config.R")

# ---- Load libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(bs4Dash)
library(bslib)
library(later) # to delay loader
library(gganimate)
library(plotly)
library(tidyverse)
library(shinycssloaders)
library(mapdeck)
library(markdown)
library(geojsonsf)
library(here)
library(sf)
library(viridis)
library(RColorBrewer) # for colors
library(bslib) # for theme colors
library(rsconnect)
library(pryr)

# ---- Load & prep data ---------------------------------------------------------------

# ---- Emissions map data ----

# Load emissions data
all_emissions <- readRDS("data/all_emissions.rds") |> 
  filter(emissions_co2_mt >= 200)

country_emissions <- readRDS("data/country_emissions.rds") |> 
  filter(emissions_co2_mt >= 200)

# Load background data
fao_regions <- st_read(here::here("data/fao_region_shapefile","World_Fao_Zones.shp")) |>
  st_transform(4326)

fao_borders <- st_cast(fao_regions, "MULTILINESTRING")

# Color palette for FAO zones
m <- grDevices::colorRamp(c("#DA8D03"))( (1:256)/256 )

# Precompute values used in UI
country_flags <- sort(unique(country_emissions$flag))
year_min <- min(country_emissions$year, na.rm = TRUE)
year_max <- max(country_emissions$year, na.rm = TRUE)

# Pre-calculate total emissions for the initial year (max year)
initial_total_broadcasting <- all_emissions %>%
  filter(year == max(all_emissions$year, na.rm = TRUE)) %>%
  summarise(total = sum(emissions_co2_mt, na.rm = TRUE)) %>%
  pull(total)

# ---- Seafood explorer data ----

# Create the fake data
df <- data.frame(
  country = c("China", "USA", "Japan", "Iceland", "Argentina", "Australia"),
  emissions = c(500, 175, 150, 105, 75, 73)
)
