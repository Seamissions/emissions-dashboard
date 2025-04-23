# ---- Global ------------------------------------------------------------------

# ---- Source config file storing tokens ----
source("config.R")

# ---- Load libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(later) # to delay loader

library(tidyverse)
library(shinycssloaders)
library(mapdeck)
library(markdown)
library(geojsonsf)
library(here)
library(sf)
library(viridis)
library(RColorBrewer) # for colors
library(rsconnect)
library(pryr)



# ---- Load data ---------------------------------------------------------------
#MEDS/capstone/emissions-dashboard/
# --- Load emissions data ----
nb_emissions <- readRDS("data/nb_emissions.rds")  |> 
  filter(emissions_co2_mt >= 200,
         year == 2016)

country_emissions <-  readRDS("data/country_emissions.rds") |> 
  filter(emissions_co2_mt >= 200)

all_emissions <- readRDS("data/all_emissions.rds") |> 
  filter(emissions_co2_mt >= 200)

# --- Load background data ----
fao_regions <- st_read(here::here("data/fao_region_shapefile","World_Fao_Zones.shp")) |>
  st_transform(4326)

fao_borders <- st_cast(fao_regions, "MULTILINESTRING")

# --- Color palette for FAO zones ----
m <- grDevices::colorRamp(c("yellow"))( (1:256)/256 )

# ---- Precompute values used in UI ----
country_flags <- sort(unique(country_emissions$flag))
year_min <- min(country_emissions$year, na.rm = TRUE)
year_max <- max(country_emissions$year, na.rm = TRUE)
