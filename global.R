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




# --- Load emissions data ----
nb_emissions <- st_read(here::here("data","nb_emissions.geojson"))  |> filter(emissions_co2_mt >= 200)
country_emissions <- st_read(here::here("data","country_emissions.geojson")) |> filter(emissions_co2_mt >= 200)
all_emissions <- st_read(here::here("data","all_emissions.geojson"))  |> filter(emissions_co2_mt >= 200)


# Load background data ----
fao_regions <- st_read(here::here("data","fao_region_shapefile","World_Fao_Zones.shp")) |>
  st_transform(4326) 

fao_borders <- st_cast(fao_regions, "MULTILINESTRING")

m <- grDevices::colorRamp(c("yellow"))( (1:256)/256 )

sea_palette <- colorRamp(c("#3C7B85", "#76F3FF"))( (1:256)/256) 


