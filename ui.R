# =============================================================================
# Name:           ui.R
# Description:    User interface definition for the Seamissions Explorer Shiny app.
#                 Constructs the layout, panels, and visual elements for navigation,
#                 map exploration, data visualization, and interpretive content. 
#                 Integrates custom styling, component inputs, and responsive design
#                 to guide user interaction across desktop and mobile devices.
# 
# Inputs:         theme.R                #  Custom color palette and visual theme
#                 global.R               #  Preloaded data and logic available app-wide
#                 infoPopup.R            #  Reusable UI component for info tooltips
#                 www/ (images, CSS)     #  Supporting assets for icons, logos, and styling
# 
# Outputs:        Renders the full UI for all app tabs, including maps, plots,
#                 tooltips, filters, and info sections
# 
# Notes:          This file defines the layout structure and all visible elements
#                 seen by users. It uses Shiny modules, `navbarPage`, `fluidRow`,
#                 `absolutePanel`, and custom JavaScript/CSS for interactive behavior.
# =============================================================================

# Load custom visual theme (e.g., colors, fonts, spacing) defined in theme.R
source("theme.R")

ui <- 

# =================================================================================================================================================================================================
# Define general app settings
# =================================================================================================================================================================================================

# ---- Set up external assets ----

# Link to Google Fonts - Roboto (weights 400 & 500) 
tagList(
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap"), # END tags
    
    # Link to Mapbox GL CSS - required for rendering mapdeck maps
  tags$link(
    rel = "stylesheet",
    href = "https://api.tiles.mapbox.com/mapbox-gl-js/v2.13.0/mapbox-gl.css"), # END tags
  
  # Set the favicon to custom (icon shown in browser tab)
  tags$link(
    rel = "icon",
    type = "image/png",
    href = "images/logos/fav-icon.png") # END tags
) # END tagList


# --- Define app title and high level settings ----
navbarPage(
  title = "Seamissions Explorer",
  header = NULL, 
  windowTitle = "Seamissions Explorer",
  id = "navbarPage",
  theme = seamissions_theme,

# Initiate shiny javascript
useShinyjs(),

# =================================================================================================================================================================================================
# Home Tab
# =================================================================================================================================================================================================
  
# ---- Home Panel ----
shiny::tabPanel("Home",
                  
                # ---- Hero banner with ocean image ----
                  div(style = "position: relative;
                              min-height: 100vh;
                              width: 100%;
                              background-image: url('images/ocean-banner.png');
                              background-size: cover;
                              background-position: center;
                              background-attachment: fixed;
                              text-align: center;
                              color: #e8fffd;
                              display: flex;
                              flex-direction: column;
                              justify-content: flex-start;
                              align-items: center;
                              padding: 40px 20px 60px 20px;", 
                      
                      # Transparent overlay on top of image
                      div(style = "position: absolute; 
                                   top: 0; left: 0; right: 0; bottom: 0;
                                   background-color: rgba(0, 0, 0, 0.5);
                                   z-index: 1;"), # END overlay div
                      
                      # Add seamissions logo
                      tags$img(src = "images/logos/seamissions-logo.png",
                               style = "position: relative;
                                        height: 150px;
                                        z-index: 2;
                                        margin-bottom: 20px;"), # END tags$img for logo
                      
                      # Text in hero banner
                      div(style = "position: relative; z-index: 2; max-width: 900px;",
                          
                          h1("Explore Global Seafood Emissions",
                             style = "font-weight: 600 !important;"), # END h1
                          
                          tags$hr(style = "border-color: white;"), # end tags$hr for style
                          
                          tags$p("Understanding carbon emissions from fishing vessels is essential to understanding the full environmental impact of wild-caught seafood — but tracking vessel activity at sea has long been a challenge... until now.",
                                 style = "font-weight: normal; color: white; margin-top: 30px;"), # END tags$p 
                          
                          tags$p("This dashboard links CO₂ emissions estimates from Global Fishing Watch’s novel vessel emissions dataset with seafood catch data from the UN Food and Agriculture Organization (FAO). Users can track where fishing vessel emissions occur and compare how emissions for seafood production vary. Improving the carbon efficiency of fisheries can also lead to better-managed fish stocks, healthier oceans, and cleaner air.",
                                 style = "font-weight: normal; color: white; margin-top: 20px;")), # END tags$p
                      
                      # Teaser Cards Inside Hero Section
                      div(style = "position: relative; z-index: 2; margin-top: 50px; display: flex; flex-wrap: wrap; justify-content: center; gap: 20px; padding: 0 20px;",
                          
                          # ---- Emissions Map Card ----
                          
                          div(id = "explore_map_card",
                              style = "flex: 1 1 300px; max-width: 350px; cursor: pointer; position: relative;
                                       padding-top: 50px; padding-bottom: 10px; background-color: white;
                                       border-radius: 8px; box-shadow: 0 4px 10px rgba(0,0,0,0.1);
                                       overflow: visible; font-family: sans-serif; text-align: center;",
                              
                              div(style = "height: 12px; background-color: #08C4E5;
                                          border-top-left-radius: 8px; border-top-right-radius: 8px;
                                          position: absolute; top: 0; left: 0; right: 0;"), # END card background
                              
                              div(style = "position: absolute; top: -28px; left: 50%; transform: translateX(-50%);
                                            background-color: #08C4E5; width: 56px; height: 56px;
                                            border-radius: 50%; display: flex; align-items: center;
                                            justify-content: center; z-index: 10;",
                                  
                                  tags$i(class = "fas fa-earth-americas",
                                         style = "color: white; font-size: 24px;")), # END div for icon circle
                              
                              h4("Fishing Vessel Emissions Map",
                                 style = "color: #0B2232; font-weight: 600;"), # END h4
                              
                              p("Explore a global map of CO₂ emissions from large-scale fishing vessels, powered by a novel dataset from Global Fishing Watch and emLab.",
                                style = "color: #444; padding: 0 20px;") # END p
                          ), # END div for explore_map_card
                          
                          # ---- Seafood Comparison Card ----
                          
                          div(id = "explore_seafood_card",
                              
                              style = "flex: 1 1 300px; max-width: 350px; cursor: pointer; position: relative;
                                       padding-top: 50px; padding-bottom: 10px; background-color: white;
                                       border-radius: 8px; box-shadow: 0 4px 10px rgba(0,0,0,0.1);
                                       overflow: visible; font-family: sans-serif; text-align: center;",
                              
                              div(style = "height: 12px; background-color: #F9B928;
                                          border-top-left-radius: 8px; border-top-right-radius: 8px;
                                          position: absolute; top: 0; left: 0; right: 0;"), # END Card background div
                              
                              div(style = "position: absolute; top: -28px; left: 50%; transform: translateX(-50%);
                                          background-color: #F9B928; width: 56px; height: 56px;
                                          border-radius: 50%; display: flex; align-items: center;
                                          justify-content: center; z-index: 10;",
                                  
                                  tags$i(class = "fas fa-chart-bar",
                                         style = "color: white; font-size: 24px;") # END tags$i
                                  ), # END div for icon holder
                              
                              h4("Compare Seafood Emissions",
                                 style = "color: #0B2232; font-weight: 600;"),
                              
                              p("Use this tool to compare CO₂ emissions by country and seafood category, combining Global Fishing Watch activity data with FAO catch statistics.",
                                style = "color: #444; padding: 0 20px; margin-bottom: 25px;"))), # END div for explore_seafood_card
                      
                      # Flexbox for logos at the top
                      div(style = "position: relative;
                                   display: flex;
                                   flex-wrap: wrap;
                                   justify-content: center;
                                   align-items: center;
                                   gap: 40px;
                                   padding-top: 40px;
                                   z-index: 2;",
                          
                          tags$img(src = "images/logos/emlab-logo-color.png",
                                   style = "height: 85px;"), # END tags$img for emlab logo
                          tags$img(src = "images/logos/gfw-logo.png",
                                   style = "height: 80px;") # END tags$img for gfw logo
                          ), # END div for logos
                      
                      # ---- Learn More Link ----
                      div(style = "position: relative; z-index: 2; margin-top: 40px;",
                          fluidRow(
                            div(id = "learn_more_link",
                                style = "display: flex; 
                                         justify-content: center; 
                                         align-items: center; 
                                         gap: 5px; 
                                         flex-wrap: wrap;",
                                
                                tags$p("Click here",
                                       style = "font-weight: 600; color: white; cursor: pointer; text-decoration: underline; display: flex; flex-wrap: wrap;"), # END tags$p
                                
                                tags$p("to learn more about our project, the data behind it, and its intended use.",
                                       style = "font-weight: 400; color: white; display: flex; flex-wrap: wrap;") # END tags$p
                            ) # END learn_more_link div
                          ) # END fluidRow
                      ) # END learn more section
                  ) # END Unified Hero Section
                  
                  
                
  ), # END tabPanel(Home)
  
# =================================================================================================================================================================================================
# Emissions Map Tab
# =================================================================================================================================================================================================


  shiny::tabPanel("Emissions Map",
                  useShinyjs(),
                  
                  
                  # ---- Map Container ----
                  div(style = "position: relative; height: 90vh;",
                      
                      # Wrapper around background + icon button
                      div(style = "position: absolute; top: 40%; width: 40px; height: 50px; z-index: 1000;",
                          
                          # Background layer behind the sidebar button (below icon)
                          div(id = "sidebar_toggle_background",
                              style = "position: absolute;
                                           top: 0px;
                                           left: 310px;
                                           width: 44px; height: 50px;
                                           background-color: #F9F9F9;
                                           border-top-right-radius: 6px;
                                           border-bottom-right-radius: 6px;
                                           z-index: 900;"),
                          
                          # Toggle sidebar close
                          actionButton("toggle_sidebar_open_input",
                                       label = NULL,
                                       icon = icon("angle-left", style = "font-size: 25px; color: #DA8D03;margin-left: -15px;"),
                                       style = "position: absolute;
                                                    top: 0px;
                                                    left: 310px;
                                                    width: 40px;
                                                    height: 50px;
                                                    background-color: transparent;
                                                    border: none;
                                                    z-index: 1600;") # END action button 
                      ), # divider
                      
                      # ---- Sidebar Panel --------------------------------------------
                      div(id = "sidebar-panel",
                          style = "position: absolute;
                                   top: 0;
                                   left: 0;
                                   height: 100%;
                                   overflow-y: auto;
                                   overflow-x: hidden;
                                   width: 20%;
                                   min-width: 310px;
                                   max-width: 320px;
                                   min-height: 500px;
                                   background-color: #f9f9f9;
                                   padding: 15px;
                                   border-right: 0px solid #ccc;
                                   z-index: 1001;",
                          
                          
                          # Map title
                          tags$h3(style = "font-size: 24px; font-weight: 400; color: #20404F; margin-bottom: 5px;", 
                                  "Fishing Vessel Emissions"), 
                          
                          # Map description
                          tags$p(style = "font-weight: 400; color: #20404F; margin-bottom: 5px;font-size: 16px;", 
                                 "This map features a novel dataset from Global Fishing Watch and emLab that models global fishing vessel emissions by combining Automatic Identification System (AIS, which acts like GPS tracking for ships) with satellite-based Synthetic Aperture Radar (SAR, which functions like radar from space). Together, these technologies allow us to detect both broadcasted and non-broadcasted fishing activity."), 
                          
                          # Horizontal separator
                          tags$hr(),
                          
                          # ---- Sidebar Layer Controls ----------------------------------
                          
                          # ---- Controls for broadcasting emissions data ----
                          column(
                            width = 12,
                            
                            div(
                              style = "display: flex; align-items: center; gap: 8px;",
                              
                              # Material switch to toggle on/off broadcasting data
                              div(
                                style = "margin-top: -20px;",
                                materialSwitch(
                                  inputId = "show_broadcasting_input",
                                  label = tags$div(
                                    style = "display: flex; align-items: center; gap: 6px; font-size: 18px; font-weight: 400; color: #20404F; margin: 0; z-index: 1002;",
                                    
                                    # Label text
                                    tags$span("Broadcasted Emissions"),
                                    
                                    # Info icon
                                    infoPopup(
                                      id = "broadcasting_popup",
                                      description = "This global dataset maps fishing vessel CO₂ emissions modeled from vessels that broadcast their location using the Automatic Identification System (AIS). Vessels included in this dataset were classified as participating in apparent fishing effort based on their movement patterns.",
                                      interpretation = "Emissions are aggregated annually in a global 1×1° latitude–longitude grid. Ligher colors mean higher CO₂ emissions.",
                                      data_source = "Global Fishing Watch",
                                      learn_more = "https://globalfishingwatch.org/user-guide/#Activity%20-%20Fishing:~:text=methodology%20paper.-,Understanding%20apparent%20fishing%20effort%20using%20AIS%20and%20VMS%20data,-Automatic%20identification%20system") # END infoPopup 
                                  ),
                                  value = TRUE,
                                  status = "info"
                                ) # END materialSwitch
                              ) # END switch wrapper
                            ) # END outer flex row
                          ), # END column
                          
                          
                          # Legend for broadcasting layer, hidden when layer isn't visible on map
                          hidden(
                            
                            div(id = "broadcasting_legend",
                                
                                # Gradient legend bar
                                tags$div(style = "display: flex; width: 70%; height: 20px; border: 1px solid #ccc;",
                                         tags$div(style = "flex: 1; background-color: #20404F;"),
                                         tags$div(style = "flex: 1; background-color: #4C9EA6;"),
                                         tags$div(style = "flex: 1; background-color: #67D6E0;"),
                                         tags$div(style = "flex: 1; background-color: #76F3FF;"),
                                         tags$div(style = "flex: 1; background-color: #A9F2FF;"),
                                         tags$div(style = "flex: 1; background-color: #DAF3FF;"),
                                         tags$div(style = "flex: 1; background-color: #F6F8FF;")  # END final color box
                                ), # END gradient legend bar
                                
                                # Min/Max labels and total emissions
                                tags$div(
                                  style = "display: flex; justify-content: space-between;
                font-size: 15px; font-weight: regular; color: #053762;
                margin-bottom: 10px; width: 100%;",
                                  tags$span("200"),
                                  textOutput("total_broadcasting", inline = TRUE)
                                ),
                                
                                # Label text
                                tags$span("Select a Flag State (Country) "),
                                
                                # Info icon
                                infoPopup(
                                  id = "country_popup",
                                  description = "A flag State represents a vessel’s country of registration and holds jurisdiction over its operations—regardless of where the vessel travels. This layer allows you to filter broadcasted emissions by the flag State responsible for each vessel.",
                                  data_source = "Global Fishing Watch",
                                  learn_more = "https://globalfishingwatch.org/user-guide/#Activity%20-%20Fishing:~:text=methodology%20paper.-,Understanding%20apparent%20fishing%20effort%20using%20AIS%20and%20VMS%20data,-Automatic%20identification%20system"
                                ), # END infoPopup
                                
                                # Country select dropdown
                                pickerInput(inputId = "country_select_input",
                                            label = NULL,
                                            choices = c("All Countries", sort(unique(broadcasting_emissions$country_name[broadcasting_emissions$country_name != "All Countries"]))),
                                            selected = "All Countries",
                                            options = list(`live-search` = TRUE,
                                                           `noneSelectedText` = "All Countries")), # END pickerInput (country select)
                                
                                # Add no data warning (when a country for a selected year has no emissions)
                                tags$div(
                                  textOutput("no_data_warning"),
                                  style = "color: #81818F;
                                           margin-top: 10px;"), # END div (no data warning text)
                                
                                # Add low emissions warning (when a country for a selected year has low, less than 1000 emissions, and it may be hard to locate)
                                tags$div(
                                  textOutput("low_emissions_warning"),
                                  style = "color: #81818F;
                                          margin-top: 10px;") # END div (low emissions warning text)
                                
                            ) # END div (broadcasting_legend)
                          ), # END hidden (broadcasting emissions legend and text)
                          
                          
                          # Horizontal separator
                          tags$hr(),
                          
                          
                          # ---- Controls for non-broadcasting emissions data ----
                          column(
                            width = 12,
                            
                            div(
                              style = "display: flex; align-items: center; gap: 8px;",
                              div(
                                style = "margin-top: -20px;",
                                
                                # Material switch to toggle on/off non-broadcasting data
                                materialSwitch(
                                  inputId = "show_non_broadcasting_input",
                                  label = tags$div(
                                    style = "display: flex; align-items: center; gap: 6px; font-size: 18px; font-weight: 400; color: #20404F; margin: 0;",
                                    
                                    # Label text
                                    tags$span("Non-Broadcasted Emissions"),
                                    
                                    # Info icon
                                    infoPopup(
                                      id = "non_broadcasting_popup",
                                      description = "This global dataset maps estimated CO₂ emissions from fishing vessels that do not broadcast their location using the Automatic Identification System (AIS). Instead, these vessels are detected using Synthetic Aperture Radar (SAR)—a satellite-based technology that captures images with microwave pulses, allowing for detection in any weather or lighting conditions.
Vessel positions are derived from Copernicus Sentinel-1 imagery using a combination of traditional detection methods and machine learning. Vessels were classified as likely engaged in apparent fishing effort based on characteristics such as vessel size, proximity to regions with historical fishing activity, and other spatial indicators.",
                                      interpretation = "Emissions are aggregated annually in a global 1×1° latitude–longitude grid. Ligher colors mean higher CO₂ emissions.",
                                      data_source = "Global Fishing Watch",
                                      learn_more = "https://globalfishingwatch.org/user-guide/#Radar%20detections%20-%20Synthetic%20aperture%20radar:~:text=Detections-,Radar%20detections%20%2D%20Synthetic%20aperture%20radar,-Synthetic%20aperture%20radar"
                                    ) # END infoPopup
                                  ),
                                  value = FALSE,
                                  status = "warning"
                                ) # END materialSwitch
                              ) # END switch wrapper
                            ) # END outer flex row
                          ), # END column
                          
                          # Legend for non-broadcasting layer, hidden when layer isn't visible on map
                          hidden(
                            
                            div(id = "non_broadcasting_legend",
                                
                                # Discrete color blocks
                                tags$div(
                                  style = "display: flex; width: 70%; height: 20px; border: 1px solid #ccc;",
                                  tags$div(style = "flex: 1; background-color: #7A5100;"),
                                  tags$div(style = "flex: 1; background-color: #B97700;"),
                                  tags$div(style = "flex: 1; background-color: #FFB300;"),
                                  tags$div(style = "flex: 1; background-color: #FFD54F;"),
                                  tags$div(style = "flex: 1; background-color: #FFEB99;"),
                                  tags$div(style = "flex: 1; background-color: #FFF5CC;"),
                                  tags$div(style = "flex: 1; background-color: #FFFEF0;")
                                ), # END div for tags$div
                                
                                # Min/Max labels
                                tags$div(
                                  style = "display: flex; justify-content: space-between;
                 font-size: 15px; font-weight: regular; color: #053762;
                 margin-bottom: 10px; width: 100%;",
                                  tags$span("200"),
                                  textOutput("total_non_broadcasting", inline = TRUE)
                                ) # END div for legend max value
                            ) # END div for non-broadcasting legend
                          ), # END hidden
                          
                          # Horizontal separator
                          tags$hr(),
                          
                          
                          # ---- Control for FAO Major Fishing Zones ----
                          column(
                            width = 12,
                            
                            div(
                              style = "display: flex; align-items: center; gap: 8px;",
                              
                              div(
                                style = "margin-top: -20px;",
                                materialSwitch(
                                  inputId = "show_fao_zones_input",
                                  label = tags$div(
                                    style = "display: flex; align-items: center; gap: 6px; font-size: 18px; font-weight: 400; color: #20404F; margin: 0;",
                                    
                                    # Label text
                                    tags$span("FAO Major Fishing Areas"),
                                    
                                    # Info icon
                                    infoPopup(
                                      id = "fao_zone_popup",
                                      description = "FAO Major Fishing Areas are standardized ocean regions defined by the Food and Agriculture Organization (FAO) of the United Nations to facilitate the global monitoring and reporting of marine fisheries. There are 19 Major Fishing Areas, each representing a large, ecologically or geographically distinct part of the ocean.",
                                      data_source = "Food and Agriculture Organization",
                                      learn_more = "https://www.fao.org/fishery/en/area/search"
                                    ) # END infoPopup
                                  ),
                                  value = FALSE,
                                  status = "info"
                                ) # END materialSwitch
                              ) # END switch wrapper
                            ) # END outer flex row
                          ), # END column
                          
                      ), # END sidebar panel
                      
                      
                      # Button - Close sidebar
                      actionButton("toggle_sidebar_close_input",
                                   label = NULL,
                                   icon = icon("layer-group",
                                               style = "font-size: 20px;  padding-right: 6px;"),
                                   style = "position: absolute;
                                             top: 40%;
                                             left: -31px;
                                             width: 40px;
                                             height: 50px;
                                             display: flex;
                                             background-color: #f9f9f9;
                                             border: none;
                                             display: none;
                                             z-index: 1051;"), # END actionButton for sidebar to close sidebar
                      
                      # ---- Emissions Map --------------------------------------------
                      mapdeckOutput("emissions_map", height = "100%"),
                      useShinyjs(),
                      uiOutput("loading_ui"),
                      
                      # ---- Year Slider input panel----
                      absolutePanel(bottom = 30,
                                    right = 15,
                                    style = "z-index: 1000;
                                    background-color: rgba(255,255,255,0.8);
                                    padding: 8px;
                                    border-radius: 8px;
                                    width: 20%;
                                    min-width: 250px;",
                                    
                                    # Label text
                                    tags$span("Select Year"),
                                    
                                    # Info icon
                                    infoPopup(
                                      id = "year_map_popup",
                                      description = "Each grid cell in the emissions data displayed is aggregated by year. Use the slider to select a year or click the play button to animate emissions trends over time.",
                                      data_source = NULL,
                                      learn_more = NULL),
                                    
                                    # Define sliderInput to filter data to year
                                    sliderInput("year_slider_input_map",
                                                NULL,
                                                min = year_min,
                                                max = year_max,
                                                value = year_max,
                                                step = 1,
                                                sep = "",
                                                width = "100%",
                                                ticks = TRUE) # END sliderInput (year)
                      ) # END absolutePanel - year
                  ) # END map container
  ), # END emissions map tab
  
# =================================================================================================================================================================================================
# Compare Seafood Emissions Tab
# =================================================================================================================================================================================================

shiny::tabPanel("Compare Seafood Emissions",
  
  # Enable ShinyJS
  useShinyjs(),
  
  # Settings for scrollable drop down menu for select_country_input ----
  tags$style(HTML("
  .dropdown-menu.inner {
    max-height: 300px !important;
    overflow-y: auto !important;
  }
")), 
  
  # Custom settings for radio button selection
  tags$script(HTML("
  Shiny.addCustomMessageHandler('set_initial_unit', function(value) {
    const el = document.querySelector('input[name=unit_plot_toggle_input][value=' + value + ']');
    if (el) {
      el.checked = true;
      Shiny.setInputValue('unit_plot_toggle_input', value, { priority: 'event' });
    }
  });
")),
  
  # Custom settings for radio button styling
  tags$style(HTML("
  /* ---- Native radio input style ---- */
  input[type='radio'][name='unit_plot_toggle_input'] {
    accent-color: #08C4E5;  /* Orange border (Seamissions) */
    width: 18px;
    height: 18px;
  }

  input[type='radio'][name='unit_plot_toggle_input'] + label {
    font-weight: 400;
    color: #20404F;
    font-size: 16px;
    cursor: pointer;
  }

  /* ---- Pretty checkbox/radio overrides ---- */
  .pretty.p-default input[type='radio'] ~ .state label:before {
    border: 1px solid #DA8D03 !important; /* Orange border */
  }

  .pretty.p-default input[type='radio']:checked ~ .state label:after {
    background-color: #08C4E5 !important; /* Light blue fill */
  }

  .pretty.p-default {
    margin-bottom: 8px;
    margin-top: 8px;
  }

  /* ---- Button classes for toggles ---- */
  .plot-button {
    background-color: #08C4E5 !important;
    color: white !important;
    border: none !important;
  }

  .plot-button-active {
    background-color: #F9B928 !important;
    color: black !important;
  }
")),
 
  # ----------------------------------------------------------------------------
  # Define plot headers and controls
  # ----------------------------------------------------------------------------

  div(id = "plot_main_content",
      div(
        style = "scrollbar-width: auto; min-height: 100px;",
        
        # ---- Header section ----
        fluidRow(
          tags$p("Compare Seafood Emissions", style = "color: white; font-size: 30px; font-weight: bold; white-space: normal; padding-left: 50px; padding-right: 50px; text-align: center;"),
          tags$p("Explore our dataset, which links fishing vessel emissions from Global Fishing Watch with annual catch report data from the Food and Agriculture Organization (FAO) of the United Nations.", style = "color: white; font-size: 18px; font-weight: bold; white-space: normal; padding-left: 50px; padding-right: 50px; text-align: center;")
        ), # END fluidRow
        
        # --- Action button controls to select visible plot---
        fluidRow(
          column(width = 12,
                 div(style = "text-align: center;",
                     
                     # Action button for compare species
                     div(style = "display: inline-block; margin: 10px;",
                         actionButton("compare_species_input",
                                      tagList(icon("fish", style = "margin-right: 8px;"), "Compare Top Species"),
                                      class = "btn btn-lg") # END actionButton for compare species
                     ), # END divider for action button
                     
                     # Action button for compare countries
                     div(style = "display: inline-block; margin: 10px;",
                         actionButton("compare_countries_input",
                                      tagList(icon("earth-americas", style = "margin-right: 8px;"), "Compare Top Countries"),
                                      class = "btn btn-lg") # END actionButton for compare countries
                     ), # END divider for action button
                     
                     # Action button for select a country
                     div(style = "display: inline-block; margin: 10px;",
                         actionButton("select_country_input",
                                      tagList(icon("flag", style = "margin-right: 8px;"), "Select a Country"),
                                      class = "btn btn-lg") # END actionButton for select a country
                     ) # END divider for action button
                 ) # END div for all action buttons
          ) # END column for all action buttons
        ), # END fluid row for all action buttons
        
        # ---- Stying for `country_select_plot_input` (hidden when select a country plot input is not selected)
        shinyjs::hidden(
          div(id = "country_select_plot_input",
              fluidRow(
                column(width = 12,
                       div(style = "text-align: center;",
                           div(
                             style = "display: inline-block; margin-right: 20px;",
                             pickerInput(
                               inputId = "selected_country_input",
                               label = tags$span("Select a Country:", style = "color: white;font-size: 18px;"),
                               choices = c("Select a country" = "", sort(unique(seafood_emissions_data$country_name))),
                               selected = NULL,
                               options = list(
                                 `live-search` = TRUE,
                                 `noneSelectedText` = "All Countries") # END options
                             ) # END picker Input
                           ) # END div
                       ) # END div
                ) # END column
              ) # END fluidRow
          ) # END div
        ), # END hidden
        
        # ----------------------------------------------------------------------------
        # Define plot area
        # ----------------------------------------------------------------------------
      
        fluidRow(
          column(width = 12,
                 div(style = "background-color:#0B2232; margin: 30px 20px; overflow: visible !important;",
                    
                      # ---- Top 10 Country Plots (Hidden when not selected) ----
                      div(
                       id = "country_plot",
                       
                       # Title and Info Icon on the same line
                       
                       div(
                         style = "display: flex; justify-content: center; align-items: center; gap: 8px;",
                         tags$h4(
                           "Top Emitting Countries", 
                           style = "color: #DA8D03; font-size: 25px; font-weight: bold; margin: 0;"), # END tags$h4
                         infoPopup(
                           id = "top_country_plot_popup",
                           description = "These plots highlight the top 10 countries with the highest annual CO₂ emissions from fishing. Emissions are estimated by linking satellite-based emissions data from Global Fishing Watch with catch reports submitted to the Food and Agriculture Organization (FAO) of the United Nations.",
                           data_source = NULL,
                           learn_more = NULL) # END infoPopup
                       ), # END div
                       
                       # Subtitle
                       uiOutput("country_subtitle"),
                       
                       # Output
                         div(style = "min-width: 1500px; min-height: 300px;",
                             plotOutput("country_plot_output", height = "60vh", width = "100%") |> withSpinner(type = 4, color = '#08C4E5')
                         ) # END div
                     ), # END div for country plot
                     
                     # ---- Top 10 Species Groups Plots (Hidden when not selected) ----
                    
                     shinyjs::hidden(
                       div(
                         id = "isscaap_plot",
                         
                         # Title and Info Icon on the same line
                         div(
                           style = "display: flex; justify-content: center; align-items: center; gap: 8px;",
                           tags$h4(
                             "Top Emitting Species Groups", 
                             style = "color: #DA8D03; font-size: 25px; font-weight: bold; margin: 0;"
                           ),
                           infoPopup(
                             id = "top_species_plot_popup",
                             description = "These plots highlight the top 10 species groups with the highest annual CO₂ emissions from fishing. Emissions are estimated by linking satellite-based emissions data from Global Fishing Watch with catch reports submitted to the Food and Agriculture Organization (FAO) of the United Nations.
Species are categorized using ISSCAAP (International Standard Statistical Classification of Aquatic Animals and Plants) groups, a system developed by the FAO to categorize species into standardized groups based on biological and ecological characteristics.",
                             data_source = NULL,
                             learn_more = NULL
                           ) # END infoPopup
                         ), # Div species plot title
                         
                         # Plot Subtitle
                         uiOutput("species_subtitle"),
                         
                         # Plot Output
                         div(
                           style = "min-width: 1500px; min-height: 300px;",
                           plotOutput("isscaap_plot_output", height = "60vh", width = "100%") |> 
                             withSpinner(type = 4, color = '#08C4E5')) # END div
                       ) # END div for isscaap_plots
                     ), # END hidden
                     
                     # ---- Select a Country Plot (Hidden when not selected) ----
                     
                     div(id = "species_bar_plot_wrapper",  style = "display: flex; flex-direction: column; align-items: center; gap: 8px;",
                         uiOutput("dynamic_country_header"),
                       
                         shinyjs::hidden(
                           div(id = "species_bar_plot",
                               div(style = "min-width: 1600px; min-height: 300px;",
                                   uiOutput("species_bar_plot_ui") |> withSpinner(type = 4, color = '#08C4E5')) # END div
                           ) # END div for species bar plot
                         ) # END hidden
                     ) # END wrapper div
                 ) # END div for all plots
          ) # END column for all plots
        ) # END fluidRow for all plots
      ) # END div for scrollable area
  ), # END plot page div content
  
  # ----------------------------------------------------------------------------
  # Define plot controls on bottom of page
  # ----------------------------------------------------------------------------
  
  fluidRow(
    # ---- Controls Row -----------------------------------------------------
    div(style = "display: flex; justify-content: center; flex-wrap: wrap;",
        
        # ---- Plot Unit Toggle -----------------------------------------------
        div(
          style = "z-index: 1000;
           background-color: rgba(249, 249, 249, 0.9);
           padding: 8px 16px;
           border-radius: 8px;
           width: 250px;
           margin: 10px;",
          
          tags$span("Select Plot Unit"),
          
          # Total emissions button
          div(style = "display: flex; align-items: center; gap: 8px; margin-top: 10px;",
              tags$input(type = "radio", name = "unit_plot_toggle_input", value = "total", id = "radio_total", checked = "checked"),
              tags$label(`for` = "radio_total", "Total Emissions"),
              infoPopup(
                id = "total_emissions_popup",
                description = "This plot shows total annual CO₂ emissions for all apparent fishing activity, summed for each country in the selected year. Emissions include both AIS-broadcasting vessels and estimated contributions from non-broadcasting vessels, which may be attributed to a country based on its flagged fleet's fishing effort and reported catch in the region. The top 10 highest-emitting fleets are shown here.",
                interpretation = "Each bar represents the total CO₂ emissions (in metric tons) attributed to a country's fishing fleet for the selected year.",
                data_source = "This dataset, developed by the Seamissions team, links fishing vessel emissions data from Global Fishing Watch with wild-caught seafood catch data from the Food and Agriculture Organization (FAO) of the United Nations.") # END infoPopup
          ), # END div for total emissinos button
          
          # Per unit catch button
          div(style = "display: flex; align-items: center; gap: 8px; margin-top: 6px;",
              tags$input(type = "radio", name = "unit_plot_toggle_input", value = "per_unit", id = "radio_per_unit"),
              tags$label(`for` = "radio_per_unit", "Emissions Efficiency"),
              infoPopup(
                id = "catch_unit_emissions_popup",
                description = "This plot shows total annual CO₂ emissions for all apparent fishing activity, summed for each country in the selected year. Emissions include both AIS-broadcasting vessels and estimated contributions from non-broadcasting vessels, which may be attributed to a country based on its flagged fleet's fishing effort and reported catch in the region. The top 10 highest-emitting fleets are shown here.",
                interpretation = "Each bar represents emissions efficiency, calculated as the total annual CO₂ emissions per metric ton of annual reported catch for the selected year. Higher values mean less efficient catch rates, lower values mean more efficient catch rates. Since there are some ",
                data_source = "This dataset, developed by the Seamissions team, links fishing vessel emissions data from Global Fishing Watch with wild-caught seafood catch data from the Food and Agriculture Organization (FAO) of the United Nations.") # END infoPopup
              ), # END div for per unit catch button
          
          # Save selection to Shiny input
          tags$script(HTML("
                          document.querySelectorAll('input[name=unit_plot_toggle_input]').forEach(el => {
                            el.addEventListener('change', (e) => {
                              Shiny.setInputValue('unit_plot_toggle_input', e.target.value, { priority: 'event' });
                            });
                          });
                        ")) # END tags$script
        ), # END div for plot unit toggle
        
        
        # ---- Year Selector ---------------------------------------------------
        div(style = "z-index: 1000;
              background-color: rgba(249, 249, 249, 0.9);
              padding: 8px 16px;
              border-radius: 8px;
              width: 250px;
              margin: 10px;",
            
            tags$span("Select Year"),
            infoPopup(
              id = "year_plot_popup",
              description = "Data displayed in these plots are aggregated by year. Use the slider to select a year.",
              data_source = NULL,
              learn_more = NULL), # END infoPopup
            
            sliderInput("year_slider_input_plot",
                        NULL,
                        min = 2016,
                        max = 2022,
                        value = 2022,
                        step = 1,
                        sep = "",
                        width = "100%",
                        ticks = TRUE) # END sliderInput
        ) # END year box
    ) # END div for bottom controls row
   
  ), # END Fluid Row
  
  fluidRow(
    div(style = "display: flex; justify-content: center; align-items: center; gap: 5px; flex-wrap: wrap;",
        id = "learn_more_link",
        tags$p("Click here",
               style = "font-weight: 600; color: white; cursor: pointer; text-decoration: underline; display: flex; flex-wrap: wrap;"),
        tags$p("to learn more about our project, the data behind it, and its intended use.",
               style = "font-weight: 400; color: white; cursor: pointer; display: flex; flex-wrap: wrap;")
    ) # END text div
  ) # END fluidRow
  
  ), # END tabPanel
  

# =================================================================================================================================================================================================
# Learn more tab
# =================================================================================================================================================================================================

tabPanel("Learn More",
         # ---- Hero Section ----
         div(style = "position: relative;
               min-height: 200px;
               width: 100%;
               padding-left: 0px !important;
               background-image: url('images/ocean-banner.png');
               background-size: cover;
               background-position: center;
               background-attachment: fixed;
               text-align: center;
               color: #e8fffd;",
             
             # Overlay
             div(style = "position: absolute; 
                   top: 0; left: 0; right: 0; bottom: 0;
                   background-color: rgba(0, 0, 0, 0.5);
                   z-index: 1;"),
             
             
             # Hero Text
             div(style = "position: relative; z-index: 2;
                    padding-top: 60px; padding-left: 40px; padding-right: 40px;",
                 
                 h1("Learn More",
                    style = "font-weight: 600 !important;"),
                 
                 
             ) # END div
         ),
         
         # ---- Page content ----
         
         fluidRow(
           column(12,
                  div(
                    style = "padding: 30px; text-align: left; color: white;",
                    
                    tags$h4(tags$strong("Key Considerations"),
                            style = "margin-bottom: 30px;  margin-left: 30px"),
                    
                    fluidRow(
                      column(
                        width = 12,
                        tags$p(
                          style = "font-weight: normal; margin-left: 30px; color: white; margin-bottom: 10px",
                          "This dashboard is intended to be an educational and exploratory tool, not a basis for scientific conclusions. All results should be interpreted with care due to some key limitations:"
                        )
                      )
                    ),
                    
                    tags$ul(
                      tags$li(
                        style = "margin-left: 35px; margin-bottom: 10px",
                        tags$u("Ambiguity in catch location and reporting"),
                        " –  stemming from the usage of “flags of convenience” as well as underreporting and inconsistencies between the FAO region where the fish were caught versus the country where landings were reported."
                      ), 
                      tags$li(
                        style = "margin-left: 35px; margin-bottom: 10px",
                        tags$u("Proportional emissions allocation by catch"
                        ),
                        " – assuming that within an FAO region, non-broadcasting emissions are proportional across", tags$em("all"), " fisheries and countries", tags$em("by catch.")
                      ),
                      tags$li(
                        style = "margin-left: 35px; margin-bottom: 10px",
                        tags$u("Dependence on harmonized datasets with different formats and assumptions"),
                        " – including the integration of FAO catch records and GFW’s fishing vessel emissions dataset, which originate from distinct sources with differing scopes, structures, and assumptions."
                      )
                    )
                  ) # END div
           )
           
         ), # END fluidrow 'Key Considerations'
         
         fluidRow(
           column(
             width = 12,
             div(style = "padding: 30px; text-align: left; color: white;",
                 tags$h4(strong("Background"),
                         style = "margin-bottom: 30px;  margin-left: 30px"),
                 tags$p(style = "font-weight: normal; text-align: left; color: white; margin: 30px;", 
                        "Global fisheries are heavily reliant on fossil fuels, contributing significantly to the rise in global greenhouse gas emissions driving climate change. While satellite technology is commonly used to monitor land-based emissions (and ocean-based emissions of shipping vessels), studies primarily estimating ocean-based emissions in the fishing sector remain limited. In collaboration with the Environmental Markets Lab (emLab) and Global Fishing Watch, this project leverages novel, high-resolution satellite-based datasets to provide precise insights into the emissions associated with global fisheries. We develop a reproducible, extensible, and open-source data processing pipeline to connect emissions data with seafood production data, along with an interactive dashboard to explore the resulting dataset.",
                        tags$br(),
                        tags$br(),
                        "Please visit our GitHub to learn more about this project.",
                        tags$br(),
                        tags$a(href = "https://github.com/Seamissions", target = "_blank", "https://github.com/Seamissions"))  
             ) # END div
           )
           
         ), # END fluidrow 'Background'
         
         fluidRow(
           column(
             width = 12,
             div(
               style = "padding: 30px; color: white;",
               tags$h4(strong("Who We Are"),
                       style = "margin-bottom: 30px;  margin-left: 30px"),
               
               tags$p(
                 style = "font-weight: normal; color: white; margin-bottom: 20px;  margin-left: 30px",
                 "We are a team of environmental data scientists working to quantify and demystify the emissions contributed through commercial fishing fleets."
               ),
               
               tags$p(
                 style = "font-weight: normal; margin-top: 15px; margin-left: 30px; color: white",
                 tags$strong("Carmen Hoyt:"), " ",
                 tags$a(href = "https://bren.ucsb.edu/people/carmen-hoyt", target = "_blank", "Bren Profile"), " | ",
                 tags$a(href = "https://github.com/orgs/Seamissions/people/ceh58", target = "_blank", "Github"), " | ",
                 tags$a(href = "https://www.linkedin.com/in/carmen-hoyt-952272153/", target = "_blank", "LinkedIn"), " | ",
                 tags$a(href = "https://ceh58.github.io/", target = "_blank", "Website")
               ),
               
               tags$p(
                 style = "font-weight: normal; margin-top: 10px; margin-left: 30px; color: white",
                 tags$strong("Josh Mull:"), " ",
                 tags$a(href = "https://bren.ucsb.edu/people/joshua-mull", target = "_blank", "Bren Profile"), " | ",
                 tags$a(href = "https://github.com/orgs/Seamissions/people/llumj", target = "_blank", "Github"), " | ",
                 tags$a(href = "https://www.linkedin.com/in/joshua-mull-046a7832b/", target = "_blank", "LinkedIn"), " | ",
                 tags$a(href = "https://llumj.github.io/", target = "_blank", "Website")
               ),
               
               tags$p(
                 style = "font-weight: normal;margin-top: 10px; margin-left: 30px; color: white",
                 tags$strong("Nicole Pepper:"), " ",
                 tags$a(href = "https://bren.ucsb.edu/people/nicole-pepper", target = "_blank", "Bren Profile"), " | ",
                 tags$a(href = "https://github.com/orgs/Seamissions/people/nicolelpepper", target = "_blank", "Github"), " | ",
                 tags$a(href = "https://www.linkedin.com/in/nicole-pepper/", target = "_blank", "LinkedIn"), " | ",
                 tags$a(href = "https://nicolelpepper.github.io/", target = "_blank", "Website")
               ),
               
               tags$p(
                 style = "font-weight: normal; margin-top: 10px; margin-left: 30px; color: white",
                 tags$strong("Stephen Carroll:"), " ",
                 tags$a(href = "https://bren.ucsb.edu/people/stephen-carroll", target = "_blank", "Bren Profile"), " | ",
                 tags$a(href = "https://github.com/orgs/Seamissions/people/stephenccodes", target = "_blank", "Github"), " | ",
                 tags$a(href = "https://www.linkedin.com/in/stephen-carroll-754b5191/", target = "_blank", "LinkedIn"), " | ",
                 tags$a(href = "https://stephenccodes.github.io/", target = "_blank", "Website")
               )
             ) # END div
           )
           
         ), # END fluidrow 'Who we are'
         
         fluidRow(
           column(12,
                  div(
                    style = "font-weight:bold; background-color: white; padding: 30px; color: #0B2232;",
                    
                    tags$h4(strong("Our Partners"),
                            style = "margin-bottom: 10px;  margin-left: 30px"),
                    
                    # emLab section
                    tags$img(
                      src   = "images/logos/emlab-logo-color-clear.svg",
                      style = "height: auto; max-height: 85px; width: auto;  margin-left: 30px; margin-top: 15px; margin-bottom: 15px"
                    ),
                    tags$h5(tags$u("Environmental Markets Lab"),
                            style = "margin-bottom: 30px;  margin-left: 30px"),
                    tags$p(
                      style = "font-weight: normal; color: #0B2232; margin-bottom: 15px; margin-left: 30px",
                      "The Environmental Markets Lab (emLab) is a think-and-do tank dedicated to market-based solutions for environmental challenges. Based at the University of California, Santa Barbara, this interdisciplinary team of scientists conducts cutting-edge, data-driven research on the power, limitations, and design of market-driven approaches to address the world's most pressing environmental problems."
                    ),
                    tags$p(
                      style = "font-weight: normal; color: #0B2232; margin-bottom: 20px; margin-left: 30px",
                      "In collaboration with implementation partners, they work to better align environmental objectives with economic incentives, supporting sustainable livelihoods and a resilient planet."
                    ),
                    
                    # Global Fishing Watch section
                    tags$img(
                      src   = "images/logos/gfw-logo-black-clear.svg",
                      style = "height: auto; max-height: 85px; width: auto;  margin-left: 30px; margin-bottom: 15px"
                    ),
                    tags$h5(tags$u("Global Fishing Watch"),
                            style = "margin-bottom: 30px;  margin-left: 30px"),
                    tags$p(
                      style = "font-weight: normal; color: #0B2232; margin-bottom: 15px; margin-left: 30px",
                      "Global Fishing Watch (GFW) is an independent, international nonprofit organization dedicated to advancing ocean governance. By publicly sharing map visualizations, data, and analysis tools, they help increase transparency of human activity at sea, enabling scientific research, and driving transformation in ocean management."
                    ),
                    tags$p(
                      style = "font-weight: normal; color: #0B2232; margin-bottom: 20px; margin-left: 30px",
                      "Established as a nonprofit organization in 2017, Global Fishing Watch was originally founded in 2015 through a collaboration between three partners: ",
                      tags$strong("Oceana,"), " an international ocean conservation organization; ",
                      tags$strong(" SkyTruth,"), " a technology firm that uses satellite imagery and data to protect the environment; and ",
                      tags$strong(" Google,"), " whose tools and contributions help process big data. "
                    )
                  ) # END div
           )
           
         ), # END fluidrow 'Our partners'
         
         fluidRow(
           column(12,
                  div(style = "font-weight:normal; padding: 30px; color: white;",
                      h4(strong("Methods"),
                         style = "margin-bottom: 30px;  margin-left: 30px"),
                      tags$p(
                        style = "font-weight: normal; color: white; margin: 30px;",
                        "Our team developed a structured process that can handle large amounts of emissions data provided by emLab from vessels with tracking signals (broadcasting) and those without (non-broadcasting).",
                        tags$br(),
                        tags$br(),
                        "For each FAO region, the total annual",
                        tags$strong("broadcasting"),"emissions are calculated for each country. The same steps are taken for ",
                        tags$strong("non-broadcasting"), "emissions, which are then proportionally distributed to all fisheries that report catch in that region.",
                        tags$br(),
                        tags$br(),
                        "Results are double-checked to ensure that all emissions were accounted for, allowing for minimal rounding error. This gives us a transparent, reproducible way to estimate fishing-related emissions at a global scale, helping to better understand the climate impact of seafood production.",
                        tags$br(),
                        tags$br(),
                        "For a more detailed look into our methods, please visit the ",
                        tags$a(href = "https://github.com/Seamissions", target = "_blank", "Seamissions GitHub."),
                        tags$br(),
                        tags$br(),
                        "Information on how GFW and emLab produced the data we used in our project can be found ",
                        tags$a(href = "https://emlab-ucsb.github.io/ocean-ghg/", target = "_blank", "here.")
                      ),
                  ) # END div
           )
           
         ), # END fluidrow 'Methods'
         
         fluidRow(
           column(12,
                  div(style = "padding: 30px; color: white;",
                      h4(strong("Emissions Map How-To"),
                         style = "margin-bottom: 30px;  margin-left: 30px"),
                      tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                             "Use the panel on the left-hand side to select ",
                             tags$strong("Broadcasted"), "emissions, ",
                             tags$strong("Non-Broadcasted"), "emissions, or both. Emissions can be viewed for ",
                             "individual", " or ", 
                             tags$strong("all countries"), "at once. The button to display ",
                             tags$strong("FAO regions"), "on the map is located here as well.",
                             tags$br(),
                             tags$br(),
                             "To explore the emissions for a specific ",
                             tags$strong("year"), " use the ",
                             tags$strong("slider box"), " on the lower- right side of the map."
                      ),  
                  ) # END div
           )
           
         ), # END fluidrow 'Emissions Map How-To'
         
         fluidRow(
           column(12,
                  div(
                    style = "padding: 30px; margin-bottom: 40px; color: white;",
                    
                    tags$h4(tags$strong("Glossary"),
                            style = "margin-bottom: 30px;  margin-left: 30px"),
                    
                    tags$ul(
                      tags$li(
                        style = "list-style-type: none; padding-left: 0; margin-bottom: 15px; color: white;",
                        tags$u("AIS (Automatic Identification System)"), tags$br(),
                        "A satellite-based tracking system used by large vessels to broadcast their location and movement. It helps monitor shipping activity, but many fishing vessels do not use it."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("Broadcasting Vessels"), tags$br(),
                        "Fishing vessels that transmit their location via AIS, making their movements and emissions trackable."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("FAO (Food and Agriculture Organization)"), tags$br(),
                        "A UN agency that collects official fisheries and aquaculture data from countries around the world."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("FAO Catch Data"), tags$br(),
                        "Country-reported records of fish and seafood harvested, often used as a global baseline for fisheries research."
                      ), 
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("Fishery"), tags$br(),
                        "The activity, industry, or location associated with catching or rearing fish."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("Flag (of a vessel)"), tags$br(),
                        "The country under which a fishing vessel is registered. Some vessels use “flags of convenience”, flags associated with different countries to flout regulation or take advantage of favorable legislation."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("Flag of Convenience"), tags$br(),
                        "When a vessel registers under a country different from its home country, often to take advantage of looser regulations."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("GHG (Greenhouse Gas)"), tags$br(),
                        "Gases that trap heat in the atmosphere and contribute to climate change. This project tracks emissions like CO₂, CH₄, and N₂O."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("Global Fishing Watch (GFW)"), tags$br(),
                        "A nonprofit that uses satellite and machine learning data to monitor fishing activities globally."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("ISSCAAP (International Standard Statistical Classification of Aquatic Animals and Plants)"), tags$br(),
                        "A classification system used by the FAO to group aquatic species for reporting purposes."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("Non-Broadcasting Vessels"), tags$br(),
                        "Fishing vessels that do not transmit AIS signals. Their emissions are harder to track and are estimated using satellite radar."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("Pipeline"), tags$br(),
                        "A series of connected data processing steps that clean, merge, and analyze raw data. The emissions pipeline in this project combines emissions and catch data."
                      ),
                      tags$li(
                        style = "list-style-type: none; padding-left: 0;  margin-bottom: 15px; color: white;",
                        tags$u("SAU (Sea Around Us)"), tags$br(),
                        "A project that reconstructs missing or underreported fisheries data to give a more complete picture of global fish harvests."
                      )
                    )
                  ) # END div
           )
         ) # END fluidrow 'Glossary'
         
  ) # END tabPanel (Learn More Page)
  
) # END navbarPage

