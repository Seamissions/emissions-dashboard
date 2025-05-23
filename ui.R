# ------------------------------------------------------------------------------------------------------------------
#     ui.R 
# ------------------------------------------------------------------------------------------------------------------


source("theme.R")

ui <- 
  tagList(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://api.tiles.mapbox.com/mapbox-gl-js/v2.13.0/mapbox-gl.css"
    ),
    tags$link(
      rel = "icon",
      type = "image/png",
      href = "images/logos/fav-icon.png"
    )
    
  )

navbarPage(
  title = "Seamissions Explorer",
  header = NULL, 
  windowTitle = "Seamissions Explorer",
  id = "navbarPage",
  theme = seamissions_theme,
  
  # ---- Head elements (fonts, styles) ----
  
  
  useShinyjs(),
  
  # ---- Home Panel ----
  shiny::tabPanel("Home",
                  
                  # ---- Hero Section ----
                  div(style = "position: relative;
               min-height: 500px;
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
                      
                      # Logo
                      tags$img(src = "images/logos/seamissions-logo.png",
                               style = "position: absolute;
                         top: 5%;
                         left: 50%;
                         transform: translateX(-50%);
                         height: 150px;
                         z-index: 2;"),
                      
                      # Hero Text
                      div(style = "position: relative; z-index: 2;
                    padding-top: 180px; padding-left: 40px; padding-right: 40px;",
                          
                          h1("Explore Global Seafood Emissions",
                             style = "font-weight: 600 !important;"),
                          
                          tags$hr(),
                          
                          tags$p(style = "font-weight: normal; color: white; margin-top: 30px;",
                                 "Understanding carbon emissions from fishing vessels is essential to understanding the full environmental impact of wild caught seafood — but tracking vessel activity at sea has long been a challenge... until now."),
                          
                          tags$p(style = "font-weight: normal; color: white; margin-top: 20px;",
                                 "This dashboard links CO₂ emissions estimates from Global Fishing Watch’s novel vessel emissions dataset with seafood catch data from the UN Food and Agriculture Organization (FAO). Users can track where fishing vessel emissions occur and compare how emissions for seafood production vary. Understanding where fishing activity and CO₂ emissions are concentrated is about more than just reducing CO₂. Improving the carbon efficiency of fisheries can also lead to better-managed fish stocks, healthier oceans, and cleaner air.")
                          
                          ) # END div
                  ),
                  
                  # ---- Teaser Cards ----
                  div(style = "margin-top: 40px; margin-bottom: 40px;",
                      div(style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 20px; padding: 0 20px;",
                          
                          # ---- Emissions Map Card ----
                          div(id = "explore_map_card",
                              style = "flex: 1 1 300px; max-width: 350px; cursor: pointer; position: relative;
                     padding-top: 50px; padding-bottom: 10px; background-color: white;
                     border-radius: 8px; box-shadow: 0 4px 10px rgba(0,0,0,0.1);
                     overflow: visible; font-family: sans-serif; text-align: center; margin: 25px;",
                              
                              div(style = "height: 12px; background-color: #08C4E5;
                      border-top-left-radius: 8px; border-top-right-radius: 8px;
                      position: absolute; top: 0; left: 0; right: 0;"),
                              
                              div(style = "position: absolute; top: -28px; left: 50%; transform: translateX(-50%);
                      background-color: #08C4E5; width: 56px; height: 56px;
                      border-radius: 50%; display: flex; align-items: center;
                      justify-content: center; z-index: 10;",
                                  tags$i(class = "fas fa-earth-americas", style = "color: white; font-size: 24px;")
                              ),
                              
                              h4(strong("Fishing Vessel Emissions Map")),
                              p("Explore a global map of CO₂ emissions from large-scale fishing vessels, powered by a novel dataset from Global Fishing Watch and emLab.",
                                style = "color: #444; padding: 0 20px;")
                          ),
                          
                          # ---- Seafood Comparison Card ----
                          div(id = "explore_seafood_card",
                              style = "flex: 1 1 300px; max-width: 350px; cursor: pointer; position: relative;
                     padding-top: 50px; padding-bottom: 10px; background-color: white;
                     border-radius: 8px; box-shadow: 0 4px 10px rgba(0,0,0,0.1);
                     overflow: visible; font-family: sans-serif; text-align: center; margin: 25px;",
                              
                              div(style = "height: 12px; background-color: #F9B928;
                      border-top-left-radius: 8px; border-top-right-radius: 8px;
                      position: absolute; top: 0; left: 0; right: 0;"),
                              
                              div(style = "position: absolute; top: -28px; left: 50%; transform: translateX(-50%);
                      background-color: #F9B928; width: 56px; height: 56px;
                      border-radius: 50%; display: flex; align-items: center;
                      justify-content: center; z-index: 10;",
                                  tags$i(class = "fas fa-chart-bar", style = "color: white; font-size: 24px;")
                              ),
                              
                              h4(strong("Compare Seafood Emissions")),
                              p("Use this tool to compare CO₂ emissions by country and seafood category, combining Global Fishing Watch activity data with FAO catch statistics.",
                                style = "color: #444; padding: 0 20px; margin-bottom: 25px;")
                          )
                      ) # END teaser card row
                  ), # END Teaser section
                  # ---- Hero Section ----
                  div(style = "position: relative;
             min-height: 200px;
             max-height: 300px;
             width: 100%;
             padding-left: 0px !important;
             background-image: url('images/ocean-banner.png');
             background-size: cover;
             background-position: center;
             background-attachment: fixed;
             text-align: center;
             color: #e8fffd;",
                      
                      # Overlay background
                      div(style = "position: absolute; 
                 top: 0; left: 0; right: 0; bottom: 0;
                 background-color: rgba(0, 0, 0, 0.5);
                 z-index: 1;"),
                      
                      # Flexbox for logos
                      div(style = "position: relative;
             display: flex;
             flex-wrap: wrap;
             justify-content: center;
             align-items: center;
             gap: 40px;
             padding-top: 30px;
             z-index: 2;",
                          
                          tags$img(src = "images/logos/emlab-logo-color.png",
                                   style = "height: 75px;"),
                          
                          tags$img(src = "images/logos/gfw-logo.png",
                                   style = "height: 70px;")
                      ),
                      
                      # Hero Text
                      div(style = "position: relative;
                           z-index: 2;
                           padding-top: 20px;
                           padding-left: 40px;
                           padding-right: 40px;",
                          
                          fluidRow(
                          div(style = "display: flex; justify-content: center; align-items: center; gap: 5px; flex-wrap: wrap;",
                              id = "learn_more_link",
                          tags$p("Click here",
                                 style = "font-weight: 600; color: white; cursor: pointer; text-decoration: underline; display: flex; flex-wrap: wrap;"),
                          
                          tags$p("to learn more about our project, the data behind it, and its intended use.",
                                 style = "font-weight: 400; color: white; cursor: pointer; display: flex; flex-wrap: wrap;")
                      ) # END text div
                          )
                  ) # END div 
                  ) # END Hero
                
  ), # END tabPanel(Home)
  
  
  # ------------------------------------------------------------------------------------------------------------------
  # ---- Emissions Map Page ------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------------
  
  shiny::tabPanel("Emissions Map",
                  useShinyjs(),
                  
                  
                  # ---- Map Container ----
                  div(style = "position: relative; height: 90vh;",
                      
                      # ---- Sidebar Panel --------------------------------------------
                      div(id = "sidebar-panel",
                          style = "position: absolute;
                           top: 0;
                           left: 0;
                           height: 100%;
                           width: 20%;
                           min-width: 310px;
                           max-width: 400px;
                           background-color: #f9f9f9;
                           padding: 15px;
                           border-right: 0px solid #ccc;
                           z-index: 1001;",
                          
                          # Wrapper around background + icon button
                          div(style = "position: absolute; top: 40%; right: -30px; width: 40px; height: 50px; z-index: 1000;",
                              
                              # Background layer behind the sidebar button (below icon)
                              div(style = "position: absolute;
                                           top: 0px; left: 0px;
                                           width: 40px; height: 50px;
                                           background-color: #F9F9F9;
                                           border-radius: 6px;
                                           z-index: 1000;"),
                              
                              # Actual icon button (higher z-index)
                              actionButton("toggle_sidebar_open_input",
                                           label = NULL,
                                           icon = icon("angle-left", style = "font-size: 25px; color: #DA8D03;margin-left: -15px;"),
                                           style = "position: absolute;
                                                    top: 0px;
                                                    left: 0px;
                                                    width: 40px;
                                                    height: 50px;
                                                    background-color: transparent;
                                                    border: none;
                                                    z-index: 1001;")
                                                    ),
                          
                          # Map title
                          tags$h3(style = "font-size: 24px; font-weight: 400; color: #20404F; margin-bottom: 5px;", 
                                  "Fishing Vessel Emissions"), 
                          
                          # Map description
                          tags$p(style = "font-weight: 400; color: #20404F; margin-bottom: 5px;font-size: 12px;", 
                                 "This map features a novel dataset from Global Fishing Watch and emLab that models global fishing vessel emissions by combining Automatic Identification System (AIS, which acts like GPS tracking for ships) with satellite-based Synthetic Aperture Radar (SAR, which functions like radar from space). Together, these technologies allow us to detect both broadcasted and non-broadcasted fishing activity."), 
                          
                          # Horizontal separator
                          tags$hr(),
                          
                          # ---- Sidebar Layer Controls ----------------------------------
                          
                          # ---- Controls for broadcasting emissions data ----
                          column(
                            width = 12,
                            
                            div(
                              style = "display: flex; align-items: center; gap: 8px;",
                              
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
                                      description = "The AIS-broadcasting layer is where Global Fishing Watch has classified apparent fishing effort using Automatic Identification System (AIS) data.",
                                      data_source = "Global Fishing Watch",
                                      learn_more = "https://globalfishingwatch.org/user-guide/#Activity%20-%20Fishing:~:text=methodology%20paper.-,Understanding%20apparent%20fishing%20effort%20using%20AIS%20and%20VMS%20data,-Automatic%20identification%20system"
                                    )
                                  ),
                                  value = TRUE,
                                  status = "info"
                                )
                              ) # END switch wrapper
                            ) # END outer flex row
                          ), # END column
                          
                          
                          # Hidden broadcasting legend ---
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
                                  description = "A flag State acts as a vessel's nationality and has jurisdiction over the ship's administrative operations. No matter where the vessel goes, its flag State is responsible for policing it. You can filter the broadcasted emissions for the flag or country resonsible for the vessel. These flag states were identified by Global Fishing Watch.",
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
                                materialSwitch(
                                  inputId = "show_non_broadcasting_input",
                                  label = tags$div(
                                    style = "display: flex; align-items: center; gap: 6px; font-size: 18px; font-weight: 400; color: #20404F; margin: 0;",
                                    
                                    # Label text
                                    tags$span("Non-Broadcasted Emissions"),
                                    
                                    # Info icon
                                    infoPopup(
                                      id = "non_broadcasting_popup",
                                      description = "This layer shows vessels detected using Synthetic Aperture Radar (SAR), a satellite-based system that captures images using microwave pulses, allowing detection in all weather and lighting conditions. Vessel positions are identified from Copernicus Sentinel-1 imagery using a combination of classical detection techniques and machine learning.",
                                      data_source = "Global Fishing Watch",
                                      learn_more = "https://globalfishingwatch.org/user-guide/#Radar%20detections%20-%20Synthetic%20aperture%20radar:~:text=Detections-,Radar%20detections%20%2D%20Synthetic%20aperture%20radar,-Synthetic%20aperture%20radar"
                                    )
                                  ),
                                  value = FALSE,
                                  status = "warning"
                                )
                              ) # END switch wrapper
                            ) # END outer flex row
                          ), # END column
                          
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
                                ),
                                
                                # Min/Max labels
                                tags$div(
                                  style = "display: flex; justify-content: space-between;
                 font-size: 15px; font-weight: regular; color: #053762;
                 margin-bottom: 10px; width: 100%;",
                                  tags$span("200"),
                                  textOutput("total_non_broadcasting", inline = TRUE)
                                )
                            )
                          ),
                          
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
                                    )
                                  ),
                                  value = FALSE,
                                  status = "info"
                                )
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
                                     left: -32px;
                                     width: 40px;
                                     height: 50px;
                                     display: flex;
                                     background-color: #f9f9f9;
                                     border: none;
                                     display: none;
                                     z-index: 1001"), # END actionButton for sidebar to close sidebar
                      
                  
                      # ---- Emissions Map --------------------------------------------
                      mapdeckOutput("emissions_map", height = "100%"),
                      useShinyjs(),
                      uiOutput("loading_ui"),
                      
                      # ---- Year Slider ----
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
                                      description = "Data displayed in the map grid is aggregated by year. Use the slider to select a year or click the play button to animate emissions trends over time.",
                                      data_source = NULL,
                                      learn_more = NULL),
                                    
                                    sliderInput("year_slider_input_map",
                                                NULL,
                                                min = year_min,
                                                max = year_max,
                                                value = year_max,
                                                step = 1,
                                                sep = "",
                                                width = "100%",
                                                ticks = TRUE,
                                                animate = animationOptions(interval = 3000, loop = FALSE)) # END sliderInput (year)
                      ) # END absolutePanel - year
                  ) # END map container
  ), # END emissions map tab
  
  
  # ------------------------------------------------------------------------------------------------------------------
  # ---- Compare Seafood Emissions Page ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------------
  
shiny::tabPanel("Compare Seafood Emissions",
  
  useShinyjs(),
  tags$style(HTML("
  /* Darken the circle border */
  .pretty.p-default input[type='radio'] ~ .state label:before {
    border: 1px solid black !important;
  }

  /* Make the selected dot black */
  .pretty.p-default input[type='radio']:checked ~ .state label:after {
    background-color: #08C4E5 !important;
  }

  /* Add spacing between inline radio buttons */
  .pretty.p-default {
    margin-bottom: 8px;
    margin-top: 8px;
  }
  
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
  
  
  
  # Main Content ----------------------------------------------------------------
  div(id = "plot_main_content",
      div(
        style = "scrollbar-width: auto; min-height: 100px;",
        
        # Header Row ----
        fluidRow(
          tags$p("Compare Seafood Emissions", style = "color: white; font-size: 30px; font-weight: bold; white-space: normal; padding-left: 50px; padding-right: 50px; text-align: center;"),
          tags$p("Explore our dataset, which links fishing vessel emissions from Global Fishing Watch with catch data from the Food and Agriculture Organization (FAO).", style = "color: white; font-size: 18px; font-weight: bold; white-space: normal; padding-left: 50px; padding-right: 50px; text-align: center;")
        ),
        
        fluidRow(
          div(
            style = "display: flex; justify-content: center; align-items: center; gap: 5px; flex-wrap: wrap;",
            id = "learn_more_link2",
            tags$p("Click here", style = "font-weight: 600; color: white; cursor: pointer; text-decoration: underline; display: flex; flex-wrap: wrap;"),
            tags$p("to learn more about the data and its limitations.", style = "font-weight: 400; color: white; cursor: pointer; display: flex; flex-wrap: wrap;")
          )
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "text-align: center;",
                     div(style = "display: inline-block; margin: 10px;",
                         actionButton("compare_species_input",
                                      tagList(icon("fish", style = "margin-right: 8px;"), "Compare Species"),
                                      class = "btn btn-lg"
                         )
                     ),
                     div(style = "display: inline-block; margin: 10px;",
                         actionButton("compare_countries_input",
                                      tagList(icon("earth-americas", style = "margin-right: 8px;"), "Compare Countries"),
                                      class = "btn btn-lg"
                         )
                     ),
                     div(style = "display: inline-block; margin: 10px;",
                         actionButton("select_country_input",
                                      tagList(icon("flag", style = "margin-right: 8px;"), "Select a Country"),
                                      class = "btn btn-lg"
                         )
                     )
                 )
          )
        ),
        
        shinyjs::hidden(
          div(id = "country_select_plot_input",
              fluidRow(
                column(width = 12,
                       div(style = "text-align: center;",
                           div(style = "display: inline-block; margin-right: 20px;",
                               pickerInput("selected_country_input", "Select a Country:", choices = c("Select a country" = "", sort(unique(species_data$country_name))), selected = NULL, options = list(`live-search` = TRUE, `noneSelectedText` = "All Countries"))
                           ),
                           div(style = "display: inline-block;",
                               tags$h4(textOutput("selected_country_total"), style = "color: white; font-weight: bold; margin-top: 25px;")
                           )
                       )
                )
              )
          )
        ),
        
        fluidRow(
          column(width = 12,
                 div(style = "background-color:#0B2232; margin: 30px 20px; overflow: visible !important;",
                     div(id = "country_plot",
                         tags$h4("Top Emitting Fishing Fleets", style = "color: #DA8D03; font-size: 25px; font-weight: bold;"),
                         tags$h4("Annual CO₂ Emissions", style = "color: white; font-size: 25px;"),
                         div(style = "min-width: 900px; min-height: 300px;",
                             plotOutput("country_plot_output", height = "50vh", width = "100%") |> withSpinner(type = 4, color = '#08C4E5')
                         )
                     ),
                     shinyjs::hidden(
                       div(id = "isscaap_plot",
                           tags$h4("Top Emitting Species Groups", style = "color: #DA8D03; font-size: 25px; font-weight: bold;"),
                           tags$h4("Annual CO₂ Emissions", style = "color: white; font-size: 25px;"),
                           div(style = "min-width: 900px; min-height: 300px;",
                               plotOutput("isscaap_plot_output", height = "50vh", width = "100%") |> withSpinner(type = 4, color = '#08C4E5')
                           )
                       )
                     ),
                     div(id = "species_bar_plot_wrapper",
                         uiOutput("dynamic_country_header"),
                         shinyjs::hidden(
                           div(id = "species_bar_plot",
                               div(style = "min-width: 900px; min-height: 300px;",
                                   plotOutput("species_bar_plot_output", height = "50vh", width = "100%") |> withSpinner(type = 4, color = '#08C4E5')
                               )
                           )
                         )
                     )
                 )
          )
        )
      )
  ),
  
  fluidRow(
    # ---- Controls Row -----------------------------------------------------
    div(style = "display: flex; justify-content: center; flex-wrap: wrap;",
        
        # ---- Plot Unit Toggle -----------------------------------------------
        div(style = "z-index: 1000;
              background-color: rgba(249, 249, 249, 0.9);
              padding: 8px 16px;
              border-radius: 8px;
              width: 250px;
              margin: 10px;",
            
            tags$span("Select Plot Unit"),
            infoPopup(
              id = "plot_unit_popup",
              description = "Update this....",
              data_source = NULL,
              learn_more = NULL
            ),
            
            prettyRadioButtons(
              inputId = "unit_plot_toggle_input",
              label = NULL,
              choices = c("Total Emissions" = "total", "Emissions Per Unit Catch" = "per_unit"),
              selected = "total",
              inline = FALSE,
              status = "primary"
            )
        ), # END plot unit box
        
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
              description = "Data displayed in the map grid is aggregated by year. Use the slider to select a year or click the play button to animate emissions trends over time.",
              data_source = NULL,
              learn_more = NULL
            ),
            
            sliderInput("year_slider_input_plot",
                        NULL,
                        min = 2016,
                        max = 2022,
                        value = 2022,
                        step = 1,
                        sep = "",
                        width = "100%",
                        ticks = TRUE)
        ) # END year box
    )
    
  ) # END Fluid Row
  
  ), # END tabPanel
  

  # ------------------------------------------------------------------------------------------------------------------
  # ---- Usage Guide Page --------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------------
  
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
                    div(style = "padding: 30px; color: white;",
                        h4(strong("Background")),
                        tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                               "Global fisheries are heavily reliant on fossil fuels, contributing significantly to the rise in global greenhouse gas emissions driving climate change. While satellite technology is commonly used to monitor land-based emissions (and ocean-based emissions of shipping vessels), studies primarily estimating ocean-based emissions in the fishing sector remain limited. In collaboration with the Environmental Markets Lab (emLab) and Global Fishing Watch, this project leverages novel, high-resolution satellite-based datasets to provide precise insights into the emissions associated with global fisheries. We develop a reproducible, extensible, and open-source data processing pipeline to connect emissions data with seafood production data, along with an interactive dashboard to explore the resulting dataset.",
                               tags$br(),
                               tags$a(href = "https://github.com/Seamissions", target = "_blank", "https://github.com/Seamissions"))  
                    ) # END div
             )
             
           ), # END fluidrow 'background'
           
           fluidRow(
             column(12,
                    div(
                      style = "padding: 30px; color: white;",
                      h4(strong("Who we are")),
                      
                      tags$p(
                        style = "font-weight: normal; color: white; margin-bottom: 20px;",
                        "We are a team of environmental data scientists working to quantify and demystify the emissions contributed through commercial fishing fleets."
                      ),
                      
                      tags$p(
                        tags$strong("Carmen Hoyt:"), " ",
                        tags$a(href = "https://bren.ucsb.edu/people/carmen-hoyt", target = "_blank", "Bren Profile"), " | ",
                        tags$a(href = "https://github.com/orgs/Seamissions/people/ceh58", target = "_blank", "Github"), " | ",
                        tags$a(href = "https://www.linkedin.com/in/carmen-hoyt-952272153/", target = "_blank", "LinkedIn"), " | ",
                        tags$a(href = "https://ceh58.github.io/", target = "_blank", "Website")
                      ),
                      
                      tags$p(
                        tags$strong("Josh Mull:"), " ",
                        tags$a(href = "https://bren.ucsb.edu/people/joshua-mull", target = "_blank", "Bren Profile"), " | ",
                        tags$a(href = "https://github.com/orgs/Seamissions/people/llumj", target = "_blank", "Github"), " | ",
                        tags$a(href = "https://www.linkedin.com/in/joshua-mull-046a7832b/", target = "_blank", "LinkedIn"), " | ",
                        tags$a(href = "https://llumj.github.io/", target = "_blank", "Website")
                      ),
                      
                      tags$p(
                        tags$strong("Nicole Pepper:"), " ",
                        tags$a(href = "https://bren.ucsb.edu/people/nicole-pepper", target = "_blank", "Bren Profile"), " | ",
                        tags$a(href = "https://github.com/orgs/Seamissions/people/nicolelpepper", target = "_blank", "Github"), " | ",
                        tags$a(href = "https://www.linkedin.com/in/nicole-pepper/", target = "_blank", "LinkedIn"), " | ",
                        tags$a(href = "https://nicolelpepper.github.io/", target = "_blank", "Website")
                      ),
                      
                      tags$p(
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
                      style = "background-color: white; padding: 30px; color: #0B2232;",
                      
                      h4(strong("Our Partners")),
                      
                      # emLab section
                      tags$img(src = "images/logos/emlab-logo-color.png", style = "max-height: 75px; margin-bottom: 30px; margin-top: 25px;"),
                      
                      tags$h5(tags$u("Environmental Markets Lab (emLab)")),
                      tags$p(
                        style = "font-weight: normal; color: #0B2232; margin-bottom: 15px;",
                        "A think-and-do tank for market-based approaches to environmental challenges. They are an interdisciplinary team of scientists based at the University of California Santa Barbara that conducts cutting-edge, data-driven research on the power, limitations, and design of market-based approaches to tackle the world's most pressing environmental problems."
                      ),
                      tags$p(
                        style = "font-weight: normal; color: #0B2232; margin-bottom: 20px;",
                        "In collaboration with implementing partners, they aim to better align environmental objectives and economic incentives in support of sustainable livelihoods and a resilient planet."
                      ),

                      # Global Fishing Watch section
                      tags$img(src = "images/logos/gfw-logo-color.png", style = "max-height: 75px; margin-bottom: 30px; margin-top: 25px;"),
                      
                      tags$h5(tags$u("Global Fishing Watch")),
                      tags$p(
                        style = "font-weight: normal; color: #0B2232; margin-bottom: 15px;",
                        "Global Fishing Watch seeks to advance ocean governance through increased transparency of human activity at sea, enabling scientific research and driving transformation in ocean management by creating and publicly sharing map visualizations, data, and analysis tools."
                      ),
                      tags$p(
                        style = "font-weight: normal; color: #0B2232; margin-bottom: 20px;",
                        "Global Fishing Watch was founded in 2015 through a collaboration between three partners: ",
                        tags$strong("Oceana"), ", an international ocean conservation organization; ",
                        tags$strong("SkyTruth"), ", a technology firm that uses satellite imagery and data to protect the environment; and ",
                        tags$strong("Google"), ", whose tools and contributions help process big data. ",
                        "In June 2017, Global Fishing Watch was established as an independent, international nonprofit organization."
                      )
                    ) # END div
             )
             
           ), # END fluidrow 'About our partners'
           
           fluidRow(
             column(12,
                    div(style = "padding: 30px; color: white;",
                        h4(strong("Methods")),
                        tags$p(
                          style = "font-weight: normal; color: white; margin: 30px;",
                          "Our team developed a structured process that could handle large amounts of data provided by emLab. Our approach was to use emissions data from vessels broadcasting their activity to determine the emissions for the vessels that are non-broadcasting. We then used catch data from the FAO to create a more complete picture of fishing-related emissions worldwide.
We then combined emissions data from vessels with visible tracking signals (“broadcasting”) and those without (“non-broadcasting”). Each fishing event was linked to a date, location, and national flag, and we calculated emissions for each grid cell on a global map.
Next, we aligned those emissions with FAO fishing regions. When a single grid cell overlapped multiple regions, we divided the emissions proportionally based on area to ensure fair and accurate regional estimates.
To make the process efficient and reliable, we built a system that could detect changes and automatically update only the affected parts of the analysis.
Finally, we double-checked our results to ensure that all emissions were accounted for, allowing for minimal rounding error. This method gives us a transparent, reproducible way to estimate fishing-related emissions at a global scale, helping to understand the climate impact of seafood production.
Information on how GFW and emLab produced the data we used in our model can be found below."
                        ),
                        tags$br(),
                        tags$a(href = "https://emlab-ucsb.github.io/ocean-ghg/", target = "_blank", "https://emlab-ucsb.github.io/ocean-ghg/")
                    ) # END div
             )
             
           ), # END fluidrow 'Methods'
           
           fluidRow(
             column(12,
                    div(style = "padding: 30px; color: white;",
                        h4(strong("Vessels Emissions Data Usage Guide")),
                        tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                               tags$a(href = "https://globalfishingwatch.org/tutorials/", target = "_blank", "https://globalfishingwatch.org/tutorials/")
                        )  
                    ) # END div
             )
             
           ), # END fluidrow 'Usage Guide'
           
           fluidRow(
             column(12,
                    div(
                      style = "padding: 30px; color: white;",
                      
                      tags$h4(tags$strong("Key Considerations")),
                      
                      fluidRow(
                        column(
                          width = 12,
                          tags$p(
                            style = "font-weight: normal; color: white; margin-bottom: 20px;",
                            "The dashboard is intended as an educational and exploratory tool, not an official regulatory product. All results should be interpreted with care due to several key limitations:"
                          )
                        )
                      ),
                      
                      tags$ul(
                        tags$li(
                          tags$u("Uncertainty in emissions estimates"),
                          " – particularly from the reattribution of emissions from non-broadcasting vessels to broadcasting vessels, a necessary step to assign emissions to flag states and align with FAO-reported catch data."
                        ),
                        tags$li(
                          tags$u("Ambiguity in catch location and reporting"),
                          " – stemming from underreporting and inconsistencies between where fish were actually caught, the vessel’s flag state, and the country where landings were reported to the FAO."
                        ),
                        tags$li(
                          tags$u("Dependence on harmonized datasets with different formats and assumptions"),
                          " – including the integration of FAO catch records and GFW’s fishing vessel emissions dataset, which originate from distinct sources with differing scopes, structures, and assumptions."
                        )
                      )
                    ) # END div
             )
             
           ), # END fluidrow 'Key Considerations'
           
           fluidRow(
             column(12,
                    div(
                      style = "padding: 30px; color: white;",
                      
                      tags$h4(tags$strong("Glossary")),
                      
                      tags$ul(
                        tags$li(
                          tags$u("AIS (Automatic Identification System)"), tags$br(),
                          "A satellite-based tracking system used by large vessels to broadcast their location and movement. It helps monitor shipping activity, but many fishing vessels do not use it."
                        ),
                        tags$li(
                          tags$u("Broadcasting Vessels"), tags$br(),
                          "Fishing vessels that transmit their location via AIS, making their movements and emissions trackable."
                        ),
                        tags$li(
                          tags$u("FAO (Food and Agriculture Organization)"), tags$br(),
                          "A UN agency that collects official fisheries and aquaculture data from countries around the world."
                        ),
                        tags$li(
                          tags$u("FAO Catch Data"), tags$br(),
                          "Country-reported records of fish and seafood harvested, often used as a global baseline for fisheries research."
                        ),
                        tags$li(
                          tags$u("Flag (of a vessel)"), tags$br(),
                          "The country under which a fishing vessel is registered. Some vessels use “flags of convenience”, flags associated with different countries to flout regulation or take advantage of favorable legislation."
                        ),
                        tags$li(
                          tags$u("Flag of Convenience"), tags$br(),
                          "When a vessel registers under a country different from its home country, often to take advantage of looser regulations."
                        ),
                        tags$li(
                          tags$u("GHG (Greenhouse Gas)"), tags$br(),
                          "Gases that trap heat in the atmosphere and contribute to climate change. This project tracks emissions like CO₂, CH₄, and N₂O."
                        ),
                        tags$li(
                          tags$u("Global Fishing Watch (GFW)"), tags$br(),
                          "A nonprofit that uses satellite and machine learning data to monitor fishing activities globally."
                        ),
                        tags$li(
                          tags$u("ISSCAAP (International Standard Statistical Classification of Aquatic Animals and Plants)"), tags$br(),
                          "A classification system used by the FAO to group aquatic species for reporting purposes."
                        ),
                        tags$li(
                          tags$u("Non-Broadcasting Vessels"), tags$br(),
                          "Fishing vessels that do not transmit AIS signals. Their emissions are harder to track and are estimated using satellite radar."
                        ),
                        tags$li(
                          tags$u("Pipeline"), tags$br(),
                          "A series of connected data processing steps that clean, merge, and analyze raw data. The emissions pipeline in this project combines emissions and catch data."
                        ),
                        tags$li(
                          tags$u("SAU (Sea Around Us)"), tags$br(),
                          "A project that reconstructs missing or underreported fisheries data to give a more complete picture of global fish harvests."
                        )
                      )
                    ) # END div
             )
           ) # END fluidrow 'Glossary'
           
  ) # END tabPanel (Learn More Page)
  
) # END navbarPage

