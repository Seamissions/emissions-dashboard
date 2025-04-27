# ---- ui.R ---------------------------------------------------------
source("theme.R")

ui <- navbarPage(
  title = "Seamissions Explorer",
  windowTitle = "Seamissions Explorer",
  id = "navbarPage",
  theme = seamissions_theme,
  
  # ---- Home Page ----
  tabPanel("Home",
           
           # ---- Hero Section ----
           div(
             style = "
        background-image: url('images/home-image.jpg');
        background-size: cover;
        background-position: center;
        padding: 100px 0;
        text-align: center;
        color: #e8fffd;
      ",
             h1("Seamissions Global Fishing Emissions Explorer"),
             h4("Connecting data to action for sustainable oceans.")
           ), # END hero section
           
           # ---- Teaser Sections ----
           div(style = "margin-top: 10px;",
               fluidRow(
                 # Emissions Map Teaser
                 column(
                   width = 6,
                   div(
                     style = "position: relative; height: 300px; background-color: #031021; border-radius: 8px; overflow: hidden; margin-bottom: 20px;",
                     img(
                       src = "images/map-preview.png",
                       style = "width: 100%; height: 100%; object-fit: cover; opacity: 0.4;"
                     ),
                     div(
                       style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                       actionButton("explore_map", "Fishing Vessel Emissions Map", class = "btn-primary btn-lg")
                     )
                   )
                 ),
                 # Seafood Emission Explorer Teaser
                 column(
                   width = 6,
                   div(
                     style = "position: relative; height: 300px; background-color: #031021; border-radius: 8px; overflow: hidden; margin-bottom: 20px;",
                     img(
                       src = "images/map-preview.png",
                       style = "width: 100%; height: 100%; object-fit: cover; opacity: 0.4;"
                     ),
                     div(
                       style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                       actionButton("explore_seafood", "Seafood Emissions Explorer", class = "btn-primary btn-lg")
                     )
                   )
                 )
               )
           ) # END teaser sections
  ), # END home tab
  
  # ---- Emissions Map Page ----
  tabPanel("Emissions Map",
           
           useShinyjs(), # Initialize shinyjs
           
           # ---- Map Container ----
           div(style = "position: relative; height: 90vh;",
               
               # ---- Sidebar Panel ----
               div(id = "sidebar-panel",
                   style = "position: absolute;
                 top: 0;
                 left: 0;
                 height: 100%;
                 width: 350px;
                 background-color: #f9f9f9;
                 padding: 15px;
                 border-right: 0px solid #ccc;
                 z-index: 1001;",
                   
                   # ---- Toggle Button Inside Sidebar ----
                   actionButton("toggle_sidebar",
                                label = NULL,
                                icon = icon("angle-left", style = "font-size: 20px;"),
                                style = "position: absolute;
                   top: 40%;
                   right: -40px;
                   width: 0%;
                   background-color: #f9f9f9;
                   border: none;"
                   ),
                   
                   # ---- Sidebar Controls ----
                   materialSwitch("show_all_countries", "Broadcasting Emissions", value = TRUE, status = "info"),
                   hidden(pickerInput("country_select", "Filter To A Country (Flag)", choices = country_flags, multiple = TRUE,
                                      options = list(`max-options` = 1, `max-options-text` = "You can only select 1 country",
                                                     `actions-box` = TRUE, `live-search` = TRUE))),
                   materialSwitch("show_non_broadcasting", "Non-Broadcasting Emissions", value = FALSE, status = "primary"),
                   materialSwitch("show_fao_zones", "FAO Zones", value = FALSE, status = "info")
               ), # END sidebar panel
               
               actionButton("toggle_sidebar_outside",
                            label = NULL,
                            icon = icon("angle-right", style = "font-size: 20px;"),
                            style = "position: absolute;
                 top: 40%;
                 left: -20px;
                 width: 0%;
                 background-color: #f9f9f9;
                 border: none;
                 display: none;
                 z-index: 1001"
               ),
               
               # ---- Emissions Map ----
               mapdeckOutput("emissions_map", height = "100%"),
               uiOutput("loading_ui"),
               
               # ---- Year Slider ----
               absolutePanel(bottom = 30,
                             right = 5,
                             left = "60%",
                             style = "z-index: 1000;
                 background-color: rgba(255,255,255,0.8);
                 padding: 8px;
                 border-radius: 8px;",
                             sliderInput("year_slider_input",
                                         "Select Year",
                                         min = year_min,
                                         max = year_max,
                                         value = year_max,
                                         step = 1,
                                         sep = "",
                                         width = "100%",
                                         ticks = TRUE,
                                         animate = animationOptions(interval = 3000, loop = FALSE)) # END sliderInput-year
               ) # END absolutePanel - year
           ) # END map container
  ), # END emissions map tab
  
  # ---- About Page ----
  tabPanel("About",
           # ---- Hero Section ----
           div(
             style = "
        background-image: url('images/home-image.jpg');
        background-size: cover;
        background-position: center;
        padding: 100px 0;
        text-align: center;
        color: #e8fffd;
      ",
             h1("Seamissions Global Fishing Emissions Explorer"),
             h4("Connecting data to action for sustainable oceans.")
           ) # END hero section
  ) # END about tab
) # END navbarPage



