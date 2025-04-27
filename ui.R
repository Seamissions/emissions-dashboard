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
        background-size: cover;
        background-position: center;
        padding: 100px 0;
        text-align: center;
        color: #e8fffd;
      ",
             h1("Welcome to the Seamissions Global Fishing Emissions Explorer"),
             h5("Check out our tools to look at global fishing activity and seafood-related emissions."),
           ), # END hero section
           
           # ---- Tools Overview Sections ----
           div(style = "margin-top: 5px;",
               fluidRow(
                 # Emissions Map Tool
                 column(
                   width = 6,
                   div(
                     style = "position: relative;
                     height: 300px;
                     background-color:#031021;
                     border-radius: 8px;
                     overflow: hidden;
                     margin-bottom: 20px;",
                     
                     img(
                       src = "images/map-preview.png",
                       style = "width: 100%;
                       height: 100%;
                       object-fit: cover;
                       opacity: 0.4;"
                     ),
                     div(
                       style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                       actionButton("explore_map",
                                    label = tagList(icon("ship"),
                                                    "Fishing Vessel Emissions Map"),
                                    class = "btn-primary btn-lg")
                     )
                   )
                 ),
                 # Seafood Emission Explorer Tool
                 column(
                   width = 6,
                   div(
                     style = "position: relative; height: 300px; background-color: #031021; border-radius: 8px; overflow: hidden; margin-bottom: 20px;",
                     img(
                       src = "images/map-preview.png",
                       style = "width: 100%; height: 100%; object-fit: cover; opacity: 0.6;"
                     ),
                     div(
                       style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                       actionButton("explore_seafood",
                                    label = tagList(icon("fish"),
                                                    "Seafood Emissions Explorer"),
                                    class = "btn-primary btn-lg")
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
                 width: px;
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
                   div(
                     style = "font-size: 18px;
                     font-weight: bold;
                     width: 100%;",
                     materialSwitch("show_all_countries",
                                    "AIS Broadcasting Emissions",
                                    value = TRUE,
                                    status = "info")
                   ),
                   
                   hidden(div(id = "broadcasting_legend",
                              tags$div(style = "background: linear-gradient(to right, #20404F, #DAF3FF);
                                       height: 20px;
                                       border: 1px solid #ccc;
                                       width: 60%;"),
                              tags$p("REPLACE WITH VALUES")
                   )),
                   
                   
                   hidden(pickerInput("country_select",
                                      "Filter To A Country (Flag)",
                                      choices = country_flags,
                                      multiple = FALSE,
                                      options = list(`live-search` = TRUE,
                                                     container = NULL)
                   )),
                   
                   tags$hr(),
                   
                   div(
                     style = "font-size: 18px;
                     font-weight: bold;
                     width: 100%;",
                     materialSwitch("show_non_broadcasting",
                                    "Non-Broadcasting Emissions",
                                    value = FALSE,
                                    status = "warning")
                   ),
                   
                   hidden(div(id = "non_broadcasting_legend",
                              tags$div(style = "background: linear-gradient(to right, #7D3650, #FFECE5);
                                       height: 20px;
                                       border: 1px solid #ccc;
                                       width: 60%;"),
                              tags$p("REPLACE WITH VALUES")
                   )),
                   
                   tags$hr(),
                   
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
                                         animate = animationOptions(interval = 3000, loop = FALSE))
               ) # END absolutePanel - year
           ) # END map container
  ), # END emissions map tab
  
  # ---- Seafood Emissions Page ----
  tabPanel("Seafood Emissions",
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
  ), # END seafood emissions tab
  
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


