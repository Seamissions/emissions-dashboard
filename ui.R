# ---- ui.R ---------------------------------------------------------

ui <- navbarPage(
  title = "Seamissions Explorer",
  windowTitle = "Seamissions Explorer",
  id = "navbarPage",  # Added navbarPage id
  
  # ---- home page ----
  tabPanel("Home",
           
           # ---- hero section ----
           div(style = "background-image: url('www/ocean_background.jpg');
                    background-size: cover; background-position: center;
                    padding: 150px 0; text-align: center; color: white;",
               h1("Illuminating Global Fishing Emissions"),
               h4("Connecting data to action for sustainable oceans."),
               actionButton("explore_map", "Explore Emissions Map", class = "btn-primary btn-lg")
           ), # END hero section
           
           # ---- intro section ----
           fluidRow(
             column(4, icon("globe", "fa-3x"), h3("Quantify"), p("Measure fishing vessel emissions worldwide.")),
             column(4, icon("map", "fa-3x"), h3("Visualize"), p("Interactive maps to explore spatial trends.")),
             column(4, icon("bullhorn", "fa-3x"), h3("Inform"), p("Support sustainable ocean policy with data-driven insights."))
           ), # END intro section
           
           # ---- map teaser ----
           div(style = "padding: 50px 0; text-align: center;",
               img(src = "www/map_preview.jpg", style = "width: 80%; max-width: 800px;"),
               p("Discover patterns and hotspots across global fishing fleets.")
           ), # END map teaser
           
           # ---- impact stats ----
           fluidRow(
             column(4, h2("1.5M km²"), p("Monitored ocean area")),
             column(4, h2("200K+ vessels"), p("Analyzed for emissions")),
             column(4, h2("20 years"), p("Of global data coverage"))
           ) # END impact stats
  ), # END home tab
  
  # ---- emissions map page with collapsible sidebar ----
  tabPanel("Emissions Map",
           
           useShinyjs(), # Initialize shinyjs
           
           # ---- Custom slider CSS ----
           tags$style(HTML("
             .slider-animate-container { display: none; }
             .irs-grid-text { display: none; }
             .irs-grid-pol { display: none; }
           ")),
           
           # ---- map container ------------------------------------------------
           div(style = "position: relative; height: 90vh;",
               
               # ---- sidebar panel (always rendered) ----
               div(id = "sidebar-panel",
                   style = "position: absolute; top: 0; left: 0; height: 100%; width: 300px;
                            background-color: rgba(255,255,255,0.95); padding: 15px;
                            border-right: 1px solid #ccc; z-index: 1000;",
                   
                   # ---- toggle button inside sidebar ----
                   actionButton("toggle_sidebar", label = NULL, icon = icon("angle-left"),
                                style = "position: absolute; top: 15px; right: 15px; background-color: #ffffffcc; border: none;"),
                   
                   
                   # ---- sidebar controls ----
                   materialSwitch("show_all_countries", "Broadcasting Emissions", value = TRUE, status = "info"),
                   hidden(sliderInput("opacity_all_countries", NULL, min = 0, max = 100, value = 0, step = 5, width = "150px")), # Broadcasting transparency slider
                   hidden(pickerInput("country_select", "Filter To A Country (Flag)", choices = country_flags, multiple = TRUE,
                                      options = list(`max-options` = 1, `max-options-text` = "You can only select 1 country", 
                                                     `actions-box` = TRUE, `live-search` = TRUE))), # END picker input # END picker input
                   materialSwitch("show_non_broadcasting", "Non-Broadcasting Emissions", value = FALSE, status = "primary"),
                   hidden(sliderInput("opacity_non_broadcasting", NULL, min = 0, max = 100, value = 0, step = 5, width = "150px")), # Non-broadcasting transparency slider
                   materialSwitch("show_fao_zones", "FAO Zones", value = FALSE, status = "info"),
                   hidden(sliderInput("opacity_fao_zones", NULL, min = 0, max = 100, value = 0, step = 5, width = "150px")) # FAO zones transparency slider
               ), # END sidebar panel
               
               # ---- toggle button outside when sidebar collapsed ----
               actionButton("toggle_sidebar_outside", label = NULL, icon = icon("angle-right"),
                            style = "position: absolute; top: 20px; left: 20px; z-index: 1001;
                                     background-color: #ffffffcc; border: none; display: none;"),
               
               # ---- emissions map ----
               mapdeckOutput("emissions_map", height = "100%"),
               uiOutput("loading_ui"),
               
               # ---- basemap toggle ----
               absolutePanel(top = 120, right = 20,
                             style = "z-index: 1000; background-color: rgba(255,255,255,0.8); padding: 8px; border-radius: 8px;",
                             switchInput("basemap_style", "Mode", value = TRUE, onLabel = "Dark", offLabel = "Light", 
                                         onStatus = "info", offStatus = "info", inline = TRUE)), # END basemap toggle
               
               # ---- year slider ----
               absolutePanel(top = 120, right = 180,
                             style = "z-index: 1000; background-color: rgba(255,255,255,0.8); padding: 8px; border-radius: 8px;",
                             sliderInput("year_slider_input", "Select Year", min = year_min, max = year_max, value = year_max, 
                                         step = 1, sep = "", animate = TRUE)) # END year slider
           ) # END map container
  ), # END emissions map tab
  
  # ---- about page ----
  tabPanel("About",
           h2("About Seamissions Explorer"),
           p("This project aims to...")) # END about tab
) # END navbarPage


