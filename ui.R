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
             h1("Seamissions Global Fishing Emissions Explorer"),
             h4("This is where the short overview goes.")
           ), # END hero section
           
           # ---- Teaser Sections ----
           div(style = "margin-top: 10px;",
               fluidRow(
                 # Emissions Map Teaser
                 column(
                   width = 6,
                   div(
                     style = "position: relative;
                     height: 300px;
                     background-color:#031021;
                     border-radius: 8px;
                     overflow: hidden;
                     margin-bottom: 20px;",
                     
                     img(src = "images/map-preview.png",
                       style = "width: 100%;
                       height: 100%;
                       object-fit: cover;
                       opacity: 0.4;"), # END img
                     div(
                       style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                       actionButton("explore_map", "Fishing Vessel Emissions Map", class = "btn-primary btn-lg")
                     )
                   )
                 ),
                 # Seafood Emission Explorer Tool Overview
                 column(
                   width = 6,
                   div(
                     style = "position: relative; height: 300px; background-color: #031021; border-radius: 8px; overflow: hidden; margin-bottom: 20px;",
                    
                      img(src = "images/map-preview.png",
                       style = "width: 100%;
                       height: 100%;
                       object-fit: cover;
                       opacity: 0.4;"), # END img
                     
                     div(style = "position: absolute;
                         top: 50%;
                         left: 50%;
                         transform: translate(-50%, -50%);",
                         
                       actionButton("explore_seafood", "Seafood Emissions Explorer", class = "btn-primary btn-lg")) # END divider
                   )
                 )
               )
           ) # END teaser sections
  ), # END home tab
  
  # ---- Emissions Map Page ----
  tabPanel("Emissions Map",
           
           useShinyjs(), # Initialize shinyjs

           # --- Top row with title and calculations ----
           
           # Title
           fluidRow(
             
             style = "margin-bottom: 10px;",
             
             column(width = 8,
                    # Title
                    tags$h3(style = "font-size: 24px; font-weight: bold; color: white; margin-bottom: 20px;", 
                            "Fishing Vessel Emissions Map")
             ),
             
             column(width = 2,
                    div(
                      style = "background-color: rgba(255, 255, 255, 0.9); padding: 8px 10px; border-radius: 8px; 
             display: flex; justify-content: space-between; align-items: center; height: auto;",
                      
                      # Icon faded (left)
                      tags$div(
                        icon("smog"),
                        style = "font-size: 30px; color: rgba(5, 55, 98, 0.2); margin-right: 8px;"
                      ),
                      
                      # Number and label (right)
                      tags$div(style = "text-align: right; line-height: 1.1;",
                               tags$div(
                                 textOutput("total_broadcasting"),
                                 style = "font-size: 20px; font-weight: bold; color: #053762; margin-bottom: 2px;"), # END total emissions calculation
                               tags$div(
                                 "Total Broadcasting Emissions",
                                 style = "font-size: 11px; font-weight: bold; color: #053762;") # END title for value box
                      )# END number and text for value box
                    
                    )
             ),
             
             # Total non-broadcasting emissions box (matched style)
             column(width = 2,
                    div(
                      style = "background-color: rgba(255, 255, 255, 0.9); padding: 8px 10px; border-radius: 8px; 
               display: flex; justify-content: space-between; align-items: center; height: auto;",
                      
                      # Icon faded (left)
                      tags$div(
                        icon("ship"),
                        style = "font-size: 30px; color: rgba(125, 54, 80, 0.2); margin-right: 8px;"  # softer pink tone
                      ),
                      
                      # Number and label (right)
                      tags$div(style = "text-align: right; line-height: 1.1;",
                               tags$div(
                                 textOutput("total_non_broadcasting"),
                                 style = "font-size: 20px; font-weight: bold; color: #7D3650; margin-bottom: 2px;"
                               ),
                               tags$div(
                                 "Total Non-Broadcasting Emissions",
                                 style = "font-size: 11px; font-weight: bold; color: #7D3650;"
                               )
                      )
  
                    )
             )
           ), # END fluidRow
           
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
                   materialSwitch("show_all_countries",
                                  label = tags$div(style = "font-size: 18px;
                                                   font-weight: bold;",
                                                   "Broadcasting Emissions"),
                                  value = TRUE,
                                  status = "info"),
                   
                   hidden(div(id = "broadcasting_legend",
                       tags$div(style = "background: linear-gradient(to right,#015661, #03C7E8);
                                height: 20px;
                                width: 70%;
                                border: 1px solid #ccc;"),
                       tags$p("Low to High Emissions")),# END div
                       
                       pickerInput("country_select",
                                   "Filter To A Country (Flag)",
                                   choices = country_flags,
                                   multiple = FALSE,
                                   options = list(`live-search` = TRUE,
                                                  container = NULL)) # END pickerInput
                   
                   ), # END hidden
                   
                   tags$hr(),
                   
                   materialSwitch("show_non_broadcasting",
                                  label = tags$div(style = "font-size: 18px;
                                                   font-weight: bold;",
                                                   "Non-Broadcasting Emissions"),
                                  value = FALSE,
                                  status = "warning"), # END materialSwitch
                   
                   hidden(div(id = "non_broadcasting_legend",
                              tags$div(style = "background: linear-gradient(to right, #805F14, #f9b928);
                                       height: 20px;
                                       width: 70%;
                                       border: 1px solid #ccc;"),
                              
                              tags$p("Low to High Emissions")) # END divider
                          ), # END hidden
                   
                   tags$hr(),
                   
                   materialSwitch("show_fao_zones",
                                  label = tags$div(style = "font-size: 18px;
                                                   font-weight: bold;",
                                                   "FAO Major Fishing Zones"),
                                  value = FALSE,
                                  status = "info")
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
                                         animate = animationOptions(interval = 3000, loop = FALSE)) # END sliderInput (year)
               ) # END absolutePanel - year
           ) # END map container
  ), # END emissions map tab
  
# ---- Seafood Emissions Explorer Page ----
tabPanel("Seafood Emissions Explorer",
         
         # Title
         tags$h3(style = "font-size: 24px; font-weight: bold; color: #f9f9f9; margin-bottom: 20px;", 
                 "Seafood Emissions Explorer"), 
         
         # First Row ----
         fluidRow(
           
           # Compare species button ---- 
           column(width = 2,
                  actionButton(
                    "explore_map",
                    tagList(
                      icon("globe", style = "margin-right: 8px;"),
                      "Compare Species"
                    ),
                    class = "btn-primary btn-lg"
                  )
           ),
           
           
           # Compare countries button ---- 
           column(width = 2,
                  actionButton(
                    "explore_map",
                    tagList(
                      icon("globe", style = "margin-right: 8px;"),
                      "Compare Countries"
                    ),
                    class = "btn-primary btn-lg"
                  )
           ),
           
           # Compare gear button ---- 
           column(width = 2,
                  actionButton(
                    "explore_map",
                    tagList(
                      icon("gears", style = "margin-right: 8px;"),
                      "Compare Fishing Gear"
                    ),
                    class = "btn-primary btn-lg"
                  )
           )
         ), # END first fluidRow
         
         # Second Row with ggplot ----
         fluidRow(
           column(width = 12,
                  div(style = "background-color: #053762; padding: 10px; height: 60vh; min-height: 300px;",
                      plotOutput("example_barplot", height = "100%", fill = TRUE)
                  
                  ), # END fluidRow
           ),
           
         ) # END second fluidRow
         
), # END tabPanel (Seafood Emissions Explorer Page)


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
  ) # END tabPanel (About Page)

) # END navbarPage



