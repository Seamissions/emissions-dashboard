# --------------------------------------------------------------------------------
# ---- ui.R --------------------------------------------------------------------
# --------------------------------------------------------------------------------
  
  
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
           
           # ---- Teaser Sections ----------------------------------------------
           div(
             style = "margin-top: 10px;",
             fluidRow(
               
               # Emissions Map Teaser ------------------------------------------
               column(
                 width = 6,
                 div(
                   style = "height: 300px;
                 background-color:#031021;
                 border-radius: 8px;
                 overflow: hidden;
                 display: flex;
                 flex-direction: column;
                 justify-content: space-between;
                 color: white;
                 font-family: sans-serif;",
                   
                   # Top half: image + button
                   div(
                     style = "position: relative;
                   height: 50%;
                   width: 100%;",
                     img(
                       src = "images/map-preview.png",
                       style = "position: absolute;
                     top: 0; left: 0;
                     width: 100%;
                     height: 100%;
                     object-fit: cover;
                     opacity: 0.4;
                     z-index: 1;"
                     ),
                     div(
                       style = "position: relative;
                     z-index: 2;
                     height: 100%;
                     display: flex;
                     justify-content: center;
                     align-items: center;",
                       actionButton("explore_map", "Explore the Map", class = "btn-primary btn-lg")
                     )
                   ), # END top half
                   
                   # Bottom half: description
                   div(
                     style = "padding: 12px; height: 50%;",
                     tags$p("Explore where fishing activity is most concentrated.",
                            style = "margin: 0; font-size: 16px; color: white;")
                   ) # END bottom half
                 ) # END outer div
               ), # END column (Emissions Map Teaser)
               
               # Seafood Emission Explorer Tool Preview ------------------------
               column(
                 width = 6,
                 div(
                   style = "height: 300px;
                 background-color:#031021;
                 border-radius: 8px;
                 overflow: hidden;
                 display: flex;
                 flex-direction: column;
                 justify-content: space-between;
                 color: white;
                 font-family: sans-serif;",
                   
                   # Top half: image + button
                   div(
                     style = "position: relative;
                   height: 50%;
                   width: 100%;",
                     img(
                       src = "images/map-preview.png",
                       style = "position: absolute;
                     top: 0; left: 0;
                     width: 100%;
                     height: 100%;
                     object-fit: cover;
                     opacity: 0.4;
                     z-index: 1;"
                     ),
                     div(
                       style = "position: relative;
                     z-index: 2;
                     height: 100%;
                     display: flex;
                     justify-content: center;
                     align-items: center;",
                       actionButton("explore_seafood", "Seafood Emissions Explorer", class = "btn-primary btn-lg")
                     )
                   ), # END top half
                   
                   # Bottom half: description
                   div(
                     style = "padding: 12px; height: 50%;",
                     tags$p("This is where the tool description goes.",
                            style = "margin: 0; font-size: 16px; color: white;")
                   ) # END bottom half
                 ) # END outer div
               ) # END column (Seafood Explorer Teaser)
               
             ) # END fluidRow
           ) # END div (Teaser Sections)
           
  ), # END home tab
  
  # ------------------------------------------------------------------------------------------------------------------
  # ---- Emissions Map Page ------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------------
  
  tabPanel("Emissions Map",
           
           useShinyjs(), # Initialize shinyjs

           
           # ---- Map Container ----
           div(style = "position: relative; height: 90vh;",
               
               # ---- Sidebar Panel --------------------------------------------
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
                                         border: none;"),
                   
                   # Map title
                   tags$h3(style = "font-size: 24px; font-weight: bold; color: #20404F; margin-bottom: 20px;", 
                           "Fishing Vessel Emissions"), 
                   
                   # ---- Sidebar Layer Controls -------------------------------
                   
                   # Controls for broadcasting emissions data
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
                       tags$div(
                         textOutput("total_broadcasting"),
                         style = "font-size: 15px; font-weight: bold; color: #053762; margin-bottom: 10px;")),# END div
                       
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
                              
                              tags$div(
                                textOutput("total_non_broadcasting"),
                                style = "font-size: 15px; font-weight: bold; color: #053762; margin-bottom: 10px;")) # END div
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
               
               
               # ---- Emissions Map --------------------------------------------
               mapdeckOutput("emissions_map", height = "100%"),
               uiOutput("loading_ui"),
               
                # ---- Year Slider ----
               absolutePanel(bottom = 30,
                             right = 8,
                             style = "z-index: 1000;
                                    background-color: rgba(255,255,255,0.8);
                                    padding: 8px;
                                    border-radius: 8px;
                                    width: 20%;",
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

# ------------------------------------------------------------------------------------------------------------------
# ---- Seafood Emissions Explorer Page -----------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

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
                    icon("fish", style = "margin-right: 8px;"),
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
         
         
), # END first fluidRow

# Second Row with ggplot ----
fluidRow(
  column(width = 12,
         div(style = "background-color: #053762; height: 60vh; min-height: 300px;  margin-top: 30px;  margin-bottom: 30px;  margin-left: 20px; margin-right: 20px;",
             plotOutput("example_barplot", height = "60vh", fill = TRUE) |> 
               withSpinner(type = 4, color = '#08C4E5') ) # END div
  ) # END column
), # END plot fluid Row

fluidRow(
  column(
    width = 12,
    div(
      style = "display: flex; justify-content: flex-end; align-items: center; padding-right: 20px; margin-top: 10px;",
      
      tags$div("Total Emissions", 
               style = "margin-right: 20px; color: white; font-weight: 500; font-size: 14px;"),
      
      div(
        style = "display: inline-block;",
        materialSwitch(
          inputId = "per_unit_plot_toggle",
          label = NULL,
          status = "info",
          right = TRUE,
          inline = TRUE
        )
      ),
      
      tags$div("Per Unit Catch", 
               style = "margin-left: 8px; margin-right: 100px; color: white; font-weight: 500; font-size: 14px;")
    )
  )
)
# END unit toggle fluidRow
         
), # END tabPanel (Seafood Emissions Explorer Page)

# ------------------------------------------------------------------------------------------------------------------
# ---- About Page --------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

  tabPanel("Methods",
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
  ) # END tabPanel (Methods Page)

) # END navbarPage



