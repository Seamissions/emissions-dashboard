# ------------------------------------------------------------------------------------------------------------------
#     ui.R 
# ------------------------------------------------------------------------------------------------------------------

  
source("theme.R")

ui <- navbarPage(
  title = "Seamissions Explorer",
  windowTitle = "Seamissions Explorer",
  id = "navbarPage",
  theme = seamissions_theme,
  
  # ---- Home Page ----
  useShinyjs(),
  
  tabPanel("Home",
           
           # ---- Hero Section ----
           div(style = "background-size: cover;
                        background-image: url('images/dashboard-hero.png');
                        background-position: center;
                        height: 400px;
                        padding: 100px 0;
                        text-align: center;
                        color: #e8fffd;",
               h1("Seamissions Global Fishing Emissions Explorer"),
               h4("This is where the short overview goes.")
           ), # END div (hero section)
           
           tags$p(style = "font-weight: bold; color: white; margin: 40px;", 
                  "Fishing vessels play a critical role in feeding the world, but they also contribute significantly to global greenhouse gas emissions. While some vessels broadcast their locations via AIS (Automatic Identification System), many do not—leaving large gaps in how we monitor industrial fishing activity and its environmental footprint. To better understand and manage the ocean's role in climate change, we need new tools that illuminate where emissions are coming from and who is responsible."), 
           
           tags$p(style = "font-weight: bold; color: white; margin: 40px;", 
                  "This dashboard combines satellite-based vessel tracking, emissions modeling, and machine learning to map fishing vessel emissions across the globe—both from broadcasting and non-broadcasting fleets. By visualizing emissions at sea, this tool offers a replicable and scalable approach for understanding the climate impact of global fisheries and informing more sustainable ocean governance."), 
           
           
           # ---- Teaser Sections ----
           div(style = "margin-top: 40px;",
               fluidRow(
                 column(width = 3),  # spacer
                 
                 # Emissions Map Teaser ----
                 column(width = 3,
                        div(id = "explore_map_card",
                            style = "cursor: pointer;
                                      position: relative;
                                      padding-top: 50px;
                                      padding-bottom: 10px;
                                      background-color: white;
                                      border-radius: 8px;
                                      box-shadow: 0 4px 10px rgba(0,0,0,0.1);
                                      overflow: visible;
                                      font-family: sans-serif;
                                      text-align: center;",
                            
                            div(style = "height: 12px;
                                         background-color: #F9B928;
                                         border-top-left-radius: 8px;
                                         border-top-right-radius: 8px;
                                         position: absolute;
                                         top: 0; left: 0; right: 0;"),
                                              
                            div(style = "position: absolute;
                                         top: -28px;
                                         left: 50%;
                                         transform: translateX(-50%);
                                         background-color: black;
                                         width: 56px;
                                         height: 56px;
                                         border-radius: 50%;
                                         display: flex;
                                         align-items: center;
                                         justify-content: center;
                                         z-index: 10;",
                                tags$i(class = "fas fa-earth-americas", style = "color: white; font-size: 24px;")
                            ),
                            
                            h4(strong("Fishing Vessel Emissions Map")),
                            p("Explore where large fishing vessels operate around the world. Many use AIS (Automatic Identification System) to broadcast their location — but over half of the global fleet do not broadcast their location, making them harder to track emissions.",
                              style = "color: #444;
                      padding: 0 20px;")
                        )
                 ), # END column
                 
                 
                 # Seafood Explorer Teaser ----
                 column(width = 3,
                        div(id = "explore_seafood_card",
                        style = "cursor: pointer;
                                  position: relative;
                                  padding-top: 50px;
                                  padding-bottom: 10px;
                                  background-color: white;
                                  border-radius: 8px;
                                  box-shadow: 0 4px 10px rgba(0,0,0,0.1);
                                  overflow: visible;
                                  font-family: sans-serif;
                                  text-align: center;",
                            
                            div(style = "height: 12px;
                                          background-color: #F9B928;
                                          border-top-left-radius: 8px;
                                          border-top-right-radius: 8px;
                                          position: absolute;
                                          top: 0; left: 0; right: 0;"),
                            
                            div(style = "position: absolute;
                                          top: -28px;
                                          left: 50%;
                                          transform: translateX(-50%);
                                          background-color: black;
                                          width: 56px;
                                          height: 56px;
                                          border-radius: 50%;
                                          display: flex;
                                          align-items: center;
                                          justify-content: center;
                                          z-index: 10;",
                                
                                tags$i(class = "fas fa-chart-bar",
                                       style = "color: white;
                                       font-size: 24px;")
                            ),
                            
                            h4(strong("Compare Seafood Emissions")),
                            p("This is where the tool description goes.",
                              style = "color: #444;
                                        padding: 0 20px;
                                        margin-bottom: 20px;")
                        )
                 ), # END column
                 
                 column(width = 3)  # spacer
               ) # END fluidRow
           ) # END div (Teaser Sections)
           
  ), # END tabPanel (Home)
  
  
  
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
                           width: 400px;
                           background-color: #f9f9f9;
                           padding: 15px;
                           border-right: 0px solid #ccc;
                           z-index: 1001;",
                   
                   # ---- Toggle Button Inside Sidebar ----
                   actionButton("toggle_sidebar_open_input",
                                label = NULL,
                                icon = icon("angle-left", style = "font-size: 25px;"),
                                style = "position: absolute;
                                         top: 40%;
                                         right: -40px;
                                         width: 0%;
                                         background-color: #f9f9f9;
                                         border: none;"),
                   
                   # Map title
                   tags$h3(style = "font-size: 24px; font-weight: bold; color: #20404F; margin-bottom: 5px;", 
                           "Fishing Vessel Emissions"), 
                   
                   # Map description
                   tags$p(style = "font-weight: bold; color: #20404F; margin-bottom: 20px;", 
                           "Explore where emissions from large-scale fishing vessels occur around the world, using data from Global Fishing Watch."), 
                   
                   # ---- Sidebar Layer Controls -------------------------------
                   
                 # Controls for broadcasting emissions data
                 column(width = 12,
                        div(
                          style = "display: flex; align-items: center; gap: 10px;",
                          
                          # materialSwitch (broadcasting)
                          materialSwitch(
                            inputId = "show_broadcasting_input",
                            label = tags$div(
                              style = "font-size: 18px; font-weight: bold;",
                              "AIS Broadcasting Emissions"
                            ),
                            value = TRUE,
                            status = "info"
                          ), # END materialSwitch
                          
                          # info icon (same line)
                          infoPopup("broadcasting_popup", "This is a very important description of the broadcasting data. This is what.")
                        ) # END div
                 ), # END column
                 
                   
                 
                 # Hidden broadcsting legend
                 hidden(div(id = "broadcasting_legend",
                            tags$div(style = "background: linear-gradient(to right,#015661, #03C7E8);
                                               height: 20px;
                                               width: 70%;
                                               border: 1px solid #ccc;"),
                              
                              tags$div(textOutput("total_broadcasting"),
                                       style = "font-size: 15px; font-weight: bold; color: #053762; margin-bottom: 10px;"),
                              
                              pickerInput(inputId = "country_select_input",
                                          label = "Select a country",
                                          choices = c("All Countries", sort(unique(broadcasting_emissions$country_name[broadcasting_emissions$country_name != "All Countries"]))),
                                          selected = "All Countries",
                                          options = list(`live-search` = TRUE,
                                                         `noneSelectedText` = "All Countries")), # END pickerInput (country select)
                              
                              tags$div(
                                textOutput("no_data_warning"),
                                style = "color: #81818F;
                                        margin-top: 10px;"))# END div (no data warning text)
                          
                          
                          ), # END hidden (broadcasting emissions legend and text)
                   
               
                   
                   # Horizontal separator
                   tags$hr(),
                   
                   materialSwitch("show_non_broadcasting_input",
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
                                style = "font-size: 15px;
                                        font-weight: bold;
                                        color: #053762;
                                        margin-bottom: 10px;")) # END div
                          ), # END hidden
                   
                   # Horizontal separator
                   tags$hr(),
                   
                   materialSwitch("show_fao_zones_input",
                                  label = tags$div(style = "font-size: 18px;
                                                   font-weight: bold;",
                                                   "FAO Major Fishing Zones"),
                                  value = FALSE,
                                  status = "info")
                   
               ), # END sidebar panel
               
               actionButton("toggle_sidebar_close_input",
                            label = NULL,
                            icon = icon("angle-right",
                                        style = "font-size: 25px;"),
                            style = "position: absolute;
                                     top: 40%;
                                     left: -20px;
                                     width: 0%;
                                     background-color: #f9f9f9;
                                     border: none;
                                     display: none;
                                     z-index: 1001"), # END actionButton for sidebar to close sidebar
               
               
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
                             sliderInput("year_slider_input_map",
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
# ---- Compare Seafood Emissions Page ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

tabPanel("Compare Seafood Emissions",
         
         tags$head(
           tags$style(HTML("
    .plot-button {
      background-color: #08C4E5 !important;
      color: white !important;
      border: none !important;
    }

    .plot-button-active {
      background-color: #F9B928 !important;
      color: black !important;
    }
  "))
         ),
         
         
         # Header Row ----
         
         fluidRow( tags$hr()),
         fluidRow(
           # Centered button row ----
           column(
             width = 12,
             div(
               style = "text-align: center;",
               
               # Compare species button ---- 
               div(
                 style = "display: inline-block; margin: 0 10px;",
                 actionButton(
                   "compare_species_input",
                   tagList(icon("fish",
                                style = "margin-right: 8px;"),
                     "Compare Species"),
                   class = "btn btn-lg") # END actionButton (Compare Species)
               ), # END div (Compare Species wrapper)
               
               # Compare countries button ---- 
               div(
                 style = "display: inline-block; margin: 0 10px;",
                 actionButton("compare_countries_input",
                   tagList(icon("earth-americas",
                                style = "margin-right: 8px;"),
                           "Compare Countries"),
                   class = "btn btn-lg") # END actionButton (Compare Countries)
               ), # END div (Compare Countries wrapper)
               
               # Select a country button ---- 
               div(
                 style = "display: inline-block; margin: 0 10px;",
                 actionButton("select_country_input",
                   tagList(icon("flag",
                                style = "margin-right: 8px;"),
                     "Select a Country"),
                   class = "btn btn-lg") # END actionButton (Select a Country)
               ) # END div (Select a Country wrapper)
               
             ) # END div (centered button container)
           ) # END column (full-width)
           
         ), # END fluidRow

         # Country Selector and Total Emissions (hidden initially)
         shinyjs::hidden(
           
           div(id = "country_select_plot_input", 
               fluidRow(
                 column(
                   width = 12,
                   div(
                     style = "text-align: center;",
                     
                     # Country dropdown ----
                     div(
                       style = "display: inline-block; margin-right: 20px;",
                       pickerInput(
                         inputId = "selected_country_input",
                         label = "Select a Country:",
                         choices = c("Select a country" = "", sort(unique(species_data$country_name))),
                         selected = NULL,
                         options = list(`live-search` = TRUE,
                                        `noneSelectedText` = "All Countries")
                       ) # END pickerInput
                     ), # END div (dropdown wrapper)
                     
                     
                     # Total emissions text ----
                     div(
                       style = "display: inline-block;",
                       tags$h4(
                         textOutput("selected_country_total"),
                         style = "color: white; font-weight: bold; margin-top: 25px;"
                       ) # END h4
                     ) # END div (text wrapper)
                     
                   ) # END center div
                 ) # END column
               ) # END fluidRow
           ) # END div (country_select_plot_input)
           
         ),
         


# --- Second Row with ggplot ---------------------------------------------------
fluidRow(
  column(width = 12,
         div(style = "background-color:#0B2232;
                     height: 60vh;
                     min-height: 300px;
                     margin-top: 30px;
                     margin-bottom: 30px;
                     margin-left: 20px;
                     margin-right: 20px;",
             
             # --- Define plots (hidden when not selected) ----------------------
             
             # ---- Country plot (default visible) ----
             div(
               id = "country_plot",
               plotOutput("country_plot_output",
                          height = "60vh", fill = TRUE) |> 
                 withSpinner(type = 4, color = '#08C4E5')), # END div
             
             # ---- ISSCAAP plot (hidden on load) ----
             shinyjs::hidden(
               div(id = "isscaap_plot",
                 plotOutput("isscaap_plot_output",
                            height = "60vh", fill = TRUE) |> 
                   withSpinner(type = 4, color = '#08C4E5')) # END div 
             ), # END hidden
             
             # ---- Species plot for selected country (hidden on load) ----
             shinyjs::hidden(
               div(id = "species_bar_plot",
                 plotOutput("species_bar_plot_output",
                            height = "60vh", fill = TRUE) |> 
                   withSpinner(type = 4, color = '#08C4E5')
               ) # END div
             ) # END hidden
             
         ) # END row div
  )
), # END fluid row (plots)


fluidRow(
  column(width = 6,
         # ---- Year Slider ----
         div(bottom = 30,
                       style = "z-index: 1000;
                                    background-color: rgba(255,255,255,0.8);
                                    padding: 8px;
                                    border-radius: 8px;
                                    width: 20%;",
                       sliderInput("year_slider_input_plot",
                                   "Select Year",
                                   min = 2016, # UPDATE to min
                                   max = 2022, # UPDATE to max
                                   value = 2022, # UPDATE to max
                                   step = 1,
                                   sep = "",
                                   width = "100%",
                                   ticks = TRUE) # END sliderInput (year)
         ) # END absolutePanel - year

         ),
  column(
    width = 6,
    div(style = "display: flex;
                justify-content: flex-end;
                align-items: center;
                padding-right: 20px;
                margin-top: 10px;",
      
      tags$div("Total Emissions", 
               style = "margin-right: 20px;
                       color: white;
                       font-weight: 500;
                       font-size: 14px;"),
      
      div(style = "display: inline-block;",
        materialSwitch(
          inputId = "unit_plot_toggle_input",
          label = NULL,
          status = "info",
          right = TRUE,
          inline = TRUE) # END materialSwitch (per unit toggle)
          ), # END div (unit materialSwitch)
      
      tags$div("Per Unit Catch", 
               style = "margin-left: 8px;
                       margin-right: 100px;
                       color: white;
                       font-weight: 500;
                       font-size: 14px;")
      ) # END div (Unit toggle)
  ) # END column (Unit toggle)

) # END fluidRow (Unit toggle)
         
), # END tabPanel (Seafood Emissions Explorer Page)

# ------------------------------------------------------------------------------------------------------------------
# ---- Usage Guide Page --------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

  tabPanel("Learn More",
           # ---- Hero Section ----
           div(style = "background-image: url('images/home-image.jpg');
                        background-size: cover;
                        background-position: center;
                        padding: 100px 0;
                        text-align: center;
                        color: #e8fffd;",
               h1("Seamissions Global Fishing Emissions Explorer"),
               h4("Connecting data to action for sustainable oceans.")), # END hero section
          
           fluidRow(
             column(12,
                    div(style = "background-color: #1b2a49; padding: 30px; color: white;",
                             h4(strong("Background")),
                             tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                             "This is the background of our project. This is the background of our project.")  
                      
                      
                    )
                    )
            
          ),
          
          fluidRow(
            column(12,
                   div(style = "background-color: #1b2a49; padding: 30px; color: white;",
                       h4(strong("Who we are")),
                       tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                              "This is who we are. Here we have informatoin about the team.")  
                   )
            )
            
          ),
          
          fluidRow(
            column(12,
                   div(style = "background-color: #1b2a49; padding: 30px; color: white;",
                       h4(strong("About our partners")),
                       tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                              "This is information about our project. Here are some details.")  
                   )
            )
            
          ),
          
          fluidRow(
            column(12,
                   div(style = "background-color: #1b2a49; padding: 30px; color: white;",
                       h4(strong("Background")),
                       tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                              "This is the background of our project. This is the background of our project.")  
                   )
            )
            
          ),
          
          fluidRow(
            column(12,
                   div(style = "background-color: #1b2a49; padding: 30px; color: white;",
                       h4(strong("Methods")),
                       tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                              "These are the methods that we used on this project.")  
                   )
            )
            
          )
          
           ) # END tabPanel (Learn More Page)
  
 

) # END navbarPage



