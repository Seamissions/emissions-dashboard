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
                                         right: -35px;
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
                                     left: -35px;
                                     width: 0%;
                                     background-color: #f9f9f9;
                                     border: none;
                                     display: none;
                                     z-index: 1001"), # END actionButton for sidebar to close sidebar
               
               
               # ---- Emissions Map --------------------------------------------
               mapdeckOutput("emissions_map", height = "100%"),
               
               # ---- Zoom Control Buttons ----
               absolutePanel(top = 20,
                             right = 10,
                             style = "z-index: 1001;",
                             tags$style(HTML(".zoom-button {
                                            background-color: rgba(255, 255, 255, 0.85) !important;
                                            color: #DA8D03 !important;
                                            border: 0px solid white !important;
                                            border-radius: 4px !important;
                                            width: 40px;
                                            height: 40px;
                                            font-size: 20px;
                                            padding: 0;
                                            text-align: center;
                                          }
                                        ")),
                 actionButton("zoom_in", "+", class = "zoom-button"),
                 actionButton("zoom_out", "−", class = "zoom-button")
               ),
               
               
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
                             "Global fisheries are heavily reliant on fossil fuels, contributing significantly to the rise in global greenhouse gas emissions driving climate change. While satellite technology is commonly used to monitor land-based emissions (and ocean-based emissions of shipping vessels), studies primarily estimating ocean-based emissions in the fishing sector remain limited. In collaboration with the Environmental Markets Lab (emLab) and Global Fishing Watch, this project leverages novel, high-resolution satellite-based datasets to provide precise insights into the emissions associated with global fisheries. We develop a reproducible, extensible, and open-source data processing pipeline to connect emissions data with seafood production data, along with an interactive dashboard to explore the resulting dataset.",
                             tags$br(),
                             tags$a(href = "https://github.com/Seamissions", target = "_blank", "https://github.com/Seamissions"))  
                    ) # END div
             )
            
          ), # END fluidrow 'background'
          
          fluidRow(
            column(12,
                   div(
                     style = "background-color: #1b2a49; padding: 30px; color: white;",
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
                     style = "background-color: #1b2a49; padding: 30px; color: white;",
                     
                     h4(strong("Our Partners")),
                     
                     # emLab section
                     tags$h5(tags$u("Environmental Markets Lab (emLab)")),
                     tags$p(
                       style = "font-weight: normal; color: white; margin-bottom: 15px;",
                       "A think-and-do tank for market-based approaches to environmental challenges. They are an interdisciplinary team of scientists based at the University of California Santa Barbara that conducts cutting-edge, data-driven research on the power, limitations, and design of market-based approaches to tackle the world's most pressing environmental problems."
                     ),
                     tags$p(
                       style = "font-weight: normal; color: white; margin-bottom: 20px;",
                       "In collaboration with implementing partners, they aim to better align environmental objectives and economic incentives in support of sustainable livelihoods and a resilient planet."
                     ),
                     tags$img(src = "images/gfw-logo.png", style = "width: 100%; max-width: 700px; margin-bottom: 30px;"),
                     
                     # Global Fishing Watch section
                     tags$h5(tags$u("Global Fishing Watch")),
                     tags$p(
                       style = "font-weight: normal; color: white; margin-bottom: 15px;",
                       "Global Fishing Watch seeks to advance ocean governance through increased transparency of human activity at sea, enabling scientific research and driving transformation in ocean management by creating and publicly sharing map visualizations, data, and analysis tools."
                     ),
                     tags$p(
                       style = "font-weight: normal; color: white; margin-bottom: 20px;",
                       "Global Fishing Watch was founded in 2015 through a collaboration between three partners: ",
                       tags$strong("Oceana"), ", an international ocean conservation organization; ",
                       tags$strong("SkyTruth"), ", a technology firm that uses satellite imagery and data to protect the environment; and ",
                       tags$strong("Google"), ", whose tools and contributions help process big data. ",
                       "In June 2017, Global Fishing Watch was established as an independent, international nonprofit organization."
                     ),
                     tags$img(src = "images/gfw-logo.png", style = "width: 100%; max-width: 700px; margin-bottom: 30px;")
                   ) # END div
            )
            
          ), # END fluidrow 'About our partners'
          
          fluidRow(
            column(12,
                   div(style = "background-color: #1b2a49; padding: 30px; color: white;",
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
                   div(style = "background-color: #1b2a49; padding: 30px; color: white;",
                       h4(strong("Usage Guide")),
                       tags$p(style = "font-weight: normal; color: white; margin: 30px;",
                              tags$a(href = "https://globalfishingwatch.org/tutorials/", target = "_blank", "https://globalfishingwatch.org/tutorials/")
                              )  
                   ) # END div
            )
            
          ), # END fluidrow 'Usage Guide'
          
          fluidRow(
            column(12,
                   div(
                     style = "background-color: #1b2a49; padding: 30px; color: white;",
                     
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
                     style = "background-color: #1b2a49; padding: 30px; color: white;",
                     
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



