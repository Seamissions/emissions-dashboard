# ---- UI ----------------------------------------------------------------------

# ---- header -------------------------------------------------
header <- dashboardHeader(
  title = "Seamissions Dashboard",
  titleWidth = 320
) # end dashboard header

# ---- dashboard sidebar -------------------------------------
sidebar <- dashboardSidebar(
  useShinyjs(),
  width = 400,
  
      tags$div(
        tags$h4(
          icon("ship"),
          "Emissions Data",
          style = "margin-top: 25px; margin-left: 10px; font-weight: bold;"
        )
      ),
  
  # ---- all emissions switch ----
  materialSwitch(inputId = "show_all_countries",
                 label = "Broadcasting Emissions",
                 value = TRUE,
                 status = "info"), 
    
  # ---- Country picker input (hidden by default) ----
  hidden(
    pickerInput(
      inputId = "country_select",
      label = "Filter To A Country (Flag)",
      choices = sort(unique(country_emissions$flag)),
      multiple = TRUE,
      options = list(
        `max-options` = 1,
        `max-options-text` = "You can only select 1 country",
        `actions-box` = TRUE,
        `live-search` = TRUE) # end options
    ) # end picker input
  ), # end hidden
  
# ---- switch to show non-broadcasting emissions ----
  materialSwitch(
    inputId = "show_non_broadcasting",
    label = "Non-Broadcasting Emissions",
    value = FALSE,
    status = "primary"
  ),
  
    tags$div(
      tags$h4(
        icon("layer-group"),
        "Other Layers",
        style = "margin-top: 25px; margin-left: 10px; font-weight: bold;"
      )
    ),
    
    # ---- FAO zones switch ----
    materialSwitch(inputId = "show_fao_zones",
                   label = "FAO Zones",
                   value = FALSE,
                   status = "info")

) # end dashboardSidebar

# ---- dashboard body ----------------------------------------
body <- dashboardBody(
  tabsetPanel(
    
    # ---- map tab--------------------------------------------
    tabPanel(
      title = "Dashboard",
      value = "Dashboard",
      
      # initiate map with loading
      div(
        id = "map-container",
        style = "position: relative; height: 90vh;",
        mapdeckOutput("emissions_map", height = "100%"),
        uiOutput("loading_ui")
      ), # end div
      
      # ---- basemap toggle ----
      absolutePanel(
        top = 120, right = 20, width = "auto",
        style = "z-index: 1000;
        background-color: rgba(255,255,255,0.8);
        padding: 8px;
        border-radius: 8px;",
        
        switchInput(
          inputId = "basemap_style",
          label = "Mode",
          value = TRUE,
          onLabel = "Dark",
          offLabel = "Light",
          onStatus = "info",
          offStatus = "info",
          inline = TRUE) # end switch input
      ), # end of absolute panel for basemap switch
      
      # ---- year slider ----
      absolutePanel(
        top = 120, right = 180, width = "auto",
        style = "z-index: 1000;
        background-color: rgba(255,255,255,0.8);
        padding: 8px;
        border-radius: 8px;",
        
      # ---- Year slider input ----
      sliderInput(inputId = "year_slider_input",
                  label = "Select Year",
                  min = min(country_emissions$year, na.rm = TRUE),
                  max = max(country_emissions$year, na.rm = TRUE),
                  value = max(country_emissions$year, na.rm = TRUE),
                  step = 1,
                  sep = "",
                  animate = TRUE), # end slider input
      ) # end absolute panel for year slider
    ), # end tab panel
    
    
    # ---- about tab -----------------------------------------
    tabPanel(
      title = "About",
      value = "About",
      "About content here") # end tab panel
  ) # end tabset panel
  
) # ----  end dashboard body ----------------------------------

# ---- combine all in dashboardPage ---------------------------
dashboardPage(header, sidebar, body)

