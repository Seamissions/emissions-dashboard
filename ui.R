# ---- UI ----------------------------------------------------------------------

# ---- header -------------------------------------------------
header <- dashboardHeader(
  title = "Seamissions Dashboard",
  titleWidth = 320
)

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
  materialSwitch(
    inputId = "show_all_countries",
    label = "Broadcasting Emissions",
    value = FALSE,
    status = "info"
  ),
  
  # ---- Country picker input (hidden by default) ----
  hidden(
    pickerInput(
      inputId = "country_select",
      label = "Filter To A Country (Flag)",
      choices = country_flags,
      multiple = TRUE,
      options = list(
        `max-options` = 1,
        `max-options-text` = "You can only select 1 country",
        `actions-box` = TRUE,
        `live-search` = TRUE
      )
    )
  ),
  
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
  materialSwitch(
    inputId = "show_fao_zones",
    label = "FAO Zones",
    value = FALSE,
    status = "info"
  )
)

# ---- dashboard body ----------------------------------------
body <- dashboardBody(
  tabsetPanel(
    # ---- map tab --------------------------------------------
    tabPanel(
      title = "Dashboard",
      value = "Dashboard",
      
      div(
        id = "map-container",
        style = "position: relative; height: 90vh;",
        mapdeckOutput("emissions_map", height = "100%"),
        uiOutput("loading_ui")
      ),
      
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
          inline = TRUE
        )
      ),
      
      # ---- year slider ----
      absolutePanel(
        top = 120, right = 180, width = "auto",
        style = "z-index: 1000;
        background-color: rgba(255,255,255,0.8);
        padding: 8px;
        border-radius: 8px;",
        sliderInput(
          inputId = "year_slider_input",
          label = "Select Year",
          min = year_min,
          max = year_max,
          value = year_max,
          step = 1,
          sep = "",
          animate = TRUE
        )
      )
    ),
    
    # ---- about tab -----------------------------------------
    tabPanel(
      title = "About",
      value = "About",
      "About content here"
    )
  )
)

# ---- combine all in dashboardPage ---------------------------
dashboardPage(header, sidebar, body)
