# ---- Server page for emissions dashboard -------------------------------------

server <- function(input, output, session) {
  
  # ---- Define color palettes ----
  blue_palette <- colorRamp(c("#20404F","#4C9EA6","#67D6E0", "#76F3FF","#A9F2FF","#DAF3FF", "white"))((1:256)/256)
  pink_palette <- colorRamp(c("#7D3650","#D15494","#FF67B5","#FF89C8","#FFAED1", "#FFECE5", "white"))((1:256)/256)
  
  # ---- Set initial view ----
  first_time <- reactiveVal(TRUE)  # Track if first time entering map tab
  current_view <- reactiveVal(list(zoom = 3, location = c(0, 0)))
  loading <- reactiveVal(TRUE)
  
  # ---- sidebar toggle logic ----
  observeEvent(input$toggle_sidebar, {
    shinyjs::hide("sidebar-panel")                  # Hide sidebar
    shinyjs::show("toggle_sidebar_outside")         # Show outside button
  }) # END inside button toggle
  
  observeEvent(input$toggle_sidebar_outside, {
    shinyjs::show("sidebar-panel")                  # Show sidebar
    shinyjs::hide("toggle_sidebar_outside")         # Hide outside button
  }) # END outside button toggle
  
  # ---- Reset sidebar visibility on tab switch ----
  observe({
    if (input$navbarPage == "Emissions Map") {      # Ensure sidebar opens on tab switch
      shinyjs::show("sidebar-panel")                # Show sidebar by default
      shinyjs::hide("toggle_sidebar_outside")       # Hide outside button by default
      
      # ---- first time loading logic ----
      if (first_time()) {
        if (input$show_all_countries) {
          mapdeck_update(map_id = "emissions_map") %>%
            add_polygon(
              data = all_emissions,
              layer_id = "all_countries",
              fill_colour = "emissions_co2_mt",
              palette = blue_palette,
              fill_opacity = (100 - input$opacity_non_broadcasting) / 100,
              tooltip = "emissions_co2_mt",
              update_view = FALSE
            )
        }
        first_time(FALSE)  # Set flag to false after initial load
      }
    }
  }) # END tab switch observer
  
  # ---- Instantly toggle visibility of country select input ----
  observeEvent(input$show_all_countries, {
    if (input$show_all_countries) {
      shinyjs::show("country_select")
    } else {
      shinyjs::hide("country_select")
      updateSelectInput(session, "country_select", selected = "")
    }
  }, priority = 10) # END observe event - select country
  
  # ---- Filter emissions by country and year ----
  country_filtered <- reactive({
    req(input$country_select)
    req(input$year_slider_input)
    
    country_emissions[
      country_emissions$flag == input$country_select &
        country_emissions$year == input$year_slider_input,
    ]
  }) # END reactive filter - select country
  
  # ---- Loading symbol ----
  output$loading_ui <- renderUI({
    if (loading()) {
      tags$div(
        id = "loading-overlay",
        style = "
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1001;
        background-color: rgba(2, 181, 886, 0.8);
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 0 10px rgba(0,0,0,0.3);
        text-align: center;",
        icon("spinner", class = "fa-spin", style = "font-size: 24px; margin-bottom: 10px;"),
        tags$p("Loading...")
      ) # END tags div
    }
  }) # END renderUI
  
  # ---- Show/hide transparency sliders ----
  observeEvent(input$show_all_countries, {
    if (input$show_all_countries) {
      shinyjs::show("opacity_all_countries")
    } else {
      shinyjs::hide("opacity_all_countries")
    }
  }) # END observe - toggle opacity slider all countries
  
  observeEvent(input$show_non_broadcasting, {
    if (input$show_non_broadcasting) {
      shinyjs::show("opacity_non_broadcasting")
    } else {
      shinyjs::hide("opacity_non_broadcasting")
    }
  }) # END observe - toggle opacity slider non broadcasting
  
  observeEvent(input$show_fao_zones, {
    if (input$show_fao_zones) {
      shinyjs::show("opacity_fao_zones")
    } else {
      shinyjs::hide("opacity_fao_zones")
    }
  }) # END observe - toggle opacity slider FAO zones
  
  # ---- All country emissions layer ----
  observe({
    if (input$show_all_countries) {
      loading(TRUE)
      
      mapdeck_update(map_id = "emissions_map") %>%
        add_polygon(
          data = all_emissions,
          layer_id = "all_countries",
          fill_colour = "emissions_co2_mt",
          palette = blue_palette,
          fill_opacity = (100 - input$opacity_all_countries) / 100,
          tooltip = "emissions_co2_mt",
          update_view = FALSE) # END add_polygon - all countries
      
      later::later(function() {
        loading(FALSE)
      }, delay = 0.5) # add 0.5 second delay to loader
      
    } else {
      # Clear broadcasting country polygons when switch is not on
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "all_countries") %>%
        clear_polygon(layer_id = "country_layer")
    }
  }) # END observe - all country emissions layer
  
  # ---- Non-broadcasting emissions layer (lazy load) ----
  observe({
    if (input$show_non_broadcasting) {
      loading(TRUE)
      
      nb_emissions <- readRDS("data/nb_emissions.rds") %>%
        filter(emissions_co2_mt >= 200, year == 2016)
      
      mapdeck_update(map_id = "emissions_map") %>%
        add_polygon(
          data = nb_emissions,
          layer_id = "non_broadcasting_layer",
          fill_colour = "emissions_co2_mt",
          palette = pink_palette,
          fill_opacity = (100 - input$opacity_all_countries) / 100,
          tooltip = "emissions_co2_mt",
          update_view = FALSE) # END add_polygon - nb emissions
      
      later::later(function() {
        loading(FALSE)
      }, delay = 0.5)  # add 0.5 second delay to loader
      
    } else {
      # Clear broadcasting country polygons when switch is not on
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "non_broadcasting_layer")
    }
  }) # END observe - non broadcasting emissions layer
  
  # ---- Initialize emissions map ----
  output$emissions_map <- renderMapdeck({
    loading(TRUE)
    
    later::later(function() {
      loading(FALSE)
    }, delay = 0.5)  # add 0.5 second delay to loader
    
    mapdeck(
      token = MAPBOX_TOKEN,
      style = mapdeck_style("dark"),
      zoom = 2,
      location = c(-10, 20)
    )
  }) # END renderMapdeck - emissions map
  
  # ---- Add country emissions if a country is selected ----
  observeEvent({
    input$year_slider_input
    input$country_select
  }, {
    loading(TRUE)
    
    if (!is.null(input$country_select) && input$country_select != "") {
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "all_countries")
    }
    
    map <- mapdeck_update(map_id = "emissions_map") %>%
      clear_polygon(layer_id = "country_layer")
    
    if (!is.null(input$country_select) && input$country_select != "") {
      filtered <- country_filtered()
      
      if (nrow(filtered) > 0) {
        map <- map %>%
          add_polygon(
            data = filtered,
            layer_id = "country_layer",
            fill_colour = "emissions_co2_mt",
            palette = blue_palette,
            fill_opacity = (100 - input$opacity_all_countries) / 100,
            tooltip = "emissions_co2_mt",
            update_view = FALSE
          )
      } else {
        mapdeck_update(map_id = "emissions_map") %>%
          clear_polygon(layer_id = "fao_layer") %>%
          clear_path(layer_id = "fao_border_layer")
      }
    }
    
    map
    loading(FALSE)
  }) # END observeEvent - add country emissions
  
  # ---- FAO Zones layer ----
  observe({
    if (input$show_fao_zones) {
      mapdeck_update(map_id = "emissions_map") %>%
        add_polygon(
          data = fao_regions,
          layer_id = "fao_layer",
          fill_colour = "plasma",
          fill_opacity = (100 - input$opacity_fao_zones) / 100,
          tooltip = "zone",
          update_view = FALSE) %>%
        add_path(
          data = fao_borders,
          layer_id = "fao_border_layer",
          stroke_color = "zone",
          palette = m,
          stroke_width = 4,
          update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "fao_layer") %>%
        clear_path(layer_id = "fao_border_layer")
    }
  }) # END observe - FAO Zones layer
  
  # ---- Track view so we don’t reset zoom/location ----
  observe({
    input$emissions_map_view_state
    isolate({
      view <- input$emissions_map_view_state
      if (!is.null(view)) {
        current_view(list(
          zoom = view$zoom,
          location = c(view$longitude, view$latitude)
        ))
      }
    })
  }) # END observe - track map view
  
  # ---- Initial render ----
  output$emissions_map <- renderMapdeck({
    mapdeck(
      token = MAPBOX_TOKEN,
      style = "mapbox://styles/mapbox/dark-v10",
      zoom = current_view()$zoom,
      location = current_view()$location
    )
  }) # END renderMapdeck - initial render
  
  # ---- Re-render map when basemap style changes ----
  observeEvent(input$basemap_style, {
    style_choice <- if (input$basemap_style) {
      "mapbox://styles/mapbox/dark-v10"
    } else {
      "mapbox://styles/mapbox/light-v10"
    }
    
    output$emissions_map <- renderMapdeck({
      mapdeck(
        token = MAPBOX_TOKEN,
        style = style_choice,
        zoom = current_view()$zoom,
        location = current_view()$location
      )
    })
  }) # END observeEvent - basemap style
  
} # END server function



