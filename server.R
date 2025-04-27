# ---- Server page for emissions dashboard -------------------------------------

server <- function(input, output, session) {
  
  # ---- Define color palettes ----
  blue_palette <- colorRamp(c("#20404F", "#4C9EA6", "#67D6E0", "#76F3FF", "#A9F2FF", "#DAF3FF", "white"))((1:256) / 256)
  pink_palette <- colorRamp(c("#7D3650", "#D15494", "#FF67B5", "#FF89C8", "#FFAED1", "#FFECE5", "white"))((1:256) / 256)
  
  # ---- Set initial view ----
  first_time <- reactiveVal(TRUE)  # Track if first time entering map tab
  current_view <- reactiveVal(list(zoom = 3, location = c(0, 0)))
  loading <- reactiveVal(TRUE)
  
  # ---- Sidebar toggle logic ----
  observeEvent(input$toggle_sidebar, {
    shinyjs::hide("sidebar-panel")
    shinyjs::show("toggle_sidebar_outside")
  })
  
  observeEvent(input$toggle_sidebar_outside, {
    shinyjs::show("sidebar-panel")
    shinyjs::hide("toggle_sidebar_outside")
  })
  
  # ---- Reset sidebar visibility on tab switch ----
  observe({
    if (input$navbarPage == "Emissions Map") {
      shinyjs::show("sidebar-panel")
      shinyjs::hide("toggle_sidebar_outside")
      if (first_time()) {
        if (input$show_all_countries) {
          mapdeck_update(map_id = "emissions_map") %>%
            add_polygon(
              data = all_emissions |> filter(year == input$year_slider_input),
              layer_id = "all_countries",
              fill_colour = "emissions_co2_mt",
              palette = blue_palette,
              fill_opacity = 0.5,
              tooltip = "emissions_co2_mt",
              update_view = FALSE
            )
        }
        first_time(FALSE)
      }
    }
  })
  
  # ---- Toggle visibility of country select input ----
  observeEvent(input$show_all_countries, {
    if (input$show_all_countries) {
      shinyjs::show("country_select")
    } else {
      shinyjs::hide("country_select")
      updateSelectInput(session, "country_select", selected = "")
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "all_countries") %>%
        clear_polygon(layer_id = "country_layer")
    }
  }, priority = 10)
  
  # ---- Filter emissions by country and year ----
  country_filtered <- reactive({
    req(input$country_select)
    req(input$year_slider_input)
    country_emissions[
      country_emissions$flag == input$country_select &
        country_emissions$year == input$year_slider_input,
    ]
  }) # END reactive (country_filtered)
  
  # ---- Loading symbol ----
  output$loading_ui <- renderUI({
    if (loading()) {
      tags$div(
        id = "loading-overlay",
        style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1001; background-color: rgba(2, 181, 886, 0.8); padding: 20px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.3); text-align: center;",
        icon("spinner", class = "fa-spin", style = "font-size: 24px; margin-bottom: 10px;"),
        tags$p("Loading...")
      )
    }
  })
  
  # ---- All countries & country emissions layers ----
  observe({
    req(input$show_all_countries)
    mapdeck_update(map_id = "emissions_map") %>%
      clear_polygon(layer_id = "all_countries") %>%
      clear_polygon(layer_id = "country_layer")
    
    if (is.null(input$country_select) || input$country_select == "") {
      loading(TRUE)
      mapdeck_update(map_id = "emissions_map") %>%
        add_polygon(
          data = all_emissions |> filter(year == input$year_slider_input),
          layer_id = "all_countries",
          fill_colour = "emissions_co2_mt",
          palette = blue_palette,
          fill_opacity = 0.5,
          tooltip = "emissions_co2_mt",
          update_view = FALSE
        )
      later::later(function() { loading(FALSE) }, delay = 0.2)
    } else {
      filtered <- country_filtered()
      if (nrow(filtered) > 0) {
        loading(TRUE)
        mapdeck_update(map_id = "emissions_map") %>%
          add_polygon(
            data = filtered,
            layer_id = "country_layer",
            fill_colour = "emissions_co2_mt",
            palette = blue_palette,
            fill_opacity = 0.6,
            tooltip = "emissions_co2_mt",
            update_view = FALSE
          )
        later::later(function() { loading(FALSE) }, delay = 0.2)
      }
    }
  })
  
  # ---- Non-broadcasting emissions layer ----
  observe({
    if (input$show_non_broadcasting) {
      loading(TRUE)
      nb_emissions <- readRDS("data/nb_emissions.rds") |> 
        filter(emissions_co2_mt >= 200)
      mapdeck_update(map_id = "emissions_map") %>%
        add_polygon(
          layer_id = "non_broadcasting_layer",
          data = nb_emissions,
          fill_colour = "emissions_co2_mt",
          palette = pink_palette,
          fill_opacity = 0.5,
          tooltip = "emissions_co2_mt",
          update_view = FALSE
        )
      later::later(function() { loading(FALSE) }, delay = 0.2)
    } else {
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "non_broadcasting_layer")
    }
  })
  
  # ---- Initialize emissions map ----
  output$emissions_map <- renderMapdeck({
    loading(TRUE)
    later::later(function() { loading(FALSE) }, delay = 0.2)
    mapdeck(
      token = MAPBOX_TOKEN,
      style = mapdeck_style("dark"),
      zoom = 2,
      location = c(-10, 20)
    )
  })
  
  # ---- FAO Zones layer ----
  observe({
    if (input$show_fao_zones) {
      mapdeck_update(map_id = "emissions_map") %>%
        add_polygon(
          data = fao_regions,
          layer_id = "fao_layer",
          fill_colour = "plasma",
          fill_opacity = 1,
          tooltip = "zone",
          update_view = FALSE
        ) %>%
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
  })
  
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
  })
  
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
  })
  
} # END server function





        