# ---- Server page for emissions dashboard -------------------------------------

server <- function(input, output, session) {
  
  # ---- Set up home page ------------------------------------------------------
  observeEvent(input$explore_map, {
    updateNavbarPage(session, "navbarPage", selected = "Emissions Map")
  })
  
  # ---- Set up map viewer -----------------------------------------------------
  
  # ---- Define color palettes ----
  blue_palette <- colorRamp(c("#20404F", "#4C9EA6", "#67D6E0", "#76F3FF", "#A9F2FF", "#DAF3FF", "white"))((1:256) / 256)
  pink_palette <- colorRamp(c( "#805F14","#CC9921", "#F9B928", "#FFD15F", "#FFECB2","#FFF9D5" ))((1:256) / 256)
  
  # ---- Set initial view ----
  first_time <- reactiveVal(TRUE)
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
  
  # ---- Toggle legends visibility and total emissions ----
  observe({
    shinyjs::toggle("broadcasting_legend", condition = input$show_all_countries)
    shinyjs::toggle("broadcasting_total", condition = input$show_all_countries)
  })
  
  observe({
    shinyjs::toggle("non_broadcasting_legend", condition = input$show_non_broadcasting)
    shinyjs::toggle("non_broadcasting_total", condition = input$show_non_broadcasting)
  })
  
  # ---- Reset total emissions on tab switch ----
  observe({
    if (input$navbarPage == "Emissions Map") {
      if (input$show_all_countries) {
        output$total_broadcasting <- renderText({
          total <- broadcasting_total()
          paste0(format(round(total, 2), big.mark = ","), " Mt CO2")
        })
      }
      if (input$show_non_broadcasting) {
        output$total_non_broadcasting <- renderText({
          nb_emissions <- readRDS("data/nb_emissions.rds") %>% filter(emissions_co2_mt >= 200, year == input$year_slider_input)
          total <- sum(nb_emissions$emissions_co2_mt, na.rm = TRUE)
          paste0(format(round(total, 2), big.mark = ","), " Mt CO2")
        })
      }
    }
  })
  
  # ---- Toggle visibility of country select input ----
  observeEvent(input$show_all_countries, {
    shinyjs::toggle("country_select", condition = input$show_all_countries)
    if (!input$show_all_countries) {
      updateSelectInput(session, "country_select", selected = "")
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "all_countries") %>%
        clear_polygon(layer_id = "country_layer")
    }
  }, priority = 10)
  
  # ---- Filter emissions by country and year ----
  country_filtered <- reactive({
    req(input$country_select, input$year_slider_input)
    country_emissions %>% filter(flag == input$country_select, year == input$year_slider_input)
  })
  
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
          data = all_emissions %>% filter(year == input$year_slider_input),
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
    mapdeck_update(map_id = "emissions_map") %>%
      clear_polygon(layer_id = "non_broadcasting_layer")
    if (input$show_non_broadcasting) {
      loading(TRUE)
      nb_emissions <- readRDS("data/nb_emissions.rds") %>% filter(emissions_co2_mt >= 200, year == input$year_slider_input)
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
    }
  })
  
  # ---- Calculate total emissions ----
  broadcasting_total <- reactive({
    req(input$show_all_countries, input$year_slider_input)
    if (!is.null(input$country_select) && input$country_select != "") {
      filtered <- country_filtered()
      sum(filtered$emissions_co2_mt, na.rm = TRUE)
    } else {
      sum(all_emissions$emissions_co2_mt[all_emissions$year == input$year_slider_input], na.rm = TRUE)
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
}

# END server function



        