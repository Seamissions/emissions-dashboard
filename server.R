# --------------------------------------------------------------------------------
# ---- server --------------------------------------------------------------------
# --------------------------------------------------------------------------------


server <- function(input, output, session) {
  
  # ---- Set up buttons ------------------------------------------------------
  
  # Define action to change to Emissions Map tab
  observeEvent(input$explore_map, {
    updateNavbarPage(session, "navbarPage", selected = "Emissions Map") 
  })
  
  # Define action to change to Seafood Emissions Explorer tab
  observeEvent(input$explore_seafood, {
    updateNavbarPage(session, "navbarPage", selected = "Seafood Emissions Explorer")
  })
  
  # ---- Define color palettes -------------------------------------------------
  blue_palette <- colorRamp(c("#20404F", "#4C9EA6", "#67D6E0", "#76F3FF", "#A9F2FF", "#DAF3FF", "white"))((1:256) / 256)
  pink_palette <- colorRamp(c("#805F14","#CC9921", "#F9B928", "#FFD15F", "#FFECB2","#FFF9D5"))((1:256) / 256)
  
  
  # ---- Emissions map ---------------------------------------------------------
  
  # ---- Initialize default states for map----
  first_time <- reactiveVal(TRUE)
  current_view <- reactiveVal(list(zoom = 3, location = c(0, 0)))
  loading <- reactiveVal(TRUE)
  
  # ---- Pre-calculate initial total emissions for max year ----
  initial_total_broadcasting <- all_emissions %>%
    filter(year == max(all_emissions$year, na.rm = TRUE)) %>%
    summarise(total = sum(emissions_co2_mt, na.rm = TRUE)) %>%
    pull(total)
  
  # ---- Sidebar toggle logic ----
  observeEvent(input$toggle_sidebar, {
    # Toggle the sidebar visibility
    shinyjs::toggle("sidebar-panel")
    
    # Show the outside button and hide the inside button
    shinyjs::toggle("toggle_sidebar_outside")
    shinyjs::toggle("toggle_sidebar")
  })
  
  observeEvent(input$toggle_sidebar_outside, {
    # Toggle the sidebar visibility
    shinyjs::toggle("sidebar-panel")
    
    # Show the inside button and hide the outside button
    shinyjs::toggle("toggle_sidebar")
    shinyjs::toggle("toggle_sidebar_outside")
  })
  # ---- Initialize the map on first render ----
  observe({
    if (first_time() && input$navbarPage == "Emissions Map") {
      
      # Trigger the material switch to be on for broadcasting layer
      updateMaterialSwitch(session, "show_all_countries", value = TRUE)
      
      # Add all_countries (broadcasting emissions) layer on first render
      mapdeck_update(map_id = "emissions_map") %>%
        add_polygon(
          data = all_emissions %>% filter(year == max(all_emissions$year, na.rm = TRUE)),
          layer_id = "all_countries",
          fill_colour = "emissions_co2_mt",
          palette = blue_palette,
          fill_opacity = 0.5,
          tooltip = "emissions_co2_mt",
          update_view = FALSE
        )
      
      # Update the broadcasting total emissions box
      output$total_broadcasting_text <- renderText({
        paste0(format(round(initial_total_broadcasting, 2), big.mark = ","), " Mt CO2")
      })
      
      # After the initial render, set first_time to FALSE to prevent re-running
      first_time(FALSE)
    }
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
          nb_emissions <- readRDS("data/nb_emissions.rds") |> filter(emissions_co2_mt >= 200, year == input$year_slider_input)
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
  
  # ---- Define Loading symbol ----
  output$loading_ui <- renderUI({
    if (loading()) {
      tags$div(
        id = "loading-overlay",
        style = "position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1001;
        color: #08C4E5;",
        icon("spinner", class = "fa-spin", style = "font-size: 40px; margin-bottom: 10px;"),
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
    req(input$show_all_countries,
        input$year_slider_input)
    
    
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
    }) # END isolate
  }) # END observe
  
  # END Emissions Map
  
  output$example_barplot <- renderPlot({
    # Fake data
    df <- data.frame(
      country = c("China", "USA", "Japan","Iceland","Argentina", "Australia"),
      emissions = c(500, 175, 150, 105, 75, 73)
    )
    
    # Simple bar plot
    ggplot(df, aes(x = emissions, y = reorder(country, emissions))) +
      geom_bar(stat = "identity", fill = "#08C4E5") +
      labs(x = "Emissions", y = "") +
      theme_void() +
      theme(legend.position = "none",
            axis.title = element_text(color = "white",
                                      family = "Roboto",
                                      face = "bold",
                                      size = 18),
            axis.text = element_text(color = "white",
                                     family = "Roboto",
                                     face = "bold",
                                     size = 14),
            panel.background = element_rect(fill = "#053762", color = NA),
            plot.background = element_rect(fill = "#053762", color = NA))
  })
  
  
  
}

# END Seafood Emissions Explorer

# END server function


        