# --------------------------------------------------------------------------------
# ---- server --------------------------------------------------------------------
# --------------------------------------------------------------------------------


server <- function(input, output, session) {
  
  # ---- Set up buttons ------------------------------------------------------
  
  # Define action to change to Emissions Map tab
  observeEvent(input$explore_map, {
    updateNavbarPage(session, "navbarPage", selected = "Emissions Map") 
  }) # END observeEvent (Select Emission)
  
  # Define action to change to Seafood Emissions Explorer tab
  observeEvent(input$explore_seafood, {
    updateNavbarPage(session, "navbarPage", selected = "Seafood Emissions Explorer")
  })
  
  # ---- Define color palettes -------------------------------------------------
  blue_palette <- colorRamp(c("#20404F", "#4C9EA6", "#67D6E0", "#76F3FF", "#A9F2FF", "#DAF3FF", "white"))((1:256) / 256)
  orange_palette <- colorRamp(c("#805F14","#CC9921", "#F9B928", "#FFD15F", "#FFECB2","#FFF9D5"))((1:256) / 256)
  green_palette <- colorRamp(c("#2A782A","#34CF46","#41FF58", "#92FF98","#C0FFC7","#ECFFE5"))((1:256) / 256)
  pink_palette <- colorRamp(c("#9E3E74","#D4539C","#FF63BB", "#FF9AD6","#FFD9D7","#FFF9F9"))((1:256) / 256)
  
  
  # ---- Emissions map ---------------------------------------------------------
  
  # ---- Initialize default states for map----
  first_time <- reactiveVal(TRUE)
  current_view <- reactiveVal(list(zoom = 3, location = c(0, 0)))
  loading <- reactiveVal(TRUE)
  
  # Pre-calculate total emissions for the initial year (max year)
  initial_total_broadcasting <- broadcasting_emissions %>%
    filter(country_name == "All Countries", year == max(year, na.rm = TRUE)) %>%
    summarise(total = sum(emissions_co2_mt, na.rm = TRUE)) %>%
    pull(total)


  # ---- Sidebar toggle logic ----
  observeEvent(input$toggle_sidebar_open_input, {
    # Toggle the sidebar visibility
    shinyjs::toggle("sidebar-panel")
    
    # Show the outside button and hide the inside button
    shinyjs::toggle("toggle_sidebar_close_input")
    shinyjs::toggle("toggle_sidebar_open_input")
  })
  
  observeEvent(input$toggle_sidebar_close_input, {
    # Toggle the sidebar visibility
    shinyjs::toggle("sidebar-panel")
    
    # Show the inside button and hide the outside button
    shinyjs::toggle("toggle_sidebar_open_input")
    shinyjs::toggle("toggle_sidebar_close_input")
  })
  # ---- Initialize the map on first render ----
  observe({
    if (first_time() && input$navbarPage == "Emissions Map") {
      
      # Trigger the material switch to be on for broadcasting layer
      updateMaterialSwitch(session, "show_broadcasting_input", value = TRUE)
      
      # Add "All Countries" layer on first render
      mapdeck_update(map_id = "emissions_map") %>%
        add_polygon(
          data = broadcasting_emissions %>% filter(country_name == "All Countries", year == max(year, na.rm = TRUE)),
          layer_id = "all_countries",
          fill_colour = "emissions_co2_mt",
          palette = blue_palette,
          fill_opacity = 0.5,
          tooltip = "emissions_co2_mt",
          update_view = FALSE
        )
      
      # Update the broadcasting total emissions box
      output$total_broadcasting_text <- renderText({
        paste0(format(round(initial_total_broadcasting, 2), big.mark = ","), " MT CO₂")
      })
      
      # After the initial render, set first_time to FALSE to prevent re-running
      first_time(FALSE)
    }
  })
  
  # ---- Toggle legends visibility and total emissions ----
  observe({
    shinyjs::toggle("broadcasting_legend", condition = input$show_broadcasting_input)
    shinyjs::toggle("broadcasting_total", condition = input$show_broadcasting_input)
  })
  
  observe({
    shinyjs::toggle("non_broadcasting_legend", condition = input$show_non_broadcasting_input)
    shinyjs::toggle("non_broadcasting_total", condition = input$show_non_broadcasting_input)
  })
  
  # ---- Reset total emissions on tab switch ----
  observe({
    if (input$navbarPage == "Emissions Map") {
      if (input$show_broadcasting_input) {
        output$total_broadcasting <- renderText({
          total <- broadcasting_total()
          paste0(format(round(total, 2), big.mark = ","), " Mt CO2")
        })
      }
      if (input$show_non_broadcasting_input) {
        output$total_non_broadcasting <- renderText({
          nb_emissions <- readRDS("data/nb_emissions.rds") |> filter(emissions_co2_mt >= 200, year == input$year_slider_input)
          total <- sum(nb_emissions$emissions_co2_mt, na.rm = TRUE)
          paste0(format(round(total, 2), big.mark = ","), " Mt CO2")
        })
      }
    }
  })
  
  # ---- Toggle visibility of country select input ----
  observeEvent(input$show_broadcasting_input, {
    shinyjs::toggle("country_select_input", condition = input$show_broadcasting_input)
    if (!input$show_broadcasting_input) {
      updateSelectInput(session, "country_select_input", selected = "")
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "all_countries") %>%
        clear_polygon(layer_id = "country_layer")
    }
  }, priority = 10)
  
  # ---- Filter emissions by country and year ----
country_filtered <- reactive({
    req(input$country_select_input, input$year_slider_input)
    broadcasting_emissions %>%
      filter(country_name == input$country_select_input, year == input$year_slider_input)
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
        icon("spinner", class = "fa-spin", style = "font-size: 60px; margin-bottom: 10px;"))
    }
  })
  
  # ---- All countries & country emissions layers ----
  observe({
    req(input$show_broadcasting_input)
    loading(TRUE)
    
    if (is.null(input$country_select_input) || input$country_select_input == "") {
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "country_layer") %>%
        add_polygon(
          data = broadcasting_emissions %>% filter(country_name == "All Countries", year == input$year_slider_input),
          layer_id = "all_countries",
          fill_colour = "emissions_co2_mt",
          palette = blue_palette,
          fill_opacity = 0.5,
          tooltip = "emissions_co2_mt",
          update_view = FALSE
        )
    } else {
      filtered <- country_filtered()
      if (nrow(filtered) > 0) {
        mapdeck_update(map_id = "emissions_map") %>%
          clear_polygon(layer_id = "all_countries") %>%
          add_polygon(
            data = filtered,
            layer_id = "country_layer",
            fill_colour = "emissions_co2_mt",
            palette = blue_palette,
            fill_opacity = 0.6,
            tooltip = "emissions_co2_mt",
            update_view = FALSE
          )
      } else {
        # Clear both layers and show no data message
        mapdeck_update(map_id = "emissions_map") %>%
          clear_polygon(layer_id = "all_countries") %>%
          clear_polygon(layer_id = "country_layer")
      }
    }
    
    later::later(function() { loading(FALSE) }, delay = 0.2)
  })
  
  
  
  # ---- Non-broadcasting emissions layer ----
  observe({
    mapdeck_update(map_id = "emissions_map") %>%
      clear_polygon(layer_id = "non_broadcasting_layer")
    if (input$show_non_broadcasting_input) {
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
  
  # ---- Calculate total emissions for the selected year and country ----
  broadcasting_total <- reactive({
    req(input$year_slider_input)
    
    if (!is.null(input$country_select_input) &&
        input$country_select_input != "All Countries" &&
        input$country_select_input != "") {
      
      # If a specific country is selected, sum emissions for that country and year
      filtered <- country_filtered()
      sum(filtered$emissions_co2_mt, na.rm = TRUE)
      
    } else {
      # If "All Countries" or no country is selected, use the pre-aggregated row
      total <- broadcasting_emissions %>%
        filter(country_name == "All Countries",
               year == input$year_slider_input) %>%
        summarise(total = sum(emissions_co2_mt, na.rm = TRUE)) %>%
        pull(total)
      
      total
    }
  })
  
  # ---- Add no data warning ----
  output$no_data_warning <- renderText({
    filtered <- country_filtered()
    if (nrow(filtered) == 0) {
      "⚠️ This country does not have emissions for the selected year. Please pick another country or year."
    } else {
      ""  # No message if data is present
    }
  })
  
  
  # ---- Initialize emissions map ----
  output$emissions_map <- renderMapdeck({
    loading(TRUE)
    later::later(function() { loading(FALSE) }, delay = 0.)
    mapdeck(
      token = MAPBOX_TOKEN,
      style = mapdeck_style("dark"),
      zoom = 2,
      location = c(-10, 20)
    )
  })
  
  # ---- FAO Zones layer ----
  observe({
    if (input$show_fao_zones_input) {
      loading(TRUE)
      
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
      
      later::later(function() { loading(FALSE) }, delay = 0.2)
      
    } else {
      mapdeck_update(map_id = "emissions_map") %>%
        clear_polygon(layer_id = "fao_layer") %>%
        clear_path(layer_id = "fao_border_layer")
    }
  })
  
  # ---- Track view so we don’t reset zoom/location ----
  observe({
    input$emissions_map_view_state_input
    isolate({
      view <- input$emissions_map_view_state_input
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
    
    show_per_unit <- input$unit_plot_toggle_input
    
    x_var <- if (isTRUE(show_per_unit)) {
      top_flags$emissions_per_ton
    } else {
      top_flags$sum_emissions
    }
    
    x_label <- if (isTRUE(show_per_unit)) {
      paste0(comma(top_flags$emissions_per_ton), " MT")
    } else {
      paste0(comma(top_flags$sum_emissions), " MT")
    }
    
    max_x <- max(x_var, na.rm = TRUE)
    
    ggplot(data = top_flags) +
      geom_col(aes(x = x_var,
                   y = reorder(country_name, sum_emissions)),
               fill = "#08C4E5") +
      geom_flag(aes(x = 0,
                    y = reorder(country_name,
                                sum_emissions),
                    country = iso2),size = 15) +
      geom_text(aes(x = x_var + 0.09 * max_x,
                    y = reorder(country_name, sum_emissions),
                    label = x_label),
        color = "white",
        size = 7) +
      
      labs(title = "Annual CO₂ Emissions from Top Fishing Fleets") +
      
      theme_void() +
      theme(legend.position = "none",
        title = element_text(color = "white",
                             family = "Roboto",
                             face = "bold",
                             size = 24),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = "white",
                                   size = 22,
                                   hjust = 1,
                                   margin = margin(r = -5)),
        panel.background = element_rect(fill = "#053762", color = NA),
        plot.background = element_rect(fill = "#053762", color = NA)) +
      expand_limits(
        x = c(-0.05 * max_x, 1.2 * max_x))
  
  })
  
}

# END Seafood Emissions Explorer

# END server function


        