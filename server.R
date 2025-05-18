# --------------------------------------------------------------------------------
# ---- server --------------------------------------------------------------------
# --------------------------------------------------------------------------------


server <- function(input, output, session) {
  
  # ---- Set up buttons ------------------------------------------------------
  
  observe({
    shinyjs::onclick("explore_map_card", {
      updateNavbarPage(session, "navbarPage", selected = "Emissions Map")
    })
    
    shinyjs::onclick("explore_seafood_card", {
      updateNavbarPage(session, "navbarPage", selected = "Compare Seafood Emissions")
    })
  })
  
  
  # ----------------------------------------------------------------------------
  # ---- Emissions map ---------------------------------------------------------
  #-----------------------------------------------------------------------------
  
  # ---- Initialize default states for map----
  first_time <- reactiveVal(TRUE)
  current_view <- reactiveVal(list(zoom = 3, location = c(0, 0)))
  loading <- reactiveVal(TRUE)
  nb_emissions <- readRDS("data/nb_emissions.rds") |>
    mutate(tooltip_text = paste0(scales::comma(round(emissions_co2_mt, 0)), " MT CO₂"))
  
  # Pre-calculate total emissions for the initial year (max year)
  initial_total_broadcasting <- broadcasting_emissions |>
    filter(country_name == "All Countries", year == max(year, na.rm = TRUE)) |>
    summarise(total = sum(emissions_co2_mt, na.rm = TRUE)) |>
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

  # Auto minimize sidebar panel for small screens/mobile devices
  observe({
    req(input$minimize_sidebar_on_mobile)
    req(input$navbarPage == "Emissions Map")
    shinyjs::hide("sidebar-panel")
    shinyjs::show("toggle_sidebar_close_input")
    shinyjs::hide("toggle_sidebar_open_input")
  })
  
            
  # ---- Initialize the map on first render ----
  observe({
    if (first_time() && input$navbarPage == "Emissions Map") {
      
      # Trigger the material switch to be on for broadcasting layer
      updateMaterialSwitch(session, "show_broadcasting_input", value = TRUE)
      
      # Add "All Countries" layer on first render
      mapdeck_update(map_id = "emissions_map") |>
        add_polygon(
          data = broadcasting_emissions |> filter(country_name == "All Countries", year == max(year, na.rm = TRUE)),
          layer_id = "all_countries",
          fill_colour = "emissions_co2_mt",
          palette = blue_palette,
          fill_opacity = 0.5,
          tooltip = "tooltip_text",
          auto_highlight = TRUE,
          highlight_colour = "#F8FDFF50",
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
          nb_emissions <- nb_emissions |> filter(emissions_co2_mt >= 200, year == input$year_slider_input_map)
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
      mapdeck_update(map_id = "emissions_map") |>
        clear_polygon(layer_id = "all_countries") |>
        clear_polygon(layer_id = "country_layer")
    }
  }, priority = 10)
  
  # ---- Filter emissions by country and year ----
  country_filtered <- reactive({
    req(input$country_select_input, input$year_slider_input_map)
    broadcasting_emissions |>
      filter(country_name == input$country_select_input, year == input$year_slider_input_map)
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
      mapdeck_update(map_id = "emissions_map") |>
        clear_polygon(layer_id = "country_layer") |>
        add_polygon(
          data = broadcasting_emissions |> filter(country_name == "All Countries", year == input$year_slider_input_map),
          layer_id = "all_countries",
          fill_colour = "emissions_co2_mt",
          palette = blue_palette,
          fill_opacity = 0.5,
          tooltip = "tooltip_text",
          auto_highlight = TRUE,
          highlight_colour = "#F8FDFF50",
          update_view = FALSE
        )
    } else {
      filtered <- country_filtered()
      if (nrow(filtered) > 0) {
        mapdeck_update(map_id = "emissions_map") |>
          clear_polygon(layer_id = "all_countries") |>
          add_polygon(
            data = filtered,
            layer_id = "country_layer",
            fill_colour = "emissions_co2_mt",
            palette = blue_palette,
            tooltip = "tooltip_text",
            auto_highlight = TRUE,
            highlight_colour = "#F8FDFF50",
            update_view = FALSE
          )
      } else {
        # Clear both layers and show no data message
        mapdeck_update(map_id = "emissions_map") |>
          clear_polygon(layer_id = "all_countries") |>
          clear_polygon(layer_id = "country_layer")
      }
    }
    
    later::later(function() { loading(FALSE) }, delay = 0.2)
  })
  
  # ---- Non-broadcasting emissions layer ----
  observe({
    mapdeck_update(map_id = "emissions_map") |>
      clear_polygon(layer_id = "non_broadcasting_layer")
    if (input$show_non_broadcasting_input) {
      loading(TRUE)
      nb_emissions <- nb_emissions |> filter(emissions_co2_mt >= 200, year == input$year_slider_input_map)
      mapdeck_update(map_id = "emissions_map") |>
        add_polygon(
          layer_id = "non_broadcasting_layer",
          data = nb_emissions,
          fill_colour = "emissions_co2_mt",
          palette = orange_palette,
          fill_opacity = 0.5,
          tooltip = "tooltip_text",
          auto_highlight = TRUE,
          highlight_colour = "#F8FDFF50",
          update_view = FALSE
        )
      later::later(function() { loading(FALSE) }, delay = 0.2)
    }
  })
  
  # ---- Calculate total emissions for the selected year and country ----
  broadcasting_total <- reactive({
    req(input$year_slider_input_map)
    
    if (!is.null(input$country_select_input) &&
        input$country_select_input != "All Countries" &&
        input$country_select_input != "") {
      
      # If a specific country is selected, sum emissions for that country and year
      filtered <- country_filtered()
      sum(filtered$emissions_co2_mt, na.rm = TRUE)
      
    } else {
      # If "All Countries" or no country is selected, use the pre-aggregated row
      total <- broadcasting_emissions |>
        filter(country_name == "All Countries",
               year == input$year_slider_input_map) |>
        summarise(total = sum(emissions_co2_mt, na.rm = TRUE)) |>
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
  
  # ---- Add low emissions warning ----
  output$low_emissions_warning <- renderText({
    filtered <- country_filtered()
    total <- sum(filtered$emissions_co2_mt, na.rm = TRUE)
    filtered <- country_filtered()
    if (total > 0 && total < 5000) {
      "⚠️ Emissions for this country are low in the selected year and may be difficult to spot on the map. Try scanning the map closely or selecting a different country or year."
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
  # ---- FAO Zones layer ----
  observe({
    if (input$show_fao_zones_input) {
      loading(TRUE)
      
      mapdeck_update(map_id = "emissions_map") |>
        add_polygon(
          data = fao_regions,
          layer_id = "fao_layer",
          fill_colour = "plasma",
          fill_opacity = 1,
          tooltip = "zone",
          update_view = FALSE
        ) |>
        add_path(
          data = fao_borders,
          layer_id = "fao_border_layer",
          stroke_color = "zone",
          palette = fao_zone_color,
          stroke_width = 4,
          update_view = FALSE
        ) 
      
      later::later(function() { loading(FALSE) }, delay = 0.2)
      
    } else {
      mapdeck_update(map_id = "emissions_map") |>
        clear_polygon(layer_id = "fao_layer") |>
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
  
  
  # ----------------------------------------------------------------------------
  # ---- Compare emissions plots ------------------------------------------------
  # ----------------------------------------------------------------------------
  
  useShinyjs()
  
  # ---- Set default active button and plot on load ----
  runjs("$('#compare_countries_input').addClass('plot-button-active');")
  shinyjs::show("country_plot")
  shinyjs::hide("isscaap_plot")
  shinyjs::hide("species_bar_plot")
  shinyjs::hide("country_select_plot_input")
  shinyjs::hide("dynamic_country_header")
  
  # ---- Toggle barplots on button click ----
  observeEvent(input$compare_species_input, {
    shinyjs::show("isscaap_plot")
    shinyjs::hide("country_plot")
    shinyjs::hide("species_bar_plot")
    shinyjs::hide("country_select_plot_input")
    shinyjs::hide("dynamic_country_header")
    
    runjs("
    $('#compare_species_input').addClass('plot-button-active');
    $('#compare_countries_input').removeClass('plot-button-active');
    $('#select_country_input').removeClass('plot-button-active');
  ")
  })
  
  observeEvent(input$compare_countries_input, {
    shinyjs::show("country_plot")
    shinyjs::hide("isscaap_plot")
    shinyjs::hide("species_bar_plot")
    shinyjs::hide("country_select_plot_input")
    shinyjs::hide("dynamic_country_header")

    runjs("
    $('#compare_species_input').removeClass('plot-button-active');
    $('#compare_countries_input').addClass('plot-button-active');
    $('#select_country_input').removeClass('plot-button-active');
  ")
  })
  
  observeEvent(input$select_country_input, {
    shinyjs::hide("country_plot")
    shinyjs::hide("isscaap_plot")
    shinyjs::show("species_bar_plot")
    shinyjs::show("country_select_plot_input")
    shinyjs::show("dynamic_country_header")
    
    runjs("
    $('#compare_species_input').removeClass('plot-button-active');
    $('#compare_countries_input').removeClass('plot-button-active');
    $('#select_country_input').addClass('plot-button-active');
  ")
  })
  
  
  # --- Plot comparing countries ----
  output$country_plot_output <- renderPlot({
    
    show_per_unit <- input$unit_plot_toggle_input
    
    filtered_flags <- top_flags |>
      filter(year == input$year_slider_input_plot)
    
    req(nrow(filtered_flags) > 0)
    
    x_var <- if (isTRUE(show_per_unit)) {
      filtered_flags$emissions_per_ton
    } else {
      filtered_flags$sum_emissions
    }
    
    x_label <- if (isTRUE(show_per_unit)) {
      paste0(comma(filtered_flags$emissions_per_ton), " MT")
    } else {
      paste0(comma(filtered_flags$sum_emissions), " MT")
    }
    
    max_x <- max(x_var, na.rm = TRUE)
    
    filtered_flags$x_var <- x_var
    filtered_flags$x_label <- x_label
    
    ggplot(data = filtered_flags) +
      geom_col(aes(x = x_var,
                   y = reorder(country_name, sum_emissions)),
               fill = "#08C4E5") +
      ggflags::geom_flag(aes(x = 0,
                             y = reorder(country_name, sum_emissions),
                             country = iso2), size = 15) +
      geom_text(aes(x = x_var + 0.09 * max_x,
                    y = reorder(country_name, sum_emissions),
                    label = x_label),
                color = "white",
                size = 7) +
      theme_void() +
      theme(legend.position = "none",
            title = element_text(color = "white", family = "Roboto", face = "bold", size = 24),
            axis.title.x = element_blank(),
            axis.text.y = element_text(color = "white", size = 22, hjust = 1, margin = margin(r = -5)),
            panel.background = element_rect(fill = "#0B2232", color = NA),
            plot.background = element_rect(fill = "#0B2232", color = NA)
      ) +
      expand_limits(x = c(-0.05 * max_x, 1.2 * max_x))
  })
  
  # --- Plot comparing ISSCAAP groups ----
  output$isscaap_plot_output <- renderPlot({
    
    show_per_unit <- input$unit_plot_toggle_input
    
    filtered_isscaap <- top_isscaap |>
      filter(year == input$year_slider_input_plot)
    
    
    x_var <- if (isTRUE(show_per_unit)) {
      filtered_isscaap$emissions_per_ton
    } else {
      filtered_isscaap$sum_emissions
    }
    
    x_label <- if (isTRUE(show_per_unit)) {
      paste0(comma(filtered_isscaap$emissions_per_ton), " MT")
    } else {
      paste0(comma(filtered_isscaap$sum_emissions), " MT")
    }
    
    max_x <- max(x_var, na.rm = TRUE)
    
    ggplot(data = filtered_isscaap) +
      geom_col(aes(x = x_var,
                   y = reorder(isscaap_group, sum_emissions)),
               fill = "#08C4E5") +
      
      # Add image aligned under the bar
      geom_image(aes(
        x = -0.05 * max_x,
        y = reorder(isscaap_group, sum_emissions),
        image = image
      ), size = 0.05, asp = 1.5) +  # Adjust size/asp as needed
      
      geom_text(aes(x = x_var + 0.09 * max_x,
                    y = reorder(isscaap_group, sum_emissions),
                    label = x_label),
                color = "white",
                size = 7) +

      theme_void() +
      theme(
        legend.position = "none",
        title = element_text(color = "white", family = "Roboto", face = "bold", size = 24),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = "white", size = 22, hjust = 1, margin = margin(r = -5)),
        panel.background = element_rect(fill = "#0B2232", color = NA),
        plot.background = element_rect(fill = "#0B2232", color = NA)
      ) +
      expand_limits(x = c(-0.1 * max_x, 1.2 * max_x))
  })
  
  
  # --- Plot for selected country ISSCAAP groups -------------------------------
  
  output$dynamic_country_header <- renderUI({
    if (is.null(input$selected_country_input) || input$selected_country_input == "" || input$selected_country_input == "All Countries") {
      tags$h4(
        "Please select a country.",
        style = "color: white;
               font-size: 30px;
               font-weight: bold;
               white-space: normal;
               word-break: break-word;
               max-width: 100%;
               margin-bottom: 10px;"
      )
    } else {
      tags$h4(
        paste("Annual CO₂ Emissions By Species Group -", input$selected_country_input),
        style = "color: white;
               font-size: 30px;
               font-weight: bold;
               white-space: normal;
               word-break: break-word;
               max-width: 100%;
               margin-bottom: 10px;"
      )
    }
  })
  
  # Create plot
  output$species_bar_plot_output <- renderPlot({
    req(input$selected_country_input)
    
    show_per_unit <- input$unit_plot_toggle_input
    
    # Aggregate across years for selected country
    filtered_select_country <- species_data |>
      filter(country_name == input$selected_country_input,
             year == input$year_slider_input_plot)
    
    
    x_var <- if (isTRUE(show_per_unit)) {
      filtered_select_country$emissions_per_ton
    } else {
      filtered_select_country$sum_emissions
    }
    
    x_label <- if (isTRUE(show_per_unit)) {
      paste0(comma(round(filtered_select_country$emissions_per_ton, 2)), " MT")
    } else {
      paste0(comma(round(filtered_select_country$sum_emissions, 2)), " MT")
    }
    
    
    max_x <- max(x_var, na.rm = TRUE)
    
    ggplot(data = filtered_select_country) +
      geom_col(aes(x = x_var,
                   y = reorder(isscaap_group, sum_emissions)),
               fill = "#08C4E5") +
      geom_text(aes(x = x_var + 0.09 * max_x,
                    y = reorder(isscaap_group, sum_emissions),
                    label = x_label),
                color = "white",
                size = 7) +
      theme_void() +
      theme(
        legend.position = "none",
        title = element_text(color = "white",
                             family = "Roboto",
                             face = "bold",
                             size = 24),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = "white",
                                   size = 22,
                                   hjust = 1,
                                   margin = margin(r = -5)),
        panel.background = element_rect(fill = "#0B2232", color = NA),
        plot.background = element_rect(fill = "#0B2232", color = NA)
      ) +
      expand_limits(x = c(-0.05 * max_x, 1.2 * max_x))
  })
  
  
  # --- Text box for selected country’s total emissions ----
  output$selected_country_total <- renderText({
    req(input$selected_country_input)
    
    total <- species_data |>
      filter(country_name == input$selected_country_input) |>
      summarize(total = sum(sum_emissions, na.rm = TRUE)) |>
      pull(total)
    
    paste0(format(round(total, 2), big.mark = ","), " MT CO₂")
  })
  
  
}

# END Seafood Emissions Explorer

# END server function



        