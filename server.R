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
  observe({
    shinyjs::onclick("learn_more_link", {
      updateNavbarPage(session, "navbarPage", selected = "Learn More")
    })
    
  })
  
  observe({
    shinyjs::onclick("learn_more_link2", {
      updateNavbarPage(session, "navbarPage", selected = "Learn More")
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
  
  # ---- Map Sidebar toggle logic ---------------------------------------
  observeEvent(input$toggle_sidebar_open_input, {
    # Toggle the sidebar visibility
    shinyjs::toggle("sidebar-panel")
    
    # Show the outside button and hide the inside button
    shinyjs::toggle("toggle_sidebar_close_input", anim = TRUE,
                    animType = "fade")
    shinyjs::toggle("toggle_sidebar_open_input", anim = TRUE,
                    animType = "fade")
  })
  
  observeEvent(input$toggle_sidebar_close_input, {
    # Toggle the sidebar visibility
    shinyjs::toggle("sidebar-panel", anim = TRUE,
                    animType = "fade")
    
    # Show the inside button and hide the outside button
    shinyjs::toggle("toggle_sidebar_open_input", anim = TRUE,
                    animType = "fade")
    shinyjs::toggle("toggle_sidebar_close_input", anim = TRUE,
                    animType = "fade")
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
    
    show_per_unit <- input$unit_plot_toggle_input =="per_unit"
    
    filtered_flags <- top_flags |>
      filter(year == input$year_slider_input_plot)
    
    # Remove observations with 0 for the selected year
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
    
    # ---- Dynamic axis breaks and labels ----
    if (!show_per_unit) {
      # ---- Total Emissions: round down to last full 10M ----
      raw_max <- max(x_var, na.rm = TRUE)
      max_x_axis <- floor(raw_max / 1e7) * 1e7  # round down
      min_x_axis <- floor(min(x_var, na.rm = TRUE) / 1e7) * 1e7
      x_breaks <- seq(min_x_axis, max_x_axis, by = 1e7)
      x_labels <- paste0(x_breaks / 1e6, "M")
      
    } else {
      # ---- Per Unit Emissions: round down to last full unit ----
      raw_max <- max(x_var, na.rm = TRUE)
      max_x_axis <- floor(raw_max)  # round down to whole number
      x_breaks <- seq(0, max_x_axis, by = 1)
      x_labels <- x_breaks
    }
    
    filtered_flags$x_var <- x_var
    filtered_flags$x_label <- x_label
    
    ggplot(data = filtered_flags) +
      
      geom_vline(xintercept = x_breaks,
                 linetype = "dotted",
                 color = "#AAAAAA",
                 linewidth = 0.3) +
      
      scale_x_continuous(
        breaks = x_breaks,
        labels = x_labels,
        expand = c(0, 0),
        position = "top") +
      
      annotate("segment",
               x =  0,
               xend = max_x,
               y = Inf,
               yend = Inf,
               color = "white",
               linewidth = 0.5) +
      geom_col(aes(x = x_var,
                   y = reorder(country_name, sum_emissions)),
               fill = "#08C4E5") +
      ggflags::geom_flag(aes(x = 0,
                             y = reorder(country_name, sum_emissions),
                             country = iso2), size = 15) +
      geom_text(aes(x = x_var + 0.15 * max_x,
                    y = reorder(country_name, sum_emissions),
                    label = x_label),
                color = "white",
                size = 7) +
      theme_void() +
      theme(
        legend.position = "none",
        title = element_text(color = "white", family = "Roboto", face = "bold", size = 24),
        axis.text.x = element_text(color = "white", size = 20, family = "Roboto", margin = margin(t = 10)),
        axis.text.y = element_text(color = "white", size = 22, hjust = 1, margin = margin(r = -5)),
        panel.background = element_rect(fill = "#0B2232", color = NA),
        plot.background = element_rect(fill = "#0B2232", color = NA)) +
      expand_limits(x = c(-0.1 * max_x, 1.5 * max_x))
  }, res = 100)
  
  # --- Plot comparing ISSCAAP groups ----
  output$isscaap_plot_output <- renderPlot({
    
    show_per_unit <- input$unit_plot_toggle_input =="per_unit"
    
    filtered_isscaap <- top_isscaap |>
      filter(year == input$year_slider_input_plot)

    
    
    x_var <- if (isTRUE(show_per_unit)) {
      filtered_isscaap$emissions_per_ton
    } else {
      filtered_isscaap$sum_emissions
    }
    
    x_label <- if (isTRUE(show_per_unit)) {
      paste0(comma(filtered_isscaap$emissions_per_ton))
    } else {
      paste0(comma(filtered_isscaap$sum_emissions))
    }
    
    # x_var is already set above correctly
    max_x <- max(x_var, na.rm = TRUE)
    
    # ---- Dynamic axis breaks and labels ----
    if (!show_per_unit) {
      # ---- Total Emissions: round down to last full 10M ----
      raw_max <- max(x_var, na.rm = TRUE)
      max_x_axis <- floor(raw_max / 1e7) * 1e7  # round down
      min_x_axis <- floor(min(x_var, na.rm = TRUE) / 1e7) * 1e7
      x_breaks <- seq(min_x_axis, max_x_axis, by = 1e7)
      x_labels <- paste0(x_breaks / 1e6, "M")
      
    } else {
      # ---- Per Unit Emissions: round down to last full unit ----
      raw_max <- max(x_var, na.rm = TRUE)
      max_x_axis <- floor(raw_max)  # round down to whole number
      x_breaks <- seq(0, max_x_axis, by = 1)
      x_labels <- x_breaks
    }
      
    # ---- Create plot -------------------------------------------
    ggplot(data = filtered_isscaap) +
      geom_vline(xintercept = x_breaks,
                 linetype = "dotted",
                 color = "#AAAAAA",
                 linewidth = 0.3) +
      
      geom_col(aes(x = x_var,
                   y = reorder(isscaap_group, sum_emissions)),
               fill = "#08C4E5") +
      
      # Add image aligned under the bar
      geom_image(aes(
        x = 0,
        y = reorder(isscaap_group, sum_emissions),
        image = image
      ), size = .12, asp = 1) +  # Adjust size/asp as needed
      
      geom_text(aes(x = x_var + 0.12 * max_x,
                    y = reorder(isscaap_group, sum_emissions),
                    label = x_label),
                color = "white",
                size = 7) +
      
      scale_x_continuous(
        breaks = x_breaks,
        labels = x_labels,
        expand = c(0, 0),
        position = "top") +
      
      annotate("segment",
               x =  0,
               xend = max_x,
               y = Inf,
               yend = Inf,
               color = "white",
               linewidth = 0.5) +
      
      theme_void() +
      theme(
        legend.position = "none",
        title = element_text(color = "white", family = "Roboto", face = "bold", size = 24),
        axis.text.x = element_text(color = "white", size = 20, family = "Roboto", margin = margin(t = 10)),
        axis.text.y = element_text(color = "white", size = 22, hjust = 1, margin = margin(r = -5)),
        panel.background = element_rect(fill = "#0B2232", color = NA),
        plot.background = element_rect(fill = "#0B2232", color = NA)
      ) +
      expand_limits(x = c(-0.1 * max_x, 1.5 * max_x))
  }, res = 100)
  
  
  # --- Plot for selected country ISSCAAP groups -------------------------------
  
  output$dynamic_country_header <- renderUI({
    if (is.null(input$selected_country_input) || input$selected_country_input == "" || input$selected_country_input == "All Countries") {
      div(style = "width: 100%; text-align: center;",
          tags$h4(
            HTML("&#8593; Please select a country."),
            style = "color: #08C4E5;
             font-size: 20px;
             font-weight: bold;
             white-space: normal;
             word-break: break-word;
             margin-bottom: 10px;"
          )
      )
    } else {
      div(style = "width: 100%; text-align: center;",
          tags$div(input$selected_country_input,
                   style = "color: #DA8D03;
                          font-size: 28px;
                          font-weight: bold;
                          margin-bottom: 4px;"
          ),
          tags$div("Annual CO₂ Emissions by Species Group",
                   style = "color: white;
                          font-size: 24px;
                          font-weight: normal;"
          )
      )
    }
  })
  
  
  # ---- Selected County ISSCAAP Plots ---
  # ---- Dynamic UI with height based on row count ----
  output$species_bar_plot_ui <- renderUI({
    req(input$selected_country_input)
    
    filtered_data <- species_data |>
      filter(
        country_name == input$selected_country_input,
        year == input$year_slider_input_plot
      )
    
    # Estimate height: 60px per bar + 100px padding
    dynamic_height <- 60 * nrow(filtered_data) + 100
    
    # Render plotOutput with dynamically calculated height
    plotOutput("species_bar_plot_output", height = paste0(dynamic_height, "px"))
  })
  
  # ---- Render Plot ----
  output$species_bar_plot_output <- renderPlot({
    req(input$selected_country_input)
    
    show_per_unit <- input$unit_plot_toggle_input == "per_unit"
    
    # Filter for selected country/year
    filtered_select_country <- species_data |>
      filter(
        country_name == input$selected_country_input,
        year == input$year_slider_input_plot
      )
    
    validate(
      need(nrow(filtered_select_country) > 0,
           " ⚠️ This country does not have catch reported for the selected year. Please pick another country or year.")
    )
    
    # Number of species for selected country
    num_species <- nrow(filtered_select_country)
    
    image_size <- case_when(
      num_species <= 2  ~ 0.5,
      num_species <= 5  ~ 0.25,
      num_species <= 10  ~ 0.12,
      num_species <= 15 ~ 0.08,
      num_species <= 25 ~ 0.05,
      TRUE              ~ 0.05
    )
    
    
    # Define x values
    x_var <- if (show_per_unit) {
      filtered_select_country$emissions_per_ton
    } else {
      filtered_select_country$sum_emissions
    }
    
    # Define x-axis text labels
    x_label <- if (show_per_unit) {
      paste0(comma(round(filtered_select_country$emissions_per_ton, 2)))
    } else {
      paste0(comma(round(filtered_select_country$sum_emissions, 2)))
    }
    
    # Dynamic axis setup
    raw_max <- max(x_var, na.rm = TRUE)
    raw_min <- min(x_var, na.rm = TRUE)
    
    if (raw_max == raw_min || raw_max == 0 || is.infinite(raw_max)) {
      raw_max <- raw_max + 1
      raw_min <- 0
    }
    
    if (!show_per_unit) {
      base_step <- 10^(floor(log10(raw_max)))
      step <- base_step
      if (step == 0) step <- 1
      repeat {
        candidate_breaks <- seq(0, raw_max, by = step)
        if (length(candidate_breaks) <= 4) break
        step <- step * 2
      }
      x_breaks <- candidate_breaks
      x_labels <- if (max(x_breaks) >= 1e6) paste0(x_breaks / 1e6, "M") else comma(x_breaks)
    } else {
      max_x_axis <- ceiling(raw_max)
      step <- if (max_x_axis <= 4) 1 else ceiling(max_x_axis / 4)
      x_breaks <- seq(0, raw_max, by = step)
      x_labels <- x_breaks
    }
    
    max_x <- raw_max
    
    # ---- ggplot ----
    ggplot(data = filtered_select_country) +
      geom_vline(xintercept = x_breaks, linetype = "dotted", color = "#AAAAAA", linewidth = 0.3) +
      geom_col(aes(x = x_var, y = reorder(isscaap_group, sum_emissions)), fill = "#08C4E5") +
      
      # Dynamically sized image icons
      geom_image(
        aes(x = 0, y = reorder(isscaap_group, sum_emissions), image = image),
        size = image_size,
        asp = 1
      ) +
      
      geom_text(
        aes(x = x_var + 0.15 * max_x,
            y = reorder(isscaap_group, sum_emissions),
            label = x_label),
        color = "white",
        size = 7
      ) +
      
      scale_x_continuous(
        breaks = x_breaks,
        labels = x_labels,
        expand = c(0, 0),
        position = "top"
      ) +
      
      annotate("segment",
               x = 0,
               xend = max_x,
               y = Inf,
               yend = Inf,
               color = "white",
               linewidth = 0.5) +
      
      theme_void() +
      theme(
        legend.position = "none",
        title = element_text(color = "white", family = "Roboto", face = "bold", size = 24),
        axis.text.x = element_text(color = "white", size = 20, family = "Roboto", margin = margin(t = 10)),
        axis.text.y = element_text(color = "white", size = 22, hjust = 1, margin = margin(r = -5)),
        panel.background = element_rect(fill = "#0B2232", color = NA),
        plot.background = element_rect(fill = "#0B2232", color = NA)
      ) +
      expand_limits(x = c(-0.05 * max_x, 1.5 * max_x))
  }, res = 100)
  
  # ---- Emissions total text ----
  output$selected_country_total <- renderText({
    req(input$selected_country_input)
    
    total <- species_data |>
      filter(country_name == input$selected_country_input) |>
      summarize(total = sum(sum_emissions, na.rm = TRUE)) |>
      pull(total)
    
    paste0(format(round(total, 2), big.mark = ","), " MT CO₂")
  })
  
  
  

# END Seafood Emissions Explorer

} # END server function





        