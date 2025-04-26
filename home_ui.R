# ---- home_ui.R ----

home_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Hero section
    div(
      style = "background-image: url('www/ocean_background.jpg'); background-size: cover; padding: 150px 0; text-align: center; color: white;",
      h1("Illuminating Global Fishing Emissions"),
      h4("Connecting data to action for sustainable oceans."),
      actionButton(ns("explore_map"), "Explore Emissions Map", class = "btn-primary btn-lg")
    ),
    
    # Intro section
    fluidRow(
      column(4,
             icon("globe", "fa-3x"),
             h3("Quantify"),
             p("Measure fishing vessel emissions worldwide.")
      ),
      column(4,
             icon("map", "fa-3x"),
             h3("Visualize"),
             p("Interactive maps to explore spatial trends.")
      ),
      column(4,
             icon("bullhorn", "fa-3x"),
             h3("Inform"),
             p("Support sustainable ocean policy with data-driven insights.")
      )
    ),
    
    # Map teaser
    div(
      style = "padding: 50px 0; text-align: center;",
      img(src = "www/map_preview.jpg", style = "width: 80%; max-width: 800px;"),
      p("Discover patterns and hotspots across global fishing fleets.")
    ),
    
    # Impact stats
    fluidRow(
      column(4, h2("1.5M km²"), p("Monitored ocean area")),
      column(4, h2("200K+ vessels"), p("Analyzed for emissions")),
      column(4, h2("20 years"), p("Of global data coverage"))
    )
  )
}
