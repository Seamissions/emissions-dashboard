home_ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .hero-banner {
        background-image: url('images/home-image.png');
        background-size: cover;
        background-position: center;
        height: 500px;
        position: relative;
      }

      .hero-content {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        text-align: center;
        color: white;
        padding: 20px;
        background-color: rgba(0, 0, 0, 0.5);
      }

      .cta-button {
        display: inline-block;
        padding: 10px 20px;
        background-color: #007bff;
        color: white;
        text-decoration: none;
        border-radius: 5px;
      }
    "))
  ),
  div(class = "hero-banner",
      div(class = "hero-content",
          h1("Illuminating Global Fishing Emissions"),
          h4("Connecting data to action for sustainable oceans."),
          actionButton("explore_map", "Explore Emissions Map", class = "cta-button")
      )
  )
)


