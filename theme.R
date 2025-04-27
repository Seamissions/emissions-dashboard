# theme.R
library(bslib)

seamissions_theme <- bs_theme(
  version = 5,  # Bootstrap version
  bg = "#053762",      # Background color
  fg = "#053762",       # Foreground (text) color
  primary = "#08C4E5",  # Primary color
  secondary = "orange",
  base_font = font_google("Inter"),       # Base font
  heading_font = font_google("Roboto"), # Heading font
  navbar_bg = "#f9f9f9"
)
