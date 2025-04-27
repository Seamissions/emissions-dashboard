# theme.R
library(bslib)

my_theme <- bs_theme(
  version = 5,  # Bootstrap version
  bg = "#e8fffd",       # Background color
  fg = "#343a40",       # Foreground (text) color
  primary = "#0d6efd",  # Primary color
  base_font = font_google("Roboto"),       # Base font
  heading_font = font_google("Roboto"), # Heading font
  navbar_bg = "#0d6efd"
)