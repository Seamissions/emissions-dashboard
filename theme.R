# theme.R
library(bslib)

seamissions_theme <- bs_theme(
  version = 5,  # Bootstrap version
  bg = "#053762",      # Background color
  fg = "#053762",       # Foreground (text) color
  primary = "#08C4E5",  # Primary color
  secondary = "#DA8D03", # Orange for buttons
  base_font = font_google("Inter"),       # Base font
  heading_font = font_google("Roboto"), # Heading font
  navbar_bg = "#f9f9f9"
)


# Add customization for dropdown menu
seamissions_theme <- bs_add_rules(seamissions_theme, "
  /* Style the pickerInput button (the bar) */
  .bootstrap-select .btn {
    background-color: #053762;  /* Dark blue bar */
    color: orange;              /* Selected text (orange) */
    border: 1px solid #08C4E5;
  }

  /* Placeholder text when no selection */
  .bootstrap-select .btn .filter-option {
    color: #f9f9f9;  /* White placeholder text */
  }

  /* Dropdown menu background */
  .bootstrap-select .dropdown-menu {
    background-color: white;  /* White dropdown */
  }

  /* Dropdown item text */
  .bootstrap-select .dropdown-menu li a {
    color: #053762;  /* Blue text */
  }

  /* Hover/focus on dropdown items */
  .bootstrap-select .dropdown-menu li a:hover,
  .bootstrap-select .dropdown-menu li a:focus {
    background-color: #053762;  /* Dark blue hover background */
    color: orange;              /* Orange hover text */
  }

  /* Style 'Clear Selection' (opt items) */
  .bootstrap-select .dropdown-menu li a.opt {
    background-color: white;     /* White background for 'Clear Selection' */
    color: #053762;              /* Blue text */
  }

  .bootstrap-select .dropdown-menu li a.opt:hover,
  .bootstrap-select .dropdown-menu li a.opt:focus {
    background-color: #053762;   /* Dark blue hover for 'Clear Selection' */
    color: orange;               /* Orange hover text */
  }

  /* Live search bar styling */
  .bootstrap-select .bs-searchbox input {
    background-color: white;     /* White background */
    color: #053762;              /* Dark blue text */
    border: 1px solid #08C4E5;   /* Optional: match theme border */
  }
")


