# ---- basemap toggle ----
absolutePanel(bottom = 20, right = 10,
              style = "z-index: 1004;
                             border-radius: 8px;",
              switchInput("basemap_style",
                          "Mode", value = TRUE,
                          onLabel = "Dark",
                          offLabel = "Light", 
                          onStatus = "info",
                          offStatus = "info",
                          inline = TRUE)), # END basemap toggle