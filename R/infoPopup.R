#' Create an info icon with a pop-up description box
#'
#' @param id A unique string used to generate element IDs (must be unique per use)
#' @param description A string of text to show inside the pop-up box
#' @param learn_more Optional URL to include a "Learn more" link
#' @param data_source Optional name of the data source to display
#'
#' @return A UI tagList containing the info icon and description popup

infoPopup <- function(id, description, learn_more = NULL, data_source = NULL) {
  
  ns_wrapper <- paste0(id, "_wrapper")
  ns_toggle  <- paste0(id, "_toggle")
  ns_popup   <- paste0(id, "_popup")
  
  # Description section
  description_tag <- tags$p(
    tags$span("Description:", style = "font-weight: 500; margin-right: 4px;"),
    tags$span(description, style = "font-weight: 400;")
  )
  
  # Optional data source
  data_source_tag <- if (!is.null(data_source) && nzchar(data_source)) {
    tags$p(
      tags$span("Data Source:", style = "font-weight: 500; margin-right: 4px;"),
      tags$span(data_source, style = "font-weight: 400;")
    )
  } else {
    NULL
  }
  
  # Optional learn more
  learn_more_tag <- if (!is.null(learn_more) && nzchar(learn_more)) {
    tags$p(
      tags$a(
        "Learn more",
        href = learn_more,
        target = "_blank",
        style = "color: #DA8D03; font-weight: bold; text-decoration: underline;"
      )
    )
  } else {
    NULL
  }
  
  tagList(
    tags$div(
      id = ns_wrapper,
      style = "position: relative; display: inline-block;",
      
      actionLink(
        inputId = ns_toggle,
        label = tags$i(
          class = "fas fa-info-circle",
          style = "color:#08C4E5; font-size: 18px; cursor: pointer;"
        )
      ),
      
      tags$div(
        id = ns_popup,
        style = "display: none;
                 position: absolute;
                 top: 25px;
                 left: 0;
                 z-index: 1000;
                 background-color: #f1f9fc;
                 color: #20404F;
                 padding: 10px 15px;
                 border-radius: 5px;
                 border: 1px solid #08C4E5;
                 box-shadow: 0 2px 6px rgba(0,0,0,0.2);
                 width: 250px;",
        description_tag,
        data_source_tag,
        learn_more_tag
      )
    ),
    
    tags$script(HTML(sprintf("
      (function() {
        let popupVisible = false;
        document.getElementById('%s').addEventListener('click', function(event) {
          event.stopPropagation();
          const popup = document.getElementById('%s');
          popupVisible = !popupVisible;
          popup.style.display = popupVisible ? 'block' : 'none';
        });

        document.addEventListener('click', function(event) {
          const popup = document.getElementById('%s');
          const wrapper = document.getElementById('%s');
          if (popupVisible && !wrapper.contains(event.target)) {
            popup.style.display = 'none';
            popupVisible = false;
          }
        });
      })();
    ", ns_toggle, ns_popup, ns_popup, ns_wrapper)))
  )
}

