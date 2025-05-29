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
  ns_close   <- paste0(id, "_close")
  
  # Description section
  description_tag <- tags$p(
    tags$span("Description:", style = "font-weight: 500; margin-right: 4px;"),
    tags$span(description, style = "font-weight: 400;")
  )
  
  data_source_tag <- if (!is.null(data_source) && nzchar(data_source)) {
    tags$p(
      tags$span("Data Source:", style = "font-weight: 500; margin-right: 4px;"),
      tags$span(data_source, style = "font-weight: 400;")
    )
  } else NULL
  
  learn_more_tag <- if (!is.null(learn_more) && nzchar(learn_more)) {
    tags$p(
      tags$a(
        "Learn more",
        href = learn_more,
        target = "_blank",
        style = "color: #DA8D03; font-weight: bold; text-decoration: underline;"
      )
    )
  } else NULL
  
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
           position: fixed;
           top: 50%%;
           left: 50%%;
           transform: translate(-50%%, -50%%);
           z-index: 2000;
           background-color: #f1f9fc;
           color: #20404F;
           padding: 30px;
           border-radius: 8px;
           border: 1px solid #08C4E5;
           box-shadow: 0 2px 10px rgba(0,0,0,0.25);
           width: 90vw;
           max-width: 500px;
           max-height: 70vh;
           overflow-y: auto;",
        
        # Floating Close Icon
        tags$div(
          id = ns_close,
          style = "position: absolute; top: 8px; right: 12px; padding: 8px; margin: 5px; cursor: pointer;",
          tags$i(class = "fas fa-times", style = "color: #DA8D03; font-size: 16px;")
        ),
        
        # Content wrapper to push text down
        tags$div(
          style = "margin-top: 16px;",
          description_tag,
          data_source_tag,
          learn_more_tag
        )
      )
    ),

    # JavaScript
    tags$script(HTML(sprintf("
      (function() {
        const toggle = document.getElementById('%s');
        const popup = document.getElementById('%s');
        const wrapper = document.getElementById('%s');
        const closeBtn = document.getElementById('%s');

        toggle.addEventListener('click', function(event) {
          event.preventDefault();
          event.stopPropagation();

          if (window.activeInfoPopup && window.activeInfoPopup !== popup) {
            window.activeInfoPopup.style.display = 'none';
          }

          const isVisible = popup.style.display === 'block';
          popup.style.display = isVisible ? 'none' : 'block';

          if (!isVisible) {
            const rect = popup.getBoundingClientRect();
            const buffer = 10;

            if (rect.right > window.innerWidth - buffer) {
              popup.style.left = 'auto';
              popup.style.right = '0px';
            } else {
              popup.style.left = '0px';
              popup.style.right = 'auto';
            }

            if (rect.bottom > window.innerHeight - buffer) {
              popup.style.top = 'auto';
              popup.style.bottom = '25px';
            } else {
              popup.style.top = '25px';
              popup.style.bottom = 'auto';
            }

            window.activeInfoPopup = popup;
          } else {
            window.activeInfoPopup = null;
          }
        });

        closeBtn.addEventListener('click', function(event) {
          event.stopPropagation();
          popup.style.display = 'none';
          window.activeInfoPopup = null;
        });

        document.addEventListener('click', function(event) {
          if (window.activeInfoPopup && !wrapper.contains(event.target)) {
            window.activeInfoPopup.style.display = 'none';
            window.activeInfoPopup = null;
          }
        });
      })();
    ", ns_toggle, ns_popup, ns_wrapper, ns_close)))
  )
}

