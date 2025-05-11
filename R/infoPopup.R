#' Create an info icon with a pop-up description box
#'
#' @param id A unique string used to generate element IDs (must be unique per use)
#' @param description A string of text to show inside the pop-up box
#'
#' @return A UI tagList containing the info icon and description popup

infoPopup <- function(id, description) {
  ns_id <- function(suffix) paste0(id, "_", suffix)
  
  tagList(
    tags$div(
      id = ns_id("wrapper"),
      style = "position: relative; display: inline-block;",
      
      actionLink(
        inputId = ns_id("toggle"),
        label = tags$i(
          class = "fas fa-info-circle",
          style = "color:#08C4E5; font-size: 18px; cursor: pointer;"
        )
      ),
      
      tags$div(
        id = ns_id("popup"),
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
        description
      )
    ),
    
    tags$script(HTML(sprintf("
      (function() {
        let popupVisible_%1$s = false;
        document.getElementById('%1$s_toggle').addEventListener('click', function(event) {
          event.stopPropagation();
          const popup = document.getElementById('%1$s_popup');
          popupVisible_%1$s = !popupVisible_%1$s;
          popup.style.display = popupVisible_%1$s ? 'block' : 'none';
        });

        document.addEventListener('click', function(event) {
          const popup = document.getElementById('%1$s_popup');
          const wrapper = document.getElementById('%1$s_wrapper');
          if (popupVisible_%1$s && !wrapper.contains(event.target)) {
            popup.style.display = 'none';
            popupVisible_%1$s = false;
          }
        });
      })();
    ", id)))
  )
}
