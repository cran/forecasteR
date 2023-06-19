#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  
  registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
    if (is.null(data))
      NULL
    else
      list(left=as.character(data$left), right=as.character(data$right))
  }, force = TRUE)
  
  Sys.setenv("LANGUAGE" = "ES")
  old <- options()
  on.exit(options(old))
  if(toupper(.Platform$OS.type) != "WINDOWS") {
    options(encoding = "utf8")
  } else {
    options(encoding = "UTF-8")
  }
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server,
      options = list(launch.browser = T)
    ), 
    golem_opts = list(...)
  )
}


