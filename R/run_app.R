#' Launch LearnTidyML Shiny App
#'
#' @param launch.browser Logical; if TRUE (default), opens the app in the
#'   system's default web browser. If FALSE, opens in RStudio viewer pane.
#' @param ... Additional arguments passed to \code{shiny::shinyApp()}, such
#'   as \code{uiPattern} or \code{enableBookmarking}.
#' @return A Shiny app object (class \code{shiny.appobj}) created by
#'   \code{shiny::shinyApp()}. The app starts when this object is printed, which
#'   happens automatically when the function is called at the console. As a side
#'   effect, the function sets the \code{shiny.maxRequestSize},
#'   \code{shiny.sanitize.errors} and \code{shiny.session.timeout} options.
#' @examples
#' if (interactive()) {
#'   launch_LearnTidyML()
#' }
#' @export
launch_LearnTidyML <- function(launch.browser = TRUE, ...) {

  # Set security options for Shiny
  options(
    # File import size limit: 50 MB
    shiny.maxRequestSize = 50 * 1024^2,

    # Sanitize errors to prevent information disclosure
    shiny.sanitize.errors = TRUE,

    # Session timeout (1 hour in seconds)
    shiny.session.timeout = 3600
  )

  # Display startup message
  cat("\n")
  cat("====================================================================\n")
  cat("                 LearnTidyML - Machine Learning Tool               \n")
  cat("====================================================================\n")
  cat("\n")
  cat("To stop the app:\n")
  cat("  - Press Ctrl+C (or Cmd+. on Mac) in the R console\n")
  cat("  - Or close the R session\n")
  cat("\n")
  cat("====================================================================\n")
  cat("\n")

  # Log application start
  message(paste("LearnTidyML starting at", Sys.time()))

  shiny::shinyApp(
    ui = app_ui(),
    server = app_server,
    onStart = function() {
      # Set flag to track active sessions
      shiny::onStop(function() {
        message(paste("LearnTidyML stopped at", Sys.time()))
      })
    },
    options = list(
      launch.browser = launch.browser
    ),
    ...
  )
}