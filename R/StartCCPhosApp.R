
#' StartCCPhosApp
#'
#' Launch Shiny app
#'
#' @param DSConnections \code{list} of \code{DSConnection} objects
#' @param ServerSpecifications \code{data.frame}
#' @param CCPTestData \code{list} - Optional CCP test data
#' @param RDSCheckData \code{list} - Optional RDSCheck data
#' @param CurationReport \code{list} - Optional CurationReport data
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StartCCPhosApp <- function(#--- Arguments for app itself ---
                           CCPTestData = NULL,
                           RDSCheckData = NULL,
                           CDSCheckData = NULL,
                           ADSCheckData = NULL,
                           CurationReport = NULL,
                           DSConnections = NULL,
                           ServerSpecifications = NULL,
                           ServerWorkspaceInfo = NULL,
                           #--- Arguments for app wrapper ---
                           EndProcessWhenClosingApp = TRUE,
                           RunAutonomously = FALSE,
                           RunInViewer = FALSE,
                           ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # Create the app initiating function (UI and server component resulting in a ShinyApp object)
  InitFunction <- function(...)
  {
      # Set option to use themes for semantic CCS
      #options(semantic.themes = TRUE)

      #Worker <- shiny.worker::initialize_worker()

      # Since the app is deployed as a package, the folder for external resources (e.g. CSS files, static images) needs to be added manually
      shiny::addResourcePath('www', system.file("www", package = "CCPhosApp"))

      # Start CCPhos app
      shiny::shinyApp(ui = MainUIComponent(),
                      server = MainServerComponent(CCPTestData = CCPTestData,
                                                   RDSCheckData = RDSCheckData,
                                                   CDSCheckData = CDSCheckData,
                                                   ADSCheckData = ADSCheckData,
                                                   CurationReport = CurationReport,
                                                   DSConnections = DSConnections,
                                                   ServerSpecifications = ServerSpecifications,
                                                   ServerWorkspaceInfo = ServerWorkspaceInfo))
  }

  # Either use CCPhosApp::RunAutonomousApp() to run the app in a separate background process or run it in the hosting session
  if (RunAutonomously == TRUE)
  {
      RunAutonomousApp(ShinyAppInitFunction = InitFunction,
                       AppArguments = list(...),
                       RunInViewer = RunInViewer)
  } else {

      # This returns the app itself
      InitFunction(...)
  }
}
