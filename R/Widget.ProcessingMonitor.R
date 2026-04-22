
#' Widget.ProcessingMonitor
#'
#' Launch a Shiny app that facilitates interacting with output of processing monitoring, like data set checks and data transformation tracks.
#'
#' @param ServerSpecifications \code{data.frame} containing credentials for login
#' @param DSConnections \code{list} of \code{DSConnection} objects
#' @param RunAutonomously \code{logical} indicating whether the Shiny app is hosted by a background process (default) available as a URL via web browsers or - if set to \code{FALSE} - is hosted by the current running R session.
#' @param RunInViewer \code{logical} indicating whether the Shiny app should be run in the RStudio Viewer pane (Default: \code{FALSE})
#' @param EndProcessWhenClosingApp \code{logical} indicating whether the background process that runs the Shiny app (if it runs autonomously) should end when the app is closed (default) or should be preserved, in which case it should be ended manually.
#'
#' @return When 'RunAutonomously' is set to \code{TRUE} this function can return the background process to make it assignable to an R symbol. Otherwise it will run/return a \code{shinyApp} object.
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Widget.ProcessingMonitor <- function(#--- Arguments for app itself ---
                                     ServerSpecifications = NULL,
                                     RDSCheckData = NULL,
                                     CDSCheckData = NULL,
                                     ADSCheckData = NULL,
                                     CurationReport = NULL,
                                     DSConnections = NULL,
                                     #--- Arguments for app wrapper ---
                                     EnableLiveConnection = FALSE,
                                     EndProcessWhenClosingApp = TRUE,
                                     RunAutonomously = FALSE,
                                     RunInViewer = FALSE,
                                     UseVirtualConnections = FALSE,
                                     ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DSConnections <- CCPConnections
  # ServerWorkspaceInfo <- dsCCPhosClient::GetServerWorkspaceInfo(DSConnections = DSConnections)

  # --- Argument Validation ---
  assert_that(is.logical(EndProcessWhenClosingApp),
              is.logical(RunAutonomously),
              is.logical(RunInViewer))

  if (!is.null(ServerSpecifications)) assert_that(is.data.frame(ServerSpecifications))
  if (!is.null(RDSCheckData)) assert_that(is.list(RDSCheckData))
  if (!is.null(CDSCheckData)) assert_that(is.list(CDSCheckData))
  if (!is.null(ADSCheckData)) assert_that(is.list(ADSCheckData))
  if (!is.null(CurationReport)) assert_that(is.list(CurationReport))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # --- Preparational settings ---
  if (UseVirtualConnections == TRUE) { EnableLiveConnection == FALSE }
  if (EnableLiveConnection == TRUE) { DSConnections <- NULL }

  # If no monitor data is passed, get it programmatically
  if (is.null(RDSCheckData) && EnableLiveConnection == FALSE) { RDSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
                                                                                                                  Module = "CCP",
                                                                                                                  Stage = "Raw",
                                                                                                                  DSConnections = DSConnections) }
  if (is.null(CDSCheckData) && EnableLiveConnection == FALSE) { CDSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.CuratedDataSet",
                                                                                                                  Module = "CCP",
                                                                                                                  Stage = "Curated",
                                                                                                                  DSConnections = DSConnections) }
  if (is.null(ADSCheckData) && EnableLiveConnection == FALSE) { ADSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.AugmentedDataSet",
                                                                                                                  Module = "CCP",
                                                                                                                  Stage = "Augmented",
                                                                                                                  DSConnections = DSConnections) }
  if (is.null(CurationReport) && EnableLiveConnection == FALSE) { CurationReport <- dsFredaClient::ds.GetCurationReport(Module = "CCP",
                                                                                                                        DSConnections = DSConnections) }
#-------------------------------------------------------------------------------

  # Create the app initiating function (UI and server component resulting in a ShinyApp object)
  InitFunction <- function(...)
  {
      # Since the app is deployed as a package, the folder for external resources (e.g. CSS files, static images) needs to be added manually
      shiny::addResourcePath('www', system.file("www", package = "CCPhosApp"))

      #-------------------------------------------------------------------------
      # Widget UI component
      #-------------------------------------------------------------------------
      UI <- function()
      {
          Layout <- function(ns)
          {
              div(h4(class = "ui dividing header",
                  "Processing Monitor"),

                  div(class = "ui accordion",

                      div(class = "active title AccordionHeader",
                          shiny.semantic::icon(class = "dropdown"),
                          "Data Set Checks"),

                      div(class = "active content",

                          div(style = "height: 30em;
                                       overflow: auto;
                                       margin: 0;",

                              shiny.semantic::tabset(tabs = list(list(menu = "Raw Data Set (RDS)",
                                                                      content = ModDataSetMonitor_UI("RDSMonitor")),
                                                                 list(menu = "Curated Data Set (CDS)",
                                                                      content = ModDataSetMonitor_UI("CDSMonitor")),
                                                                 list(menu = "Augmented Data Set (ADS)",
                                                                      content = ModDataSetMonitor_UI("ADSMonitor"))))))),


                  #-----------------------------------------------------------------------
                  div(class = "ui divider",
                      style = "margin: 1.5em 0;"),
                  #-----------------------------------------------------------------------


                  div(class = "ui accordion",      # Note: For this to work an extra JS script is necessary (see MainUIComponent())

                      div(class = "active title AccordionHeader",
                          shiny.semantic::icon(class = "dropdown"),
                          "Curation Report"),

                      div(class = "active content",

                          div(style = "height: 20em;
                                       overflow: auto;
                                       margin: 0;",

                              ModCurationReport_UI("CurationReport")))),


                  #-----------------------------------------------------------------------
                  div(class = "ui divider",
                      style = "margin: 1.5em 0;"),
                  #-----------------------------------------------------------------------


                  div(class = "ui accordion",

                      div(class = "active title AccordionHeader",
                          shiny.semantic::icon(class = "dropdown"),
                          "Data Transformation Monitor"),

                      div(class = "active content",

                          div(style = "height: 30em;
                                       overflow: auto;",

                              ModDataTransformationMonitor_UI("DataTransformationMonitor")))))
           }

           # Call Widget frame module UI and pass widget-specific UI layout
           ModWidget_UI(id = "ProcessingMonitorWidget",
                        Title = "CCPhos Processing Monitor",
                        WidgetMainUI = Layout)
      }

      #-------------------------------------------------------------------------
      # Widget Server Logic
      #-------------------------------------------------------------------------
      Server <- function(input, output, session)
      {
          # Hide waiter loading screen after initial app load has finished
          waiter::waiter_hide()

          # Define widget-specific server logic that is passed to widget frame module
          WidgetServerLogic <- function(session)
                               {


                                  # # --- Call modules: DataSet Monitors ---
                                  # ModDataSetMonitor_Server(id = "RDSMonitor", DataSetCheckData = session$userData$RDSCheckData)
                                  # ModDataSetMonitor_Server(id = "CDSMonitor", DataSetCheckData = session$userData$CDSCheckData)
                                  # ModDataSetMonitor_Server(id = "ADSMonitor", DataSetCheckData = session$userData$ADSCheckData)
                                  #
                                  # # --- Call module: Data Curation Monitor ---
                                  # ModCurationReport_Server(id = "CurationReport")
                                  #
                                  # # --- Call module: Data Transformation Monitor ---
                                  # ModDataTransformationMonitor_Server(id = "DataTransformationMonitor")
                                }

          # Call Widget frame module and pass widget-specific server logic
          ModWidget_Server(id = "ServerExplorerWidget",
                           WidgetServerLogic,
                           EnableLiveConnection)

          #---------------------------------------------------------------------

          # Initialize global objects
          session$userData$RDSCheckData <- reactiveVal(NULL)
          session$userData$CDSCheckData <- reactiveVal(NULL)
          session$userData$ADSCheckData <- reactiveVal(NULL)
          session$userData$CurationReport <- reactiveVal(NULL)
          session$userData$DSConnections <- reactiveVal(NULL)
          session$userData$ServerSpecifications <- reactiveVal(NULL)

          # output$TestMonitor <- renderText({  req(session$userData$ServerWorkspaceInfo())
          #                                     paste0(names(session$userData$ServerWorkspaceInfo()), collapse = ", ") })


          # 'ModInitialize' assigns content to session$userData objects at app start
          ModInitialize(id = "Initialize",
                        RDSCheckData = RDSCheckData,
                        CDSCheckData = CDSCheckData,
                        ADSCheckData = ADSCheckData,
                        CurationReport = CurationReport,
                        DSConnections = DSConnections,
                        ServerSpecifications = ServerSpecifications)

          # If the option 'EndProcessWhenClosingApp' is TRUE, the following ensures that the background process is automatically ending when the app shuts down
          if (EndProcessWhenClosingApp == TRUE) { session$onSessionEnded(function() { stopApp() }) }
      }

      # Return Mini-App
      shiny::shinyApp(ui = UI,
                      server = Server)
  }

#-------------------------------------------------------------------------------

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
