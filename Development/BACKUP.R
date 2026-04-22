
#' Widget.ServerExplorer
#'
#' Launch Module \code{ModServerExplorer} as a Shiny app in a background process so it runs independently from hosting R session.
#'
#' @param ServerSpecifications \code{data.frame} containing credentials for login
#' @param ServerWorkspaceInfo \code{list} containing the previously obtained results of \code{dsCCPhosClient::GetServerWorkspaceInfo()}. Default is \code{NULL}, which means these info data are obtained by the app itself.
#' @param DSConnections \code{list} of \code{DSConnection} objects
#' @param RunAutonomously \code{logical} indicating whether the Shiny app is hosted by a background process (default) available as a URL via web browsers or - if set to \code{FALSE} - is hosted by the current running R session.
#' @param RunInViewer \code{logical} indicating whether the Shiny app should be run in the RStudio Viewer pane (Default: \code{FALSE})
#' @param EndProcessWhenClosingApp \code{logical} indicating whether the background process that runs the Shiny app (if it runs autonomously) should end when the app is closed (default) or should be preserved, in which case it should be ended manually.
#'
#' @return When 'RunAutonomously' is set to \code{TRUE} this function can return the background process to make it assignable to an R symbol. Otherwise it will run/return a \code{shinyApp} object.
#'
#' @export
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Widget.ServerExplorer <- function(#--- Arguments for app itself ---
                                  ServerSpecifications = NULL,
                                  ServerWorkspaceInfo = NULL,
                                  DSConnections = NULL,
                                  #--- Arguments for app wrapper ---
                                  EndProcessWhenClosingApp = TRUE,
                                  RunAutonomously = TRUE,
                                  RunInViewer = FALSE,
                                  ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)

  # --- For Testing Purposes ---#
  # DSConnections <- CCPConnections
  # ServerWorkspaceInfo <- dsCCPhosClient::GetServerWorkspaceInfo(DSConnections = DSConnections)

  # --- Argument Assertions ---
  assert_that(is.logical(EndProcessWhenClosingApp),
              is.logical(RunAutonomously),
              is.logical(RunInViewer))

  if (!is.null(ServerSpecifications)) assert_that(is.data.frame(ServerSpecifications))
  if (!is.null(ServerWorkspaceInfo)) assert_that(is.list(ServerWorkspaceInfo))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

  # If no 'ServerWorkspaceInfo' object is passed, get it programmatically
  if (is.null(ServerWorkspaceInfo)) { ServerWorkspaceInfo <- GetServerWorkspaceInfo(DSConnections = DSConnections) }

#-------------------------------------------------------------------------------

  # Create the app initiating function (UI and server component resulting in a ShinyApp object)
  InitFunction <- function(...)
  {
      require(dsCCPhosClient)
      require(dplyr)
      require(DSI)
      require(DT)
      #require(gt)
      #require(plotly)
      require(purrr)
      require(shiny)
      require(shinyjs)
      require(shiny.semantic)
      require(stringr)
      require(waiter)


      # Since the app is deployed as a package, the folder for external resources (e.g. CSS files, static images) needs to be added manually
      shiny::addResourcePath('www', system.file("www", package = "CCPhosApp"))

      #-------------------------------------------------------------------------
      # Widget UI component
      #-------------------------------------------------------------------------
      UI <- function()
      {
          shiny.semantic::semanticPage(

              # Set margin 0 (default is 10 px)
              margin = "0",

              # Add custom CSS file
              tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/CCPhosStyle.css")),

              # Title shown in browser
              title = "CCPhos Server Explorer",

              # Initiate use of shinyjs functionality
              shinyjs::useShinyjs(),

              # Initiate use of waiter functionality
              waiter::useWaiter(),
              waiter::waiterShowOnLoad(html = spin_3(),
                                       color = transparent(.5)),

              #textOutput(outputId = "TestMonitor"),

              #-----------------------------------------------------------------

              shiny.semantic::grid(id = "MainGrid",

                  # Provide grid template (including definition of area names)
                  grid_template = shiny.semantic::grid_template(

                                        # --- Main grid layout for desktop devices ---
                                        default = list(areas = rbind("header",
                                                                     "main"),

                                                       rows_height = c("minmax(100px, 10vh)", "86vh")),

                                        # --- Main grid layout for mobile devices ---
                                        mobile = list(areas = rbind("header",
                                                                    "main"),

                                                      rows_height = c("100px", "auto"))),

                  area_styles = list(header = "padding: 10px 1em;
                                               background: rgb(5,73,150);
                                               background: linear-gradient(90deg, rgba(5,73,150,1) 8%, rgba(255,255,255,1) 100%);
                                               color: #595959;",
                                     main = "padding: 10px;"),

                  #--- HEADER ------------------------------------------------------------
                  # header = split_layout(style = "display: flex;      /* Set up flexbox to use 'justify-content: space-between' to enable free space between columns without specifying column width */
                  #                                justify-content: space-between;
                  #                                align-items: center;",
                  #
                  #                       img(src = "www/Logo_CCPhosApp.png",
                  #                           alt = "CCPhos App Logo",
                  #                           height = "20px"),
                  #
                  #                       ModConnectionStatus_UI("ConnectionStatus")),

                  header = UIWidgetHeaderBar(),

                  #--- MAIN PANEL --------------------------------------------------------
                  main = segment(class = "ui raised scrolling segment",
                                 style = "height: 100%;
                                          overflow: auto;",

                                 div(h4(class = "ui dividing header",
                                       "Server Explorer"),

                                     div(style = "height: 40em;",

                                         ModServerExplorer_UI("ServerExplorer")),

                                     div(class = "ui divider"), #--------------

                                     div(ModUnivariateExploration_UI("UnivariateExploration"))))))
      }

      #-------------------------------------------------------------------------
      # Widget Server Logic
      #-------------------------------------------------------------------------
      Server <- function(input, output, session)
      {
          WaiterScreen <- CreateWaiterScreen(ID = "WaiterScreenContainer")

          # Hide waiter loading screen after initial app load has finished
          waiter::waiter_hide()

          # Initialize global objects
          session$userData$DSConnections <- reactiveVal(NULL)
          session$userData$ServerSpecifications <- reactiveVal(NULL)
          session$userData$ServerWorkspaceInfo <- reactiveVal(NULL)

          # output$TestMonitor <- renderText({  req(session$userData$DSConnections())
          #                                     paste0(names(session$userData$DSConnections()), collapse = ", ") })

          # 'ModInitialize' assigns content to session$userData objects at app start
          ModInitialize(id = "Initialize",
                        DSConnections = DSConnections,
                        ServerSpecifications = ServerSpecifications,
                        ServerWorkspaceInfo = ServerWorkspaceInfo)


          observe({ WaiterScreen$show()
                    on.exit({ WaiterScreen$hide() })

                    # Assign Server Specifications (CCP credentials and project names) to session$userData according to input table
                    session$userData$ServerSpecifications(ServerSpecifications_EditData())

                    # Trigger dsCCPhosClient::ConnectToCCP() ...
                    Connections <- dsCCPhosClient::ConnectToCCP(ServerSpecifications = session$userData$ServerSpecifications())

                    # ... and assign return to session$userData
                    session$userData$DSConnections(Connections)

                    # Initiate 'Checkpoints' data frame ...
                    Checkpoints <- session$userData$ServerSpecifications() %>%
                                        select(ServerName) %>%
                                        mutate(ConnectionStatus = case_when(ServerName %in% names(session$userData$DSConnections()) ~ "green",
                                                                            TRUE ~ "red"))

                    # ... and assign it to session$userData object
                    session$userData$Checkpoints(Checkpoints)

                 }) %>%
              bindEvent(input$ButtonLogin)


          # --- Call module server logic ---
          Selection <- ModServerExplorer_Server(id = "ServerExplorer")
          SelectedObject <- Selection$Object
          SelectedElement <- Selection$Element

          ModUnivariateExploration_Server(id = "UnivariateExploration",
                                          Selection)


          # If the option 'EndProcessWhenClosingApp' is TRUE, the following ensures that the background process is automatically ending when the app shuts down
          if (EndProcessWhenClosingApp == TRUE) { session$onSessionEnded(function() { stopApp() }) }
      }

      # Return Mini-App
      shiny::shinyApp(ui = UI,
                      server = Server)
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
