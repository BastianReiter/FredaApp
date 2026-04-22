

# --- Module: Login ---


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Module Login UI function
#'
#' @noRd
#-------------------------------------------------------------------------------
ModLogin_UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(class = "ui segment",
      style = "position: relative;
               background: #f9fafb;
               border-color: rgba(34, 36, 38, 0.15);
               box-shadow: 0 2px 25px 0 rgba(34, 36, 38, 0.05) inset;",

      div(id = ns("WaiterScreenContainer"),
          style = "position: absolute;
                   height: 100%;
                   width: 100%;
                   top: 0;
                   left: 0;"),

      #-----------------------------------------------------------------------
      # Connect to CCP
      #-----------------------------------------------------------------------

      div(style = "padding: 2em 4em;
                   text-align: center",

          div(class = "ui form",

              div(class = "inline field",
                  div(class = "ui right pointing label primary",
                      "Project name"),
                  shiny.semantic::text_input(ns("ProjectName"))),

              shiny.semantic::file_input(input_id = ns("FileInput"),
                                         width = "30%",
                                         label = "Test",
                                         button_label = "Open file",
                                         accept = c(".csv")),

              DataEditR::dataOutputUI(ns("ServerSpecificationsSave")),

              DataEditR::dataEditUI(ns("ServerSpecificationsTable")),

              br(),

              shiny.semantic::checkbox_input(input_id = "CheckTermsOfUse",
                                             label = "I have read and agree to the CCP terms of use.",
                                             is_marked = FALSE),

              br(),br(),

              shiny.semantic::action_button(ns("ButtonLogin"),
                                            class = "ui blue button",
                                            style = "box-shadow: 0 0 10px 10px white;",
                                            label = "Connect to CCP"))),

      #-----------------------------------------------------------------------
      div(class = "ui horizontal divider", "Or"),
      #-----------------------------------------------------------------------

      #-----------------------------------------------------------------------
      # Connect to virtual CCP
      #-----------------------------------------------------------------------

      br(),br(),

      div(style = "display: grid;
                   grid-template-columns: auto 30em auto;
                   padding: 0 2em 2em 2em;",

          div(),

          div(class = "ui form",
              style = "text-align: center;",

              div(class = "fields",

                  div(class = "field",
                      tags$label("Number of virtual servers"),
                      shiny.semantic::text_input(ns("NumberOfServers"),
                                                 value = "3")),

                  div(class = "field",
                      tags$label("Number of patients per server"),
                      shiny.semantic::text_input(ns("NumberOfPatientsPerServer"),
                                                 value = "1000"))),

              shiny.semantic::action_button(ns("ButtonLoginVirtual"),
                                            label = "Connect to virtual CCP")),

          div()))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module Server
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Module Login Server function
#'
#' @noRd
#-------------------------------------------------------------------------------
ModLogin_Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  # Setting up loading screen
                  ns <- session$ns
                  WaiterScreen <- CreateWaiterScreen(ID = ns("WaiterScreenContainer"))

                  # --- Server logic real CCP connection ---

                  # Create a reactive expression containing a data frame read from an uploaded csv-file if provided
                  ServerSpecifications_InputData <- reactive({ FilePath <- input$FileInput$datapath
                                                               if (is.null(FilePath)) { return(dsCCPhosClient::ServerSpecifications) }
                                                               else { return(read.csv(file = FilePath)) } })

                  # Create a reactive value containing data in the specification's table (initially fed with optional input and then optionally edited)
                  ServerSpecifications_EditData <- DataEditR::dataEditServer(id = "ServerSpecificationsTable",
                                                                             data = ServerSpecifications_InputData,
                                                                             col_names = c("Server name", "Server URL", "Project name", "Token"),
                                                                             col_stretch = TRUE,
                                                                             col_options = list(Token = "password"))

                  # Determine file-saving functionality
                  DataEditR::dataOutputServer(id = "ServerSpecificationsSave",
                                              data = ServerSpecifications_EditData,
                                              write_fun = "write.csv",
                                              write_args = list(row.names = FALSE))   # Don't write row names in csv-file


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


                  # --- Server logic virtual CCP connection ---

                  observe({ WaiterScreen$show()
                            on.exit({ WaiterScreen$hide() })

                            # Trigger function that returns connections object
                            Connections <- dsCCPhosClient::ConnectToVirtualCCP(CCPTestData = session$userData$CCPTestData,
                                                                               NumberOfServers = as.integer(input$NumberOfServers),
                                                                               NumberOfPatientsPerServer = as.integer(input$NumberOfPatientsPerServer))

                            # Assign connections to session$userData object
                            session$userData$DSConnections(Connections)

                            # Assign virtual server specifications to session$userData object
                            session$userData$ServerSpecifications(data.frame(ServerName = names(Connections),
                                                                             URL = "Virtual",
                                                                             ProjectName = "Virtual",
                                                                             Token = "Virtual"))

                            # Initiate 'Checkpoints' data frame ...
                            Checkpoints <- session$userData$ServerSpecifications() %>%
                                                select(ServerName) %>%
                                                mutate(CheckConnection = case_when(ServerName %in% names(session$userData$DSConnections()) ~ "green",
                                                                                   TRUE ~ "red"))

                            # ... and assign it to session$userData object
                            session$userData$Checkpoints(Checkpoints)

                         }) %>%
                      bindEvent(input$ButtonLoginVirtual)
               })
}


