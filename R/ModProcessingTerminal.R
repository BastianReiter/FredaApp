

# --- MODULE: Processing Terminal ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModProcessingTerminal_UI <- function(id,
                                     ButtonLabel)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(style = "display: grid;
               height: 100%;
               grid-template-rows: 4em 22.8em;",

      div(style = "padding: 10px;
                   text-align: center;",

          shiny.semantic::action_button(ns("ProcessingTrigger"),
                                        class = "ui blue button",
                                        style = "box-shadow: 0 0 10px 10px white;",
                                        label = ButtonLabel)),

          div(style = "position: relative;",

              div(id = ns("WaiterScreenContainer"),
                  style = "position: absolute;
                           height: 100%;
                           width: 100%;
                           top: 0.5em;
                           left: 0;"),

              ModMessageMonitor_UI(ns("Monitor"))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModProcessingTerminal_Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ReturnMessages <- reactiveVal(NULL)
                  Complete <- reactiveVal(FALSE)

                  ModMessageMonitor_Server("Monitor",
                                           MessagesList = ReturnMessages)

                  observe({ shinyjs::showElement(id = "Monitor", anim = TRUE, animType = "fade") }) %>%
                      bindEvent(ReturnMessages())


                  # Setting up loading behavior with waiter package functionality
                  #-------------------------------------------------------------
                  ns <- session$ns
                  WaiterScreen <- CreateWaiterScreen(ID = ns("WaiterScreenContainer"))

                  LoadingOn <- function()
                  {
                      shinyjs::disable("ProcessingTrigger")
                      WaiterScreen$show()
                  }

                  LoadingOff <- function()
                  {
                      shinyjs::enable("ProcessingTrigger")
                      WaiterScreen$hide()
                  }


                  if (id == "CheckServerRequirements")
                  {
                      observe({ # Assign loading behavior
                                LoadingOn()
                                on.exit(LoadingOff())

                                # Trigger function CheckServerRequirements() and save returned list
                                ServerCheck <- SafeDS(dsCCPhosClient::CheckServerRequirements(ServerSpecifications = session$userData$ServerSpecifications(),
                                                                                              DSConnections = session$userData$DSConnections()))

                                # In case of error (determined by class of 'SafeDS()' output), break reactive chain and show error notification
                                if (inherits(ServerCheck, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign 'Messages' to reactive value ReturnMessages
                                ReturnMessages(ServerCheck$Messages)

                                # Update 'Checkpoints' data frame ...
                                Checkpoints <- session$userData$Checkpoints() %>%
                                                    left_join(select(ServerCheck$PackageAvailability, c(ServerName, CheckPackageAvailability)), by = join_by(ServerName)) %>%
                                                    left_join(ServerCheck$VersionOfdsCCPhos, by = join_by(ServerName)) %>%
                                                    left_join(select(ServerCheck$FunctionAvailability, c(ServerName, CheckFunctionAvailability)), by = join_by(ServerName)) %>%
                                                    left_join(select(ServerCheck$OpalTableAvailability, c(ServerName, CheckOpalTableAvailability)), by = join_by(ServerName))

                                # ... and reassign it to session$userData object
                                session$userData$Checkpoints(Checkpoints)

                                # Trigger function GetServerOpalDBInfo() and assign return (data.frame) to reactive value ServerOpalDBInfo in session$userData
                                session$userData$ServerOpalDBInfo(ServerCheck$OpalTableAvailability)

                                # Set reactive value 'Complete' TRUE
                                Complete(TRUE)

                                }) %>%
                          bindEvent(input$ProcessingTrigger)
                  }


                  if (id == "LoadData")
                  {
                      observe({ # Assign loading behavior
                                LoadingOn()
                                on.exit(LoadingOff())

                                # Trigger function LoadRawDataSet()
                                Messages <- SafeDS(dsCCPhosClient::LoadRawDataSet(ServerSpecifications = session$userData$ServerSpecifications(),
                                                                                  RunAssignmentChecks = TRUE,
                                                                                  DSConnections = session$userData$DSConnections()))

                                # In case of error (determined by class of 'SafeDS()' output), break reactive chain and show error notification
                                if (inherits(Messages, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign messages to reactive value
                                ReturnMessages(Messages)

                                # Trigger function ds.CheckDataSet() for RDS and save returned list
                                RDSCheckData <- SafeDS(dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
                                                                                         Module = "CCP",
                                                                                         Stage = "Raw",
                                                                                         DSConnections = session$userData$DSConnections()))

                                if (inherits(RDSCheckData, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign to session$userData object
                                session$userData$RDSCheckData(RDSCheckData)

                                # Trigger function GetServerWorkspaceInfo()...
                                InfoData <- SafeDS(dsFredaClient::GetServerWorkspaceInfo(DSConnections = session$userData$DSConnections()))
                                # ... handle possible error ...
                                if (inherits(InfoData, "dsFail")) { ShowDSError(); return(NULL) }
                                # ... and assign return (data.frame) to reactive value ServerWorkspaceInfo in session$userData
                                session$userData$ServerWorkspaceInfo(InfoData)

                                # Set reactive value Complete TRUE
                                Complete(TRUE)

                                }) %>%
                          bindEvent(input$ProcessingTrigger)
                  }


                  if (id == "CurateData")
                  {
                      observe({ # Assign loading behavior
                                LoadingOn()
                                on.exit(LoadingOff())

                                # Trigger function ds.CurateData() and save return
                                Curation <- SafeDS(dsCCPhosClient::ds.CurateData(RawDataSetName = "CCP.RawDataSet",
                                                                                 Settings = NULL,
                                                                                 OutputName = "CCP.CurationOutput",
                                                                                 UnpackCuratedDataSet = TRUE,
                                                                                 RunAssignmentChecks = FALSE,
                                                                                 DSConnections = session$userData$DSConnections()))

                                if (inherits(Curation, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign returned messages (concatenated lists) to reactive value ReturnMessages
                                ReturnMessages(Curation$Messages)

                                # Trigger function ds.CheckDataSet() for CDS and save returned list
                                CDSCheckData <- SafeDS(dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.CuratedDataSet",
                                                                                         Module = "CCP",
                                                                                         Stage = "Curated",
                                                                                         DSConnections = session$userData$DSConnections()))

                                if (inherits(CDSCheckData, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign to session$userData object
                                session$userData$CDSCheckData(CDSCheckData)

                                # Update 'Checkpoints' data frame ...
                                Checkpoints <- session$userData$Checkpoints() %>%
                                                    left_join(Curation$CurationCompletionCheck, by = join_by(ServerName))

                                # # ... and reassign it to session$userData object
                                session$userData$Checkpoints(Checkpoints)

                                # Trigger function GetServerWorkspaceInfo() and assign return to reactive value ServerWorkspaceInfo in session$userData
                                InfoData <- SafeDS(dsFredaClient::GetServerWorkspaceInfo(DSConnections = session$userData$DSConnections()))
                                # ... handle possible error ...
                                if (inherits(InfoData, "dsFail")) { ShowDSError(); return(NULL) }
                                # ... and assign return (data.frame) to reactive value ServerWorkspaceInfo in session$userData
                                session$userData$ServerWorkspaceInfo(InfoData)

                                # Trigger function ds.GetCurationReport() and assign return to reactive value 'CurationReport' in session$userData
                                CurationReport <- SafeDS(dsFredaClient::ds.GetCurationReport(Module = "CCP",
                                                                                             DSConnections = session$userData$DSConnections()))
                                # ... handle possible errors ...
                                if (inherits(CurationReport, "dsFail")) { ShowDSError(); return(NULL) }
                                # ... and assign return to reactive value in session$userData
                                session$userData$CurationReport(CurationReport)

                                # Set reactive value Complete TRUE
                                Complete(TRUE)

                                }) %>%
                          bindEvent(input$ProcessingTrigger)
                  }


                  if (id == "AugmentData")
                  {
                      observe({ # Assign loading behavior
                                LoadingOn()
                                on.exit(LoadingOff())

                                # Trigger function ds.AugmentData() and save return
                                Augmentation <- SafeDS(dsCCPhosClient::ds.AugmentData(CuratedDataSetName = "CCP.CuratedDataSet",
                                                                                      OutputName = "CCP.AugmentationOutput",
                                                                                      UnpackAugmentedDataSet = TRUE,
                                                                                      RunAssignmentChecks = FALSE,
                                                                                      DSConnections = session$userData$DSConnections()))

                                if (inherits(Augmentation, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign returned messages (concatenated lists) to reactive value ReturnMessages
                                ReturnMessages(Augmentation$Messages)

                                # Trigger function ds.CheckDataSet() for ADS and save returned list
                                ADSCheckData <- SafeDS(dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.AugmentedDataSet",
                                                                                         Module = "CCP",
                                                                                         Stage = "Augmented",
                                                                                         DSConnections = session$userData$DSConnections()))

                                if (inherits(ADSCheckData, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign to session$userData object
                                session$userData$ADSCheckData(ADSCheckData)

                                # Update 'Checkpoints' data frame ...
                                Checkpoints <- session$userData$Checkpoints() %>%
                                                    left_join(Augmentation$AugmentationCompletionCheck, by = join_by(ServerName))

                                # # ... and reassign it to session$userData object
                                session$userData$Checkpoints(Checkpoints)

                                # Trigger function GetServerWorkspaceInfo() and assign return to reactive value ServerWorkspaceInfo in session$userData
                                InfoData <- SafeDS(dsFredaClient::GetServerWorkspaceInfo(DSConnections = session$userData$DSConnections()))
                                # ... handle possible error ...
                                if (inherits(InfoData, "dsFail")) { ShowDSError(); return(NULL) }
                                # ... and assign return (data.frame) to reactive value ServerWorkspaceInfo in session$userData
                                session$userData$ServerWorkspaceInfo(InfoData)

                                # Set reactive value Complete TRUE
                                Complete(TRUE)

                                }) %>%
                          bindEvent(input$ProcessingTrigger)
                  }

                  return(Complete)
               })
}


