

# --- MODULE: ServerExplorer ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.ServerExplorer.UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(id = ns("ServerExplorerContainer"),
      class = "ui segment",
      style = "height: 100%;
               margin: 0;",

      div(shiny.semantic::button(ns("ByObjectButton"),
                                 label = "by Object",
                                 class = "ui left attached toggle button"),
          shiny.semantic::button(ns("ByServerButton"),
                                 label = "by Server",
                                 class = "ui right attached toggle button"),
          shiny.semantic::button(ns("UpdateButton"),
                                 label = "Update",
                                 class = "ui right attached button")),
          # textOutput(outputId = ns("TestMonitor"))),

      # div(class = "ui top attached label",
      #     "Object"),

      div(style = "display: grid;
                   grid-template-columns: 2fr 2fr 2fr 2fr;
                   grid-gap: 1em;
                   margin: 0;
                   height: 100%;",

          div(style = "height: calc(100% - 30px);
                       overflow: auto;",

              DT::DTOutput(ns("PrimarySelection"),
                           width = "95%",      # Width calculation necessary to avoid false overflow rendering (vertical scroll bar is approx. 14 px wide)
                           height = "95%")),

          div(style = "height: calc(100% - 30px);
                       overflow: auto;",

              DT::DTOutput(ns("SecondarySelection"),
                           width = "95%",
                           height = "95%")),

          div(style = "height: calc(100% - 30px);
                       overflow: auto;",

              DT::DTOutput(ns("ObjectDetails"),
                           width = "95%",
                           height = "95%")),

          div(style = "height: calc(100% - 30px);
                       overflow: auto;",

              DT::DTOutput(ns("Values"),
                           width = "95%",
                           height = "95%"))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.ServerExplorer.Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ns <- session$ns
                  WaiterScreen <- CreateWaiterScreen(ID = ns("WaiterScreenContainer"))

                  LoadingOn <- function()
                  {
                      shinyjs::disable("ByObjectButton")
                      shinyjs::disable("ByServerButton")
                      shinyjs::disable("UpdateButton")
                      WaiterScreen$show()
                  }
                  LoadingOff <- function()
                  {
                      shinyjs::enable("ByObjectButton")
                      shinyjs::enable("ByServerButton")
                      shinyjs::enable("UpdateButton")
                      WaiterScreen$hide()
                  }
                  #---------------------------------------------------------

                  SelectedObjectName <- reactiveVal(NULL)
                  SelectedServerName <- reactiveVal("All")
                  PrimarySelectionCriterion <- reactiveVal("ByObject")


                  # output$TestMonitor <- renderText({
                  #                                     #req(session$userData$ServerWorkspaceInfo())
                  #                                     #paste(names(session$userData$ServerWorkspaceInfo()), collapse = ", ")
                  #                                     SelectedServerName()
                  #                                  })


                  observe({ req(session$userData$ServerWorkspaceInfo())
                            req(session$userData$DSConnections())

                            # Assign loading behavior
                            LoadingOn()
                            on.exit(LoadingOff())

                            # Trigger function GetServerWorkspaceInfo() and assign return to reactive value ServerWorkspaceInfo in session$userData
                            InfoData <- SafeDS(dsFredaClient::GetServerWorkspaceInfo(DSConnections = session$userData$DSConnections()))
                            # ... handle possible errors ...
                            if (inherits(InfoData, "dsFail")) { ShowDSError(); return(NULL) }
                            # ... and assign return (data.frame) to reactive value ServerWorkspaceInfo in session$userData
                            session$userData$ServerWorkspaceInfo(InfoData)

                          }) %>%
                      bindEvent(input$UpdateButton)

                  observe({ SelectedObjectName(session$userData$ServerWorkspaceInfo()$Overview$Object[1]) }) %>%
                      bindEvent(session$userData$ServerWorkspaceInfo())

                  observe({ SelectedServerName(names(session$userData$ServerWorkspaceInfo()$Overview)[1]) }) %>%
                      bindEvent(session$userData$ServerWorkspaceInfo())

                  observe({ PrimarySelectionCriterion("ByObject") }) %>%
                      bindEvent(input$ByObjectButton)

                  observe({ PrimarySelectionCriterion("ByServer") }) %>%
                      bindEvent(input$ByServerButton)


                  # Reset object and server selections when primary selection criterion changes
                  observe({ req(session$userData$ServerWorkspaceInfo())
                            SelectedObjectName(session$userData$ServerWorkspaceInfo()$Overview$All$Object[1])
                            SelectedServerName("All")
                          }) %>%
                        bindEvent(PrimarySelectionCriterion())


                  PrimarySelectionData <- reactive({  req(session$userData$ServerWorkspaceInfo())
                                                      req(PrimarySelectionCriterion())

                                                      if (PrimarySelectionCriterion() == "ByObject")
                                                      {
                                                          PrimarySelectionData <- session$userData$ServerWorkspaceInfo()$Overview$All %>%
                                                                                      select(Object, Class)
                                                      }
                                                      if (PrimarySelectionCriterion() == "ByServer")
                                                      {
                                                          PrimarySelectionData <- data.frame(ServerName = names(session$userData$ServerWorkspaceInfo()$Overview))
                                                      }
                                                      return(PrimarySelectionData)
                                                   })


                  SecondarySelectionData <- reactive({  req(session$userData$ServerWorkspaceInfo())
                                                        req(PrimarySelectionCriterion())
                                                        req(SelectedObjectName())
                                                        req(SelectedServerName())

                                                        SecondarySelectionData <- NULL

                                                        if (PrimarySelectionCriterion() == "ByObject" & !is.null(SelectedObjectName()))
                                                        {
                                                            SecondarySelectionData <- session$userData$ServerWorkspaceInfo()$Overview %>%
                                                                                          list_rbind(names_to = "ServerName") %>%
                                                                                          filter(Object == SelectedObjectName()) %>%
                                                                                          select(ServerName,
                                                                                                 Exists,
                                                                                                 Length,
                                                                                                 RowCount) %>%
                                                                                          ConvertLogicalToIcon()
                                                        }
                                                        if (PrimarySelectionCriterion() == "ByServer" & !is.null(SelectedServerName()))
                                                        {
                                                            SecondarySelectionData <- session$userData$ServerWorkspaceInfo()$Overview[[SelectedServerName()]] %>%
                                                                                          select(Object,
                                                                                                 Exists,
                                                                                                 Class,
                                                                                                 Length,
                                                                                                 RowCount) %>%
                                                                                          ConvertLogicalToIcon()
                                                        }
                                                        return(SecondarySelectionData)
                                                     })

                  output$PrimarySelection <- DT::renderDT({ req(session$userData$ServerWorkspaceInfo())
                                                            req(PrimarySelectionData())

                                                            DT::datatable(data = PrimarySelectionData(),
                                                                          class = "ui small compact scrollable selectable table",
                                                                          editable = FALSE,
                                                                          escape = FALSE,
                                                                          #extensions = "FixedHeader",
                                                                          filter = "none",
                                                                          options = list(fixedHeader = TRUE,
                                                                                         info = FALSE,
                                                                                         layout = list(top = NULL),
                                                                                         ordering = FALSE,
                                                                                         paging = FALSE,
                                                                                         #scrollY = 400,
                                                                                         searching = FALSE),
                                                                          rownames = FALSE,
                                                                          selection = list(mode = "single",
                                                                                           target = "row"),
                                                                          style = "semanticui")
                                                          })


                  output$SecondarySelection <- DT::renderDT({ req(session$userData$ServerWorkspaceInfo())
                                                              req(SecondarySelectionData())

                                                              DT::datatable(data = SecondarySelectionData(),
                                                                            class = "ui small compact scrollable selectable table",
                                                                            editable = FALSE,
                                                                            escape = FALSE,
                                                                            #extensions = "FixedHeader",
                                                                            filter = "none",
                                                                            options = list(fixedHeader = TRUE,
                                                                                           info = FALSE,
                                                                                           layout = list(top = NULL),
                                                                                           ordering = FALSE,
                                                                                           paging = FALSE,
                                                                                           #scrollY = 400,
                                                                                           searching = FALSE),
                                                                            rownames = FALSE,
                                                                            selection = list(mode = "single",
                                                                                             target = "row",
                                                                                             selected = 1),
                                                                            style = "semanticui")
                                                            })


                  observe({ req(session$userData$ServerWorkspaceInfo())
                            req(PrimarySelectionCriterion())
                            req(PrimarySelectionData())

                            # Get the index of the selected row using DT functionality
                            RowIndex <- input$PrimarySelection_rows_selected

                            if (PrimarySelectionCriterion() == "ByObject")
                            {
                                # Set selected object name
                                SelectedObjectName(PrimarySelectionData()$Object[RowIndex])
                            }
                            if (PrimarySelectionCriterion() == "ByServer")
                            {
                                # Set selected server name
                                SelectedServerName(PrimarySelectionData()$ServerName[RowIndex])
                            }

                            # Returning name of object selected in table
                            #session$userData$ServerWorkspaceInfo()$Overview$Object[RowIndex]

                          }) %>%
                      bindEvent(input$PrimarySelection_rows_selected)


                  observe({ req(session$userData$ServerWorkspaceInfo())
                            req(PrimarySelectionCriterion())
                            req(SecondarySelectionData())

                            # Get the index of the selected row using DT functionality
                            RowIndex <- input$SecondarySelection_rows_selected

                            if (PrimarySelectionCriterion() == "ByObject")
                            {
                                # Set selected server name
                                SelectedServerName(SecondarySelectionData()$ServerName[RowIndex])
                            }
                            if (PrimarySelectionCriterion() == "ByServer")
                            {
                                # Set selected object name
                                SelectedObjectName(SecondarySelectionData()$Object[RowIndex])
                            }

                          }) %>%
                      bindEvent(input$SecondarySelection_rows_selected)


                  DataObjectDetails <- reactive({ req(session$userData$ServerWorkspaceInfo())
                                                  req(SelectedObjectName())
                                                  req(SelectedServerName())

                                                  Data <- session$userData$ServerWorkspaceInfo()$ObjectDetails[[SelectedServerName()]][[SelectedObjectName()]]

                                                  if ("Feature" %in% names(Data))
                                                  {
                                                      Data <- Data %>%
                                                                  select(Feature,
                                                                         Exists,
                                                                         Type,
                                                                         NonMissingValueRate) %>%
                                                                  mutate(NonMissingValueRate = if_else(NonMissingValueRate > 0 & NonMissingValueRate < 0.01,
                                                                                                       "<1%",
                                                                                                       paste0(round(100 * NonMissingValueRate), "%"))) %>%
                                                                  ConvertLogicalToIcon()
                                                  }

                                                  return(Data)
                                                })


                  output$ObjectDetails <- DT::renderDT({ req(DataObjectDetails())

                                                         ColnamesVector <- NULL
                                                         if ("NonMissingValueRate" %in% names(DataObjectDetails())) { ColnamesVector <- c("NMVR" = "NonMissingValueRate") }

                                                         DT::datatable(data = DataObjectDetails(),
                                                                       class = "ui small compact inverted scrollable selectable table",
                                                                       colnames = ColnamesVector,
                                                                       editable = FALSE,
                                                                       escape = FALSE,
                                                                       #extensions = "FixedHeader",
                                                                       filter = "none",
                                                                       options = list(fixedHeader = TRUE,
                                                                                      info = FALSE,
                                                                                      layout = list(top = NULL),
                                                                                      ordering = FALSE,
                                                                                      paging = FALSE,
                                                                                      #scrollY = 400,
                                                                                      searching = FALSE,
                                                                                      columnDefs = list(list(width = "40px", targets = 1))),
                                                                       rownames = FALSE,
                                                                       selection = list(mode = "single",
                                                                                        target = "row"),
                                                                       style = "semanticui")
                                                      })


                  SelectedElementName <- reactive({ req(DataObjectDetails())
                                                    req(input$ObjectDetails_rows_selected)

                                                    # Get the index of the selected row using DT functionality
                                                    RowIndex <- input$ObjectDetails_rows_selected

                                                    # Returning name of feature selected in table
                                                    if ("Feature" %in% names(DataObjectDetails()))
                                                    {
                                                        return(DataObjectDetails()$Feature[RowIndex])
                                                    } else { return(NULL) }
                                                  })

                  DataValues <- reactive({  req(session$userData$ServerWorkspaceInfo())
                                            req(SelectedObjectName())
                                            req(SelectedElementName())

                                            session$userData$ServerWorkspaceInfo()$EligibleValues[[SelectedObjectName()]][[SelectedElementName()]] %>%
                                                { if (!is.null(.)) { select(., Value, Label) } else { NULL } }
                                         })

                  output$Values <- DT::renderDT({ req(DataValues())

                                                  DT::datatable(data = DataValues(),
                                                                class = "ui small compact scrollable table",
                                                                editable = FALSE,
                                                                escape = FALSE,
                                                                #extensions = "FixedHeader",
                                                                filter = "none",
                                                                options = list(fixedHeader = TRUE,
                                                                               info = FALSE,
                                                                               layout = list(top = NULL),
                                                                               ordering = FALSE,
                                                                               paging = FALSE,
                                                                               #scrollY = 400,
                                                                               searching = FALSE),
                                                                rownames = FALSE,
                                                                style = "semanticui")
                                                })

                  return(list(Object = SelectedObjectName,
                              Element = SelectedElementName))
               })
}
