

# --- MODULE: ServerExplorer ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @param id
#' @noRd
ModServerExplorer_UI <- function(id)
{
    ns <- NS(id)

    div(id = ns("ServerExplorerContainer"),
        class = "ui segment",
        style = "height: 100%;
                 margin: 0;",

        div(button(ns("ByObjectButton"),
                   label = "by Object",
                   class = "ui left attached toggle button"),
            button(ns("ByServerButton"),
                   label = "by Server",
                   class = "ui right attached toggle button")),

        # div(class = "ui top attached label",
        #     "Object"),

        div(style = "display: grid;
                     grid-template-columns: 2fr 2fr 2fr 2fr;
                     grid-gap: 1em;
                     margin: 0;
                     height: 100%;",

            div(style = "height: calc(100% - 30px);
                         overflow: auto;",

                DTOutput(ns("PrimarySelection"),
                         width = "95%")),      # Width calculation necessary to avoid false overflow rendering (vertical scroll bar is approx. 14 px wide)

            div(style = "height: calc(100% - 30px);
                         overflow: auto;",

                DTOutput(ns("SecondarySelection"),
                         width = "95%")),

            div(style = "height: calc(100% - 30px);
                         overflow: auto;",

                DTOutput(ns("ObjectDetails"),
                         width = "95%")),

            div(style = "height: calc(100% - 30px);
                         overflow: auto;",

                DTOutput(ns("EligibleValues"),
                         width = "95%"))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @param id
#' @param input
#' @param output
#' @param session
#' @noRd
ModServerExplorer_Server <- function(id,
                                     Standalone = FALSE,
                                     Offline = FALSE,
                                     ServerWorkspaceInfo = NULL)
{
    require(dplyr)



    moduleServer(id,
                 function(input, output, session)
                 {
                      ns <- session$ns

                      SelectedObjectName <- reactiveVal(NULL)
                      SelectedServerName <- reactiveVal(NULL)
                      PrimarySelectionCriterion <- reactiveVal("ByObject")

                      observe({ SelectedObjectName(session$userData$ServerWorkspaceInfo()$Overview$Object[1]) }) %>%
                          bindEvent(session$userData$ServerWorkspaceInfo())

                      observe({ SelectedServerName(session$userData$ServerWorkspaceInfo()$Overview$ServerName[1]) }) %>%
                          bindEvent(session$userData$ServerWorkspaceInfo())

                      observe({ PrimarySelectionCriterion("ByObject") }) %>%
                          bindEvent(input$ByObjectButton)

                      observe({ PrimarySelectionCriterion("ByServer") }) %>%
                          bindEvent(input$ByServerButton)


                      # Reset object and server selections when primary selection criterion changes
                      observe({ req(session$userData$ServerWorkspaceInfo())
                                SelectedObjectName(session$userData$ServerWorkspaceInfo()$Overview$Object[1])
                                SelectedServerName(session$userData$ServerWorkspaceInfo()$Overview$ServerName[1])
                              }) %>%
                            bindEvent(PrimarySelectionCriterion())


                      PrimarySelectionData <- reactive({  req(session$userData$ServerWorkspaceInfo())
                                                          req(PrimarySelectionCriterion())

                                                          if (PrimarySelectionCriterion() == "ByObject")
                                                          {
                                                              PrimarySelectionData <- session$userData$ServerWorkspaceInfo()$Overview %>%
                                                                                          distinct(Object, Class)
                                                          }
                                                          if (PrimarySelectionCriterion() == "ByServer")
                                                          {
                                                              PrimarySelectionData <- session$userData$ServerWorkspaceInfo()$Overview %>%
                                                                                          distinct(ServerName)
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
                                                                                              filter(Object == SelectedObjectName()) %>%
                                                                                              select(ServerName,
                                                                                                     ObjectExists,
                                                                                                     Length,
                                                                                                     RowCount) %>%
                                                                                              ConvertLogicalToIcon()
                                                            }
                                                            if (PrimarySelectionCriterion() == "ByServer" & !is.null(SelectedServerName()))
                                                            {
                                                                SecondarySelectionData <- session$userData$ServerWorkspaceInfo()$Overview %>%
                                                                                              filter(ServerName == SelectedServerName()) %>%
                                                                                              select(-ServerName) %>%
                                                                                              ConvertLogicalToIcon()
                                                            }
                                                            return(SecondarySelectionData)
                                                         })

                      output$PrimarySelection <- renderDT({ req(session$userData$ServerWorkspaceInfo())
                                                            req(PrimarySelectionData())

                                                            DT::datatable(data = PrimarySelectionData(),
                                                                          class = "ui small compact scrollable selectable table",
                                                                          editable = FALSE,
                                                                          escape = FALSE,
                                                                          filter = "none",
                                                                          options = list(info = FALSE,
                                                                                         ordering = FALSE,
                                                                                         paging = FALSE,
                                                                                         searching = FALSE),
                                                                          rownames = FALSE,
                                                                          selection = list(mode = "single",
                                                                                           target = "row"),
                                                                          style = "semanticui")
                                                          })


                      output$SecondarySelection <- renderDT({ req(session$userData$ServerWorkspaceInfo())
                                                              req(SecondarySelectionData())

                                                              DT::datatable(data = SecondarySelectionData(),
                                                                            class = "ui small compact scrollable selectable table",
                                                                            editable = FALSE,
                                                                            escape = FALSE,
                                                                            filter = "none",
                                                                            options = list(info = FALSE,
                                                                                           ordering = FALSE,
                                                                                           paging = FALSE,
                                                                                           searching = FALSE),
                                                                            rownames = FALSE,
                                                                            selection = list(mode = "single",
                                                                                             target = "row"),
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

                                                      session$userData$ServerWorkspaceInfo()$Details[[SelectedObjectName()]]$Structure
                                                    })


                      output$ObjectDetails <- renderDT({ req(DataObjectDetails())

                                                         DT::datatable(data = DataObjectDetails(),
                                                                       class = "ui small compact inverted scrollable selectable table",
                                                                       editable = FALSE,
                                                                       escape = FALSE,
                                                                       filter = "none",
                                                                       options = list(info = FALSE,
                                                                                      ordering = FALSE,
                                                                                      paging = FALSE,
                                                                                      searching = FALSE,
                                                                                      layout = list(top = NULL)),
                                                                       rownames = FALSE,
                                                                       selection = list(mode = "single",
                                                                                        target = "row"),
                                                                       style = "semanticui")
                                                      })


                      SelectedElementName <- reactive({ req(DataObjectDetails())
                                                        req(input$ObjectDetails_rows_selected)

                                                        # Get the index of the selected row using DT functionality
                                                        RowIndex <- input$ObjectDetails_rows_selected

                                                        # Returning name of element selected in table
                                                        DataObjectDetails()$Element[RowIndex]
                                                      })

                      return(list(Object = SelectedObjectName,
                                  Element = SelectedElementName))
                 })
}
