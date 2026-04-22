

# --- MODULE: Server Object Explorer ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @param id
#' @param ShowObjectDetailTable
#' @noRd
ModServerWorkspaceMonitor_UI <- function(id,
                                         ShowObjectDetailsTable = TRUE)
{
    ns <- NS(id)

    div(id = ns("ServerWorkspaceMonitorContainer"),
        class = paste(ifelse(ShowObjectDetailsTable == TRUE, "ui segment", "")),
        style = paste("height: 100%;",
                      ifelse(ShowObjectDetailsTable == TRUE, "", "overflow: auto;"),
                      "margin: 0;"),

        div(class = ifelse(ShowObjectDetailsTable == TRUE, "ui top attached label", ""),
            style = ifelse(ShowObjectDetailsTable == TRUE, "", "display: none;"),
            ifelse(ShowObjectDetailsTable == TRUE, "Server R Session Workspace", "")),

        div(style = paste("display: grid;",
                          ifelse(ShowObjectDetailsTable == TRUE, "grid-template-columns: 5fr 2fr;", "grid-template-columns: 1fr;"),
                          "grid-gap: 1em;
                           margin: 0;
                           height: 100%;"),

            div(style = ifelse(ShowObjectDetailsTable == TRUE,
                               "height: calc(100% - 30px); overflow: auto;",
                               "height: 100%;"),

                DTOutput(ns("WorkspaceObjects"),
                         width = "calc(100% - 14px)")),      # Width calculation necessary to avoid false overflow rendering (vertical scroll bar is approx. 14 px wide)

            div(style = paste(ifelse(ShowObjectDetailsTable == TRUE, "display: block;", "display: none;"),
                              "height: calc(100% - 30px);
                               overflow: auto;"),

                DTOutput(ns("ObjectDetails"),
                         width = "calc(100% - 14px)"))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @param id
#' @param input
#' @param output
#' @param session
#' @noRd
ModServerWorkspaceMonitor_Server <- function(id)
{
    moduleServer(id,
                 function(input, output, session)
                 {
                      ns <- session$ns

                      output$WorkspaceObjects <- renderDT({ req(session$userData$ServerWorkspaceInfo())

                                                            DataWorkspaceOverview <- session$userData$ServerWorkspaceInfo()$Overview %>%
                                                                                          ConvertLogicalToIcon()

                                                            DT::datatable(data = DataWorkspaceOverview,
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


                      SelectedObjectName <- reactive({ req(session$userData$ServerWorkspaceInfo())
                                                       req(input$WorkspaceObjects_rows_selected)

                                                       # Get the index of the selected row using DT functionality
                                                       RowIndex <- input$WorkspaceObjects_rows_selected

                                                       # Returning name of object selected in table
                                                       session$userData$ServerWorkspaceInfo()$Overview$Object[RowIndex]
                                                     })


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
