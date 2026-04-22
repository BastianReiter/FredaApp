

# --- MODULE: ServerOpalDBMonitor ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModServerOpalDBMonitor_UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(id = ns("ServerOpalDBMonitorContainer"),
      #class = "ui scrollable segment",
      style = "height: 100%;
               overflow: auto;",

      uiOutput(outputId = ns("ServerOpalDBMonitor")))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModServerOpalDBMonitor_Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  output$ServerOpalDBMonitor <- renderUI({  req(session$userData$ServerOpalDBInfo())

                                                            # Modify table data
                                                            TableData <- session$userData$ServerOpalDBInfo() %>%
                                                                              select(-CheckOpalTableAvailability)

                                                            if (!is.null(TableData))
                                                            {
                                                               DataFrameToHtmlTable(DataFrame = TableData,
                                                                                    ColContentHorizontalAlign = "center",
                                                                                    ColumnLabels = c(SiteName = "Site"),
                                                                                    SemanticTableCSSClass = "ui small compact celled structured table",
                                                                                    TurnLogicalsIntoIcons = TRUE)
                                                            }
                                                          })
               })
}

