

# --- MODULE: CurationReport ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModCurationReport_UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(id = ns("CurationReportContainer"),
      style = "",

      div(style = "display: grid;
                   grid-template-columns: 1fr 3fr 1fr;",

          div(),

          div(class = "ui segment",
              style = "margin: 2em;",

              div(class = "ui top attached label",
                  "Table Entry Counts"),

              uiOutput(outputId = ns("EntryCounts"))),

          div()),

      uiOutput(outputId = ns("DiagnosisClassification")))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModCurationReport_Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ns <- session$ns

                  # Dynamically create empty UI elements to be filled further down
                  output$EntryCounts <- renderUI({  req(session$userData$CurationReport())

                                                    TableOutputList <- list()

                                                    TableList <- session$userData$CurationReport()$EntryCounts

                                                    # Using for-loop instead of purrr-functionality because there is no map-function that can access both names and index of list items
                                                    for (i in 1:length(TableList))
                                                    {
                                                        TableOutput <- div(style = "margin-top: 1em;",
                                                                           div(class = "ui small grey ribbon label",
                                                                               names(TableList)[i]),
                                                                           div(style = "width: 1000px; overflow: auto;",
                                                                               uiOutput(outputId = ns(paste0("EntryCounts_", i)))))

                                                        TableOutputList <- list(TableOutputList,
                                                                                TableOutput)
                                                    }

                                                    # Convert into tagList for html output
                                                    do.call(tagList, TableOutputList)
                                                })


                  # Create table HTML code to be fed into UI output elements
                  TableList_EntryCounts <- reactive({ req(session$userData$RDSTableCheck())

                                                      HTMLTables <- session$userData$CurationReport()$EntryCounts %>%
                                                                        map(function(TableData)
                                                                            {
                                                                                TableData <- TableData %>%
                                                                                                mutate(PrimaryExclusion = paste0(ExcludedPrimary, " (", round(ExcludedPrimary.Proportion * 100, 0), "%)"),
                                                                                                       # InterimCount = paste0(AfterPrimaryExclusion, " (", round(AfterPrimaryExclusion.Proportion * 100, 0), "%)"),
                                                                                                       SecondaryExclusion = paste0(ExcludedSecondary, " (", round(ExcludedSecondary.Proportion * 100, 0), "%)"),
                                                                                                       SecondaryRedundancyExclusion = paste0(ExcludedSecondaryRedundancy, " (", round(ExcludedSecondaryRedundancy.Proportion * 100, 0), "%)"),
                                                                                                       FinalCount = paste0(AfterSecondaryExclusion, " (", round(AfterSecondaryExclusion.Proportion * 100, 0), "%)")) %>%
                                                                                                select(Server,
                                                                                                       InitialCount,
                                                                                                       PrimaryExclusion,
                                                                                                       # InterimCount,
                                                                                                       SecondaryExclusion,
                                                                                                       SecondaryRedundancyExclusion,
                                                                                                       FinalCount)

                                                                                # Turn data frame into html object
                                                                                DataFrameToHtmlTable(DataFrame = TableData,
                                                                                                     ColContentHorizontalAlign = "center",
                                                                                                     ColumnLabels = c(InitialCount = "Initial Count",
                                                                                                                      PrimaryExclusion = "Primary Exclusion",
                                                                                                                      # InterimCount = "Interim Count",
                                                                                                                      SecondaryExclusion = "Secondary Exclusion",
                                                                                                                      SecondaryRedundancyExclusion = "Secondary Redundancy Exclusion",
                                                                                                                      FinalCount = "Final Count"),
                                                                                                     SemanticTableCSSClass = "ui small compact inverted scrollable structured table")
                                                                            })

                                                      HTMLTables
                                                    })

                  output[["EntryCounts_1"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[1]] })
                  output[["EntryCounts_2"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[2]] })
                  output[["EntryCounts_3"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[3]] })
                  output[["EntryCounts_4"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[4]] })
                  output[["EntryCounts_5"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[5]] })
                  output[["EntryCounts_6"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[6]] })
                  output[["EntryCounts_7"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[7]] })
                  output[["EntryCounts_8"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[8]] })
                  output[["EntryCounts_9"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[9]] })
                  output[["EntryCounts_10"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[10]] })
                  output[["EntryCounts_11"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[11]] })
                  output[["EntryCounts_12"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[12]] })
                  output[["EntryCounts_13"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[13]] })
                  output[["EntryCounts_14"]] <- renderUI({ req(TableList_EntryCounts); TableList_EntryCounts()[[14]] })


                  output$DiagnosisClassification <- renderUI({  req(session$userData$CurationReport())

                                                                # Modify table data
                                                                TableData <- session$userData$CurationReport()$DiagnosisClassification

                                                                if (!is.null(TableData))
                                                                {
                                                                   DataFrameToHtmlTable(DataFrame = as.data.frame(TableData),
                                                                                        ColContentHorizontalAlign = "center",
                                                                                        ColumnLabels = c(ServerName = "Server"),
                                                                                        SemanticTableCSSClass = "ui small compact celled structured table")
                                                                }
                                                             })

               })
}

