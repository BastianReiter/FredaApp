

# --- MODULE: DataTransformationMonitor ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModDataTransformationMonitor_UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(style = "display: grid;
               grid-template-columns: 14em auto;
               grid-gap: 4em;",


      div(selectInput(inputId = ns("ServerName"),
                      label = "Select Server",
                      choices = ""),

          selectInput(inputId = ns("MonitorTableName"),
                      label = "Select Table",
                      choices = ""),

          br(), br(),

          shiny.semantic::toggle(input_id = ns("ShowNonOccurringValues"),
                                 label = "Show non-occurring eligible values",
                                 is_marked = FALSE)),


      div(style = "position: relative;",

          div(id = ns("WaiterScreenContainer"),
              style = "position: absolute;
                       height: 100%;
                       width: 100%;
                       top: 0.5em;
                       left: 0;"),

          div(style = "display: grid;
                       grid-template-columns: 1fr 2fr;
                       grid-gap: 2em;
                       height: 100%;",

              div(class = "ui segment",
                  style = "margin: 0;",

                  div(class = "ui top attached label",
                      "Value eligibility overview"),

                  uiOutput(outputId = ns("EligibilityOverview"))),

              div(class = "ui segment",
                  style = "margin: 0;",

                  div(class = "ui top attached label",
                      "Value transformation tracks"),

                  uiOutput(outputId = ns("TransformationTracks"))))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModDataTransformationMonitor_Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # Setting up loading behavior
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  ns <- session$ns
                  WaiterScreen <- CreateWaiterScreen(ID = ns("WaiterScreenContainer"))

                  LoadingOn <- function()
                  {
                      shinyjs::disable("ServerName")
                      shinyjs::disable("MonitorTableName")
                      shinyjs::disable("ShowNonOccurringValues")
                      WaiterScreen$show()
                  }

                  LoadingOff <- function()
                  {
                      shinyjs::enable("ServerName")
                      shinyjs::enable("MonitorTableName")
                      shinyjs::enable("ShowNonOccurringValues")
                      WaiterScreen$hide()
                  }
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


                  # Update input elements when data changes
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  observe({ updateSelectInput(session = getDefaultReactiveDomain(),
                                              inputId = "ServerName",
                                              choices = names(session$userData$CurationReport()$Transformation),
                                              selected = names(session$userData$CurationReport()$Transformation)[1])

                            updateSelectInput(session = getDefaultReactiveDomain(),
                                              inputId = "MonitorTableName",
                                              choices = names(session$userData$CurationReport()$Transformation[[1]]$Monitors),
                                              selected = names(session$userData$CurationReport()$Transformation[[1]]$Monitors)[1])
                          }) %>%
                      bindEvent(session$userData$CurationReport())



                  #output$TestBox <- renderText({ paste0(names(session$userData$CurationReport()), collapse = ", ")  })


                  # Reactive expression: Data for eligibility overview
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  Data_EligibilityOverview <- reactive({ req(session$userData$CurationReport())
                                                         req(input$ServerName)
                                                         req(input$MonitorTableName)

                                                         if (!is.null(session$userData$CurationReport()))
                                                         {
                                                             # - Restructure eligibility overview table to meet requirements of plot function
                                                             # - Create separate data frames for each 'Feature' value
                                                             # - Columns in final object:
                                                             #   - 'Feature': contains names of features
                                                             #   - 'data': plot data for feature-specific plot
                                                             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                             session$userData$CurationReport()$Transformation[[input$ServerName]]$EligibilityOverviews[[input$MonitorTableName]] %>%
                                                                 select(-ends_with(".Proportional")) %>%
                                                                 pivot_longer(cols = c(Raw, Harmonized, Recoded, Final),
                                                                              names_to = "Stage",
                                                                              values_to = "Count") %>%
                                                                 pivot_wider(names_from = "Eligibility",
                                                                             values_from = "Count") %>%
                                                                 nest(.by = Feature)      # 'Split' the whole table into smaller data frames for each 'Feature' value
                                                         }
                                                       })


                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # Render reactive output: Plots for eligibility overview
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                  # Dynamically create empty UI elements to be filled further down
                  output$EligibilityOverview <- renderUI({ req(Data_EligibilityOverview())

                                                           # Assign loading behavior
                                                           LoadingOn()
                                                           on.exit(LoadingOff())

                                                           # Create list of divs with (empty) plotlyOutput objects with an assigned Output ID for plots
                                                           PlotOutputList <- Data_EligibilityOverview()$Feature %>%
                                                                                  imap(function(feature, index)
                                                                                       {
                                                                                          div(div(class = "ui small grey ribbon label",
                                                                                                  feature),   # Plot label
                                                                                              plotly::plotlyOutput(outputId = ns(paste0("PlotEligibility_", index)),
                                                                                                                   height = "120px"))
                                                                                       })

                                                           # Convert into tagList for html output
                                                           do.call(tagList, PlotOutputList)
                                                         })

                  # List of plotly-objects to be assigned to output
                  PlotList_EligibilityOverview <- reactive({ req(Data_EligibilityOverview())

                                                             Data_EligibilityOverview()$Feature %>%
                                                                  map(function(feature)
                                                                      {
                                                                          PlotData <- as.data.frame(filter(Data_EligibilityOverview(), Feature == feature)$data[[1]])

                                                                          plotly::plot_ly(data = PlotData,      # Must be a data.frame, not a tibble!
                                                                                          x = ~Stage,
                                                                                          y = ~Eligible,
                                                                                          type = "bar",
                                                                                          name = "Eligible",
                                                                                          color = I(dsCCPhosClient::CCPhosColors$Green),
                                                                                          showlegend = FALSE) %>%
                                                                                      plotly::add_trace(y = ~Ineligible,
                                                                                                        name = "Ineligible",
                                                                                                        color = I(dsCCPhosClient::CCPhosColors$Red)) %>%
                                                                                      plotly::add_trace(y = ~Missing,
                                                                                                        name = "Missing",
                                                                                                        color = I(dsCCPhosClient::CCPhosColors$MediumGrey)) %>%
                                                                                      plotly::layout(font = list(size = 11,
                                                                                                                 color = I(dsCCPhosClient::CCPhosColors$DarkGrey)),
                                                                                                     xaxis = list(#side = "top",
                                                                                                                  title = "",
                                                                                                                  categoryorder = "array",
                                                                                                                  categoryarray = c("Raw", "Harmonized", "Recoded", "Final")),
                                                                                                     yaxis = list(title = "",
                                                                                                                  showticklabels = FALSE),
                                                                                                     barmode = "stack") %>%
                                                                                      plotly::config(displayModeBar = FALSE)
                                                                      })
                                                           })

                  # For now, each output element must be assigned explicitly
                  output[["PlotEligibility_1"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[1]] })
                  output[["PlotEligibility_2"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[2]] })
                  output[["PlotEligibility_3"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[3]] })
                  output[["PlotEligibility_4"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[4]] })
                  output[["PlotEligibility_5"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[5]] })
                  output[["PlotEligibility_6"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[6]] })
                  output[["PlotEligibility_7"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[7]] })
                  output[["PlotEligibility_8"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[8]] })
                  output[["PlotEligibility_9"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[9]] })
                  output[["PlotEligibility_10"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[10]] })
                  output[["PlotEligibility_11"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[11]] })
                  output[["PlotEligibility_12"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[12]] })
                  output[["PlotEligibility_13"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[13]] })
                  output[["PlotEligibility_14"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[14]] })
                  output[["PlotEligibility_15"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[15]] })
                  output[["PlotEligibility_16"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[16]] })
                  output[["PlotEligibility_17"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[17]] })
                  output[["PlotEligibility_18"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[18]] })
                  output[["PlotEligibility_19"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[19]] })
                  output[["PlotEligibility_20"]] <- plotly::renderPlotly({ req(PlotList_EligibilityOverview); PlotList_EligibilityOverview()[[20]] })



                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # Reactive expression: Data for transformation track table
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  Data_TransformationTracks <- reactive({ req(session$userData$CurationReport())
                                                          req(input$ServerName)
                                                          req(input$MonitorTableName)

                                                          if (!is.null(session$userData$CurationReport()))
                                                          {
                                                              session$userData$CurationReport()$Transformation[[input$ServerName]]$Monitors[[input$MonitorTableName]] %>%
                                                                  { if (input$ShowNonOccurringValues == FALSE) { filter(., IsOccurring == TRUE) } else {.} } %>%       # Filter for occurring values only, if option checked in UI
                                                                  mutate(CellCSSClass.Value.Raw = case_when(IsOccurring == FALSE & IsEligible.Raw == TRUE ~ "CellCSSClass_Info",
                                                                                                            IsOccurring == TRUE & IsEligible.Raw == TRUE ~ "CellCSSClass_Success",
                                                                                                            !is.na(Value.Raw) & IsEligible.Raw == FALSE ~ "CellCSSClass_Failure",
                                                                                                            is.na(Value.Raw) ~ "CellCSSClass_Grey",
                                                                                                            TRUE ~ "None"),
                                                                         CellCSSClass.Value.Harmonized = case_when(IsOccurring == TRUE & IsEligible.Harmonized == TRUE ~ "CellCSSClass_Success",
                                                                                                                   !is.na(Value.Harmonized) & IsEligible.Harmonized == FALSE ~ "CellCSSClass_Failure",
                                                                                                                   is.na(Value.Harmonized) ~ "CellCSSClass_Grey",
                                                                                                                   TRUE ~ "None"),
                                                                         CellCSSClass.Value.Recoded = case_when(IsOccurring == TRUE & IsEligible.Recoded == TRUE ~ "CellCSSClass_Success",
                                                                                                                !is.na(Value.Recoded) & IsEligible.Recoded == FALSE ~ "CellCSSClass_Failure",
                                                                                                                is.na(Value.Recoded) ~ "CellCSSClass_Grey",
                                                                                                                TRUE ~ "None"),
                                                                         CellCSSClass.Value.Final = case_when(!is.na(Value.Final) ~ "CellCSSClass_Success",
                                                                                                              is.na(Value.Final) ~ "CellCSSClass_Grey",
                                                                                                              TRUE ~ "None"))
                                                          }
                                                        })


                  # Render reactive output: TransformationTracks
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  output$TransformationTracks <- renderUI({   req(Data_TransformationTracks())

                                                              # Assign loading behavior
                                                              LoadingOn()
                                                              on.exit(LoadingOff())

                                                              # Modify data for rendering purposes
                                                              TableData <- Data_TransformationTracks() %>%
                                                                                select(Feature,
                                                                                       Value.Raw,
                                                                                       Value.Harmonized,
                                                                                       Value.Recoded,
                                                                                       Value.Final,
                                                                                       Count.Raw,
                                                                                       starts_with("CellCSSClass"))

                                                              if (!is.null(TableData))
                                                              {
                                                                  DataFrameToHtmlTable(DataFrame = TableData,
                                                                                       CategoryColumn = "Feature",
                                                                                       ColContentHorizontalAlign = "center",
                                                                                       ColumnLabels = c(Value.Raw = "Raw",
                                                                                                        Value.Harmonized = "Harmonized",
                                                                                                        Value.Recoded = "Recoded",
                                                                                                        Value.Final = "Final",
                                                                                                        Count.Raw = "Count"),
                                                                                       SemanticTableCSSClass = "ui small compact celled structured table")
                                                              } })
               })
}
