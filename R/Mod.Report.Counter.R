

# --- MODULE: Report.Counter ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Report.Counter.UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(id = ns("Container"),
      style = "",

      div(style = "display: grid;
                   grid-template-columns: 1fr 9fr 1fr;",

          div(),

          div(style = "overflow: auto;",

              reactableOutput(outputId = ns("NestedView"))),

              #-----------------------------------------------------------------
              # div(class = "ui divider", style = "margin: 1.5em 0;"),
              #-----------------------------------------------------------------

              # uiOutput(outputId = ns("TableView"))),

          div()))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Report.Counter.Server <- function(id,
                                      CurationReport)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ns <- session$ns

                  output$NestedView <- renderReactable({  req(session$userData$CurationReport)

                                                          return(CreateTable.Counter.NestedView(CounterData = session$userData$CurationReport()$Counter))
                                                       })
               })
}


# ui <- fluidPage(
#   titlePanel("Module Template"),
#   Mod.Report.Counter.UI("my_module")
# )
#
# server <- function(input, output, session) {
#   # Pass the fixed local object directly — no reactive() wrapper needed
#   Mod.Report.Counter.Server("my_module", CurationReport = CurationReport)
# }
#
# shinyApp(ui, server)

