

# --- MODULE: Report.Log ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Report.Log.UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  # div(id = ns("Container"),
  #     style = "",
  #
  #     div(style = "display: grid;
  #                  grid-template-columns: 1fr 3fr 1fr;",
  #
  #         div(),
  #
  #         div(class = "ui segment",
  #             style = "margin: 2em;",
  #
  #             div(class = "ui top attached label",
  #                 "Table Entry Counts"),
  #
  #             uiOutput(outputId = ns("EntryCounts"))),
  #
  #         div()),
  #
  #     uiOutput(outputId = ns("DiagnosisClassification")))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Report.Log.Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ns <- session$ns



               })
}

