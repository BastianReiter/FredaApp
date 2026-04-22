

# --- MODULE: ConnectionStatus ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModConnectionStatus_UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  shiny.semantic::split_layout(style = "justify-self: end;
                                        grid-template-columns: auto 14em auto;    /* Fixed width of middle column */
                                        justify-content: end;    /* Align grid items horizontally to the right side */
                                        align-items: center;    /* Align grid items vertically */
                                        background: #f9fafb;
                                        box-shadow: 0 2px 8px 0 rgba(34, 36, 38, 0.05) inset;
                                        border: 2px solid rgb(5,73,150);
                                        border-radius: 4px;",

                               cell_args = "padding: 10px;",

                               # Status Icon
                               uiOutput(ns("StatusIcon")),

                               # Status Text
                               textOutput(ns("StatusText")),

                               # Button for switching back to console session
                               shiny.semantic::action_button(ns("ConsoleButton"),
                                                             label = "Back to Console"),

                               # Logout Button
                               shiny.semantic::action_button(ns("LogoutButton"),
                                                             label = "Logout"))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModConnectionStatus_Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                    output$StatusIcon <- renderUI({ if (is.list(session$userData$DSConnections())) { shiny.semantic::icon(class = "large green power off") }
                                                    else { shiny.semantic::icon(class = "large grey power off") } })

                    output$StatusText <- renderText({ if (is.list(session$userData$DSConnections())) { "Connected to CCP" }
                                                      else { "No live connection" } })

                    observe({ if (is.list(session$userData$DSConnections()))
                              {
                                  shinyjs::enable("ConsoleButton")
                                  shinyjs::enable("LogoutButton")
                              }
                              else
                              {
                                  shinyjs::disable("ConsoleButton")
                                  shinyjs::disable("LogoutButton")
                              } })


                    # Switch console button functionality

                    # Logout button functionality
                    observe({ DSI::datashield.logout(session$userData$DSConnections())
                              session$userData$DSConnections(NULL) }) %>%
                        bindEvent(input$LogoutButton)

               })
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ModConnectionStatusApp <- function()
# {
#     ui <- fluidPage(
#         ModConnectionStatus_UI("Test")
#     )
#
#     server <- function(input, output, session)
#     {
#         ModConnectionStatus_Server("Test")
#     }
#
#     shinyApp(ui, server)
# }

# Run app
#ModConnectionStatusApp()


