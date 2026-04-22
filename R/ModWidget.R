

# --- MODULE: ModWidget ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModWidget_UI <- function(id,
                         Title = "CCPhos",
                         WidgetMainUI)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  shiny.semantic::semanticPage(

      # Set margin 0 (default is 10 px)
      margin = "0",

      # Add custom CSS file
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/CCPhosStyle.css")),

      # Title shown in browser
      title = Title,

      # Initiate use of shinyjs functionality
      shinyjs::useShinyjs(),

      # Initiate use of waiter functionality
      waiter::useWaiter(),
      waiter::waiterShowOnLoad(html = waiter::spin_3(),
                               color = waiter::transparent(.5)),

      #textOutput(outputId = "TestMonitor"),

      #-----------------------------------------------------------------------

      shiny.semantic::grid(id = "MainGrid",

          # Provide grid template (including definition of area names)
          grid_template = shiny.semantic::grid_template(

                                # --- Main grid layout for desktop devices ---
                                default = list(areas = rbind("header",
                                                             "main"),

                                               rows_height = c("minmax(40px, 6vh)", "90vh")),

                                # --- Main grid layout for mobile devices ---
                                mobile = list(areas = rbind("header",
                                                            "main"),

                                              rows_height = c("100px", "auto"))),

          area_styles = list(header = "padding: 10px 1em;
                                       background: rgb(5,73,150);
                                       background: linear-gradient(90deg, rgba(5,73,150,1) 8%, rgba(255,255,255,1) 100%);
                                       color: #595959;",
                             main = "padding: 10px;"),

          #--- HEADER --------------------------------------------------------

          header = shiny.semantic::split_layout(style = "display: flex;      /* Set up flexbox to use 'justify-content: space-between' to enable free space between columns without specifying column width */
                                                         justify-content: space-between;
                                                         align-items: center;",

                                                img(src = "www/Logo_CCPhosApp.png",
                                                    alt = "CCPhos App Logo",
                                                    height = "20px"),

                                                shiny.semantic::split_layout(style = "justify-self: end;
                                                                                      grid-template-columns: auto 14em auto;    /* Fixed width of middle column */
                                                                                      justify-content: end;    /* Align grid items horizontally to the right side */
                                                                                      align-items: center;    /* Align grid items vertically */
                                                                                      background: #f9fafb;
                                                                                      box-shadow: 0 2px 8px 0 rgba(34, 36, 38, 0.05) inset;
                                                                                      border: 2px solid rgb(5,73,150);
                                                                                      border-radius: 4px;",

                                                                             cell_args = "padding: 4px;",

                                                                             # Status Icon
                                                                             uiOutput(ns("StatusIcon")),

                                                                             # Status Text
                                                                             textOutput(ns("StatusText")),

                                                                             # Button for switching back to console session
                                                                             shiny.semantic::action_button(ns("LoginButton"),
                                                                                                           label = "Login"),

                                                                             # Logout Button
                                                                             shiny.semantic::action_button(ns("LogoutButton"),
                                                                                                           label = "Logout"))),

          #--- MAIN PANEL ----------------------------------------------------
          main = shiny.semantic::segment(class = "ui raised scrolling segment",
                                         style = "height: 100%;
                                                  overflow: auto;",

                                         WidgetMainUI(ns))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModWidget_Server <- function(id,
                             WidgetServerLogic,
                             EnableLiveConnection)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  # Call widget-specific server logic passed as function
                  WidgetServerLogic(session)

                  #-----------------------------------------------------------
                  # Generic logic for all widgets
                  #-----------------------------------------------------------

                  WaiterScreen <- CreateWaiterScreen(ID = "WaiterScreenContainer")

                  if (EnableLiveConnection == FALSE)
                  {
                      shinyjs::disable("LoginButton")
                      shinyjs::disable("LogoutButton")
                  }

                  observe({ WaiterScreen$show()
                            on.exit({ WaiterScreen$hide() })

                            # Trigger dsCCPhosClient::ConnectToCCP() ...
                            Connections <- dsCCPhosClient::ConnectToCCP(ServerSpecifications = session$userData$ServerSpecifications())

                            # ... and assign return to session$userData
                            session$userData$DSConnections(Connections)

                         }) %>%
                      bindEvent(input$ButtonLogin)


                  output$StatusIcon <- renderUI({ if (EnableLiveConnection == TRUE && is.list(session$userData$DSConnections())) { shiny.semantic::icon(class = "large green power off") }
                                                  else { shiny.semantic::icon(class = "large grey power off") } })

                  output$StatusText <- renderText({ if (EnableLiveConnection == TRUE && is.list(session$userData$DSConnections())) { "Connected to CCP" }
                                                    else { "No live connection" } })

                  observe({ if (EnableLiveConnection == TRUE && is.list(session$userData$DSConnections()))
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
