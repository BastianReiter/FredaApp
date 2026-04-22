
#' UIWidgetHeaderBar
#'
#' @noRd
UIWidgetHeaderBar <- function()
{
  shiny.semantic::split_layout(style = "display: flex;      /* Set up flexbox to use 'justify-content: space-between' to enable free space between columns without specifying column width */
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
                                                            uiOutput("StatusIcon"),

                                                            # Status Text
                                                            textOutput("StatusText"),

                                                            # Button for switching back to console session
                                                            shiny.semantic::action_button("LoginButton",
                                                                                          label = "Login"),

                                                            # Logout Button
                                                            shiny.semantic::action_button("LogoutButton",
                                                                                          label = "Logout")))
}
