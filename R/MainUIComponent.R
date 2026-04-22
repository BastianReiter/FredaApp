
#' MainUIComponent
#'
#' Main UI component of CCPhosApp
#'
#' @noRd
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MainUIComponent <- function()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{

shiny.semantic::semanticPage(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # MAIN GRID
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Set margin 0 (default is 10 px)
    margin = "0",

    # Add custom CSS file
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/CCPhosStyle.css")),

    # Add extra JS to enable functionality of semantic accordion
    tags$script(language="javascript",
                "$(document).ready(function() { window.onload = function(){ $('.ui.accordion').accordion(); }; });"),

    # Title shown in browser
    title = "CCPhos App",

    # Initiate use of shinyjs functionality
    shinyjs::useShinyjs(),
    # shinyjs::extendShinyjs(script = "CCPhosJS.js",
    #                        functions = c("HoverTableRow",
    #                                      "SelectTableRow")),

    # Initiate use of waiter functionality
    waiter::useWaiter(),
    waiter::waiterShowOnLoad(html = waiter::spin_3(),
                             color = waiter::transparent(.5)),

    #waiter::autoWaiter(),


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # MAIN GRID
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    shiny.semantic::grid(id = "MainGrid",

        # Provide grid template (including definition of area names)
        grid_template = shiny.semantic::grid_template(

                              # --- Main grid layout for desktop devices ---
                              default = list(areas = rbind(c("header", "header", "header"),
                                                           c("leftside", "main", "rightside")),

                                             rows_height = c("minmax(100px, 10vh)", "86vh"),

                                             cols_width = c("0.5fr", "6fr", "1fr")),

                              # --- Main grid layout for mobile devices ---
                              mobile = list(areas = rbind(c("header", "header", "header"),
                                                          c("leftside", "main", "rightside")),

                                            rows_height = c("100px", "auto"),

                                            cols_width = c("1fr", "8fr", "1fr"))),

        area_styles = list(header = "padding: 10px 2em;
                                     background: rgb(5,73,150);
                                     background: linear-gradient(90deg, rgba(5,73,150,1) 8%, rgba(255,255,255,1) 100%);
                                     color: #595959;",

                           leftside = "min-width: 10em;
                                       padding: 10px;",
                           main = "padding: 10px;",
                           rightside = "padding: 10px;"),



        #--- HEADER ------------------------------------------------------------
        header = shiny.semantic::split_layout(style = "display: flex;      /* Set up flexbox to use 'justify-content: space-between' to enable free space between columns without specifying column width */
                                                       justify-content: space-between;
                                                       align-items: center;",

                                              img(src = "www/Logo_CCPhosApp.png",
                                                  alt = "CCPhos App Logo",
                                                  height = "80px"),

                                              uiOutput(outputId = "ProjectNameOutput"),

                                              ModConnectionStatus_UI("ConnectionStatus")),


        #--- LEFT SIDE COLUMN --------------------------------------------------
        leftside = div(style = "text-align: center;",
                       div(class = "ui vertical labeled icon menu",

                           a(id = "MenuItem_Start",
                             class = "item",
                             shiny.semantic::icon("plug"),
                             "CONNECT",
                             href = shiny.router::route_link("/")),

                           a(id = "MenuItem_Settings",
                             class = "item",
                             shiny.semantic::icon("cogs"),
                             "SETTINGS",
                             href = shiny.router::route_link("settings")),

                           a(id = "MenuItem_Prepare",
                             class = "item",
                             shiny.semantic::icon("utensils"),      # Alternatives: mug hot, box open,
                             "PREPARE",
                             href = shiny.router::route_link("prepare")),

                           a(id = "MenuItem_Explore",
                             class = "item",
                             shiny.semantic::icon("binoculars"),      # Alternatives: tv
                             "EXPLORE",
                             href = shiny.router::route_link("explore")),

                           a(id = "MenuItem_Analyze",
                             class = "item",
                             shiny.semantic::icon("calculator"),
                             "ANALYZE",
                             href = shiny.router::route_link("analyze")),

                           a(id = "MenuItem_Export",
                             class = "item",
                             shiny.semantic::icon("image"),
                             "EXPORT",
                             href = shiny.router::route_link("export"))),

                       textOutput(outputId = "TestMonitor")),


        #--- MAIN PANEL --------------------------------------------------------
        main = shiny.semantic::segment(class = "ui raised scrolling segment",
                                       style = "height: 100%;
                                                overflow: auto;",

                                       # Use shiny.router functionality to enable multi-page UI structure defined in UIPage() functions
                                       shiny.router::router_ui(shiny.router::route("/", UIPageStart()),
                                                               shiny.router::route("settings", UIPageSettings()),
                                                               shiny.router::route("prepare", UIPagePrepare()),
                                                               shiny.router::route("explore", UIPageExplore()),
                                                               shiny.router::route("analyze", UIPageAnalyze()),
                                                               shiny.router::route("export", UIPageExport()))
        ),


        #--- RIGHT SIDE COLUMN -------------------------------------------------
        rightside = div()
    )
)

}
