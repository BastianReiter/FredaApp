
#' UIPagePrepare
#'
#' @noRd
UIPagePrepare <- function()
{
  div(id = "PagePrepare",

      h4(class = "ui dividing header",
         "Data preparation"),

      div(class = "ui accordion",      # Note: For this to work extra JS script is necessary (see MainUIComponent())

          div(class = "active title AccordionHeader",
              shiny.semantic::icon(class = "dropdown"),
              "Processing Terminal"),

          div(class = "active content",

              div(class = "ui segment",
                  style = "background: #f9fafb;
                           border-color: rgba(34, 36, 38, 0.15);
                           box-shadow: 0 2px 25px 0 rgba(34, 36, 38, 0.05) inset;
                           height: 30em;",

                  div(style = "display: grid;
                               height: 100%;
                               grid-template-columns: 20em auto;
                               background: none;",

                      div(style = "display: grid;
                                   align-content: center;",

                          uiOutput("Step_Connect"),
                          uiOutput("Step_CheckServerRequirements"),
                          uiOutput("Step_LoadData"),
                          uiOutput("Step_CurateData"),
                          uiOutput("Step_AugmentData")),

                      div(id = "TerminalContainer",
                          style = "height: 100%;
                                   padding: 0 1em 0 2em;",

                          shinyjs::hidden(div(id = "Terminal_CheckServerRequirements",
                                              ModProcessingTerminal_UI("CheckServerRequirements",
                                                                       ButtonLabel = "Check server requirements"))),

                          shinyjs::hidden(div(id = "Terminal_LoadData",
                                              ModProcessingTerminal_UI("LoadData",
                                                                       ButtonLabel = "Load data"))),

                          shinyjs::hidden(div(id = "Terminal_CurateData",
                                              ModProcessingTerminal_UI("CurateData",
                                                                       ButtonLabel = "Start data curation"))),

                          shinyjs::hidden(div(id = "Terminal_AugmentData",
                                              ModProcessingTerminal_UI("AugmentData",
                                                                       ButtonLabel = "Start data augmentation")))))))),


      #-----------------------------------------------------------------------
      div(class = "ui divider",
          style = "margin: 1.5em 0;"),
      #-----------------------------------------------------------------------


      div(class = "ui accordion",      # Note: For this to work an extra JS script is necessary (see MainUIComponent())

          div(class = "active title AccordionHeader",
              shiny.semantic::icon(class = "dropdown"),
              "Checkpoints"),

          div(class = "active content",

              div( # Child of 'active content' in accordion has to be a container div. Can not be the grid div directly, this leads to loss of grid structure when accordion functionality is performed.
                  div(style = "display: grid;
                               grid-template-columns: 4fr 1fr;
                               grid-gap: 2em;
                               height: 11em;",

                      div(style = "height: 100%;
                                   overflow: auto;
                                   margin: 0;",

                          ModCheckpoints_UI("Checkpoints")),

                      div(style = "display: grid;
                                   align-items: center;
                                   margin-right: 1em;",

                          shiny.semantic::action_button("ButtonModifyConnections",
                                                        label = "Modify Connections")))))),


      #-----------------------------------------------------------------------
      div(class = "ui divider",
          style = "margin: 1.5em 0;"),
      #-----------------------------------------------------------------------


      div(class = "ui accordion",      # Note: For this to work an extra JS script is necessary (see MainUIComponent())

          div(class = "active title AccordionHeader",
              shiny.semantic::icon(class = "dropdown"),
              "Opal Database Monitor"),

          div(class = "active content",

              div(style = "height: 11em;
                           overflow: auto;
                           margin: 0;",

                  ModServerOpalDBMonitor_UI("ServerOpalDBMonitor")))),


      #-----------------------------------------------------------------------
      div(class = "ui divider",
          style = "margin: 1.5em 0;"),
      #-----------------------------------------------------------------------


      div(class = "ui accordion",      # Note: For this to work an extra JS script is necessary (see MainUIComponent())

          div(class = "active title AccordionHeader",
              shiny.semantic::icon(class = "dropdown"),
              "Server R Session Workspace"),

          div(class = "active content",

              div(style = "height: 40em;",
                           # overflow: auto;
                           # margin: 0;",

                  ModServerExplorer_UI("Prepare-ServerExplorer")))),


      #-----------------------------------------------------------------------
      div(class = "ui divider",
          style = "margin: 1.5em 0;"),
      #-----------------------------------------------------------------------


      div(class = "ui accordion",      # Note: For this to work an extra JS script is necessary (see MainUIComponent())

          div(class = "active title AccordionHeader",
              shiny.semantic::icon(class = "dropdown"),
              "Data Set Checks"),

          div(class = "active content",

              div(style = "height: 30em;
                           overflow: auto;
                           margin: 0;",

                  shiny.semantic::tabset(tabs = list(list(menu = "Raw Data Set (RDS)",
                                                          content = ModDataSetMonitor_UI("RDSMonitor")),
                                                     list(menu = "Curated Data Set (CDS)",
                                                          content = ModDataSetMonitor_UI("CDSMonitor")),
                                                     list(menu = "Augmented Data Set (ADS)",
                                                          content = ModDataSetMonitor_UI("ADSMonitor"))))))),


      #-----------------------------------------------------------------------
      div(class = "ui divider",
          style = "margin: 1.5em 0;"),
      #-----------------------------------------------------------------------


      div(class = "ui accordion",      # Note: For this to work an extra JS script is necessary (see MainUIComponent())

          div(class = "active title AccordionHeader",
              shiny.semantic::icon(class = "dropdown"),
              "Curation Report"),

          div(class = "active content",

              div(style = "height: 20em;
                           overflow: auto;
                           margin: 0;",

                  ModCurationReport_UI("CurationReport")))),


      #-----------------------------------------------------------------------
      div(class = "ui divider",
          style = "margin: 1.5em 0;"),
      #-----------------------------------------------------------------------


      div(class = "ui accordion",

          div(class = "active title AccordionHeader",
              shiny.semantic::icon(class = "dropdown"),
              "Data Transformation Monitor"),

          div(class = "active content",

              div(style = "height: 30em;
                           overflow: auto;",

                  ModDataTransformationMonitor_UI("DataTransformationMonitor")))))
}
