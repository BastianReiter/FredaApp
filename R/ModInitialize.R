
# --- Module Initialize ---

# Has no UI component, only server

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
ModInitialize <- function(id,
                          ADSCheckData = NULL,
                          CCPTestData = NULL,
                          CDSCheckData = NULL,
                          CurationReport = NULL,
                          DSConnections = NULL,
                          ExplorationData = NULL,
                          RDSCheckData = NULL,
                          ServerSpecifications = NULL,
                          ServerWorkspaceInfo = NULL)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  if (!is.null(ADSCheckData)) { session$userData$ADSCheckData(ADSCheckData) }

                  if (!is.null(CCPTestData)) { session$userData$CCPTestData <- CCPTestData }

                  if (!is.null(CDSCheckData)) { session$userData$CDSCheckData(CDSCheckData) }

                  if (!is.null(CurationReport)) { session$userData$CurationReport(CurationReport) }

                  if (!is.null(DSConnections)) { session$userData$DSConnections(DSConnections) }

                  if (!is.null(ExplorationData)) { session$userData$ExplorationData <- ExplorationData }

                  if (!is.null(RDSCheckData)) { session$userData$RDSCheckData(RDSCheckData) }

                  if (!is.null(ServerSpecifications)) { session$userData$ServerSpecifications(ServerSpecifications)
                  } else if (!is.null(session$userData$ServerSpecifications)) { session$userData$ServerSpecifications(as.data.frame(dsCCPhosClient::ServerSpecifications)) }

                  if (!is.null(ServerWorkspaceInfo)) { session$userData$ServerWorkspaceInfo(ServerWorkspaceInfo) }
               })
}
