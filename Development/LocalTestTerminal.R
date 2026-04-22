
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#      CCPhos App Test Terminal
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")

# RDSTableCheck <- readRDS(file = "C:/Users/Basti/Desktop/CurationReports/CRC/CRC_RDSTableCheck_20250624.RDS")
# CurationReport <- readRDS(file = "C:/Users/Basti/Desktop/CurationReports/CRC/CRC_CurationReport_20250624.RDS")
# CDSTableCheck <- readRDS(file = "C:/Users/Basti/Desktop/CurationReports/CRC/CRC_CDSTableCheck_20250624.RDS")

# CurationReport <- readRDS(file = "C:/Users/Basti/Desktop/CurationReports/CurationReport_ALL_20250508.rds")
# CurationReport <- readRDS(file = "C:/Users/Basti/Desktop/CurationReports/CurationReport_Berlin2025-03-03.rds")

CurationReport <- readRDS(file = "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/CCP/CDSG Projekte/EvaSchaefer/CurationReport_20250731.rds")
#RDSTableCheck <- readRDS(file = "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/CCP/CDSG Projekte/EvaSchaefer/RDSTableCheck_20250731.rds")
CDSTableCheck <- readRDS(file = "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/CCP/CDSG Projekte/EvaSchaefer/CDSTableCheck_20250731.rds")
ADSTableCheck <- readRDS(file = "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/CCP/CDSG Projekte/EvaSchaefer/ADSTableCheck_20250731.rds")




StartCCPhosApp(CCPTestData = TestData,
               RunAutonomously = FALSE)
               # RDSTableCheckData = RDSTableCheck
               # ADSTableCheckData = ADSTableCheck,
               # CurationReportData = CurationReport)



ServerExplorer()



#TestData_Frankfurt <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")
#TestData_WithEmptyTables <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData_WithEmptyTables.rds")

#SiteSpecifications <- dsCCPhosClient::CCPSiteSpecifications




library(dsCCPhosClient)

TestData <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")

CCPConnections <- ConnectToVirtualCCP(CCPTestData = TestData,
                                      NumberOfSites = 3,
                                      NumberOfPatientsPerSite = 1000)

Messages_ServerRequirements <- CheckServerRequirements(DataSources = CCPConnections)

LoadRawDataSet(DataSources = CCPConnections)

WorkspaceInfo <- GetServerWorkspaceInfo(DataSources = CCPConnections)



