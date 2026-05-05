
#===============================================================================
#   - Virtual DataSHIELD infrastructure for Demo purposes -
#===============================================================================


#===============================================================================
# Load required packages
#===============================================================================

library(dsBaseClient)
library(dsFredaClient)
library(dsCCPhosClient)

# Print DataSHIELD errors right away
options(datashield.errors.print = TRUE)


#===============================================================================
# Optional: Start FredaApp
#===============================================================================

TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")

StartFredaApp(TestData = TestData,
              RunAutonomously = FALSE)
              # RDSTableCheckData = RDSTableCheck,
              # CDSTableCheckData = CDSTableCheck,
              # CurationReport = CurationReport)


#===============================================================================
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#===============================================================================

CCPConnections <- ConnectToVirtualCCP(CCPTestData = readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds"),
                                      NumberOfServers = 3,
                                      NumberOfPatientsPerServer = 1000,
                                      AddedDsPackages = "dsTidyverse")

#===============================================================================
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#===============================================================================

CCP.LoadRawDataSet(ServerSpecifications = NULL)


#===============================================================================
# Check RDS tables for existence and completeness
#===============================================================================

RDSTableCheck <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
                                                   Module = "CCP",
                                                   Stage = "Raw")

# Widget.DataSetCheck(RDSCheckData = RDSTableCheck)


#===============================================================================
# Optionally: Draw random sample from Raw Data Set on servers
#===============================================================================

# ds.CCP.DrawSample(RawDataSetName = "CCP.RawDataSet",
#                   SampleSize = 1000,
#                   SampleName = "RDSSample")


#===============================================================================
# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
#===============================================================================

# Transform Raw Data Set (RDS) into Curated Data Set (CDS) (using default settings)
ds.CurateData(RawDataSetName = "CCP.RawDataSet",
              Module = "CCP",
              OutputName = "CCP.CurationOutput")

CDSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.CuratedDataSet",
                                    Module = "CCP",
                                    Stage = "Curated")

# Get curation reports
CurationReport <- ds.GetCurationReport(Module = "CCP")


Widget.CurationReport(Module = "CCP",
                      CurationReport = CurationReport)


#===============================================================================
# Transform Curated Data Set (CDS) into Augmented Data Set (ADS)
#===============================================================================

# Run ds.AugmentData
ds.CCP.AugmentData(CuratedDataSetName = "CCP.CuratedDataSet")


ADSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.AugmentedDataSet",
                                    Module = "CCP",
                                    Stage = "Augmented")


#===============================================================================
# Get overview of objects in server workspaces
#===============================================================================
# - Using dsCCPhosClient::GetServerWorkspaceInfo() and dsCCPhosClient::ds.GetObjectMetaData()
#-------------------------------------------------------------------------------

# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo()


Exploration <-dsFredaClient::GetExplorationData(OrderList = list(CCP.ADS.Diagnosis = c("ICD10Code",
                                                                                       "ICDOTopographyCode",
                                                                                       "LocalizationSide",
                                                                                       "ICDOMorphologyCode",
                                                                                       "Grading",
                                                                                       "UICCStage",
                                                                                       "UICCStageCategory",
                                                                                       "TNM.T",
                                                                                       "TNM.N",
                                                                                       "TNM.M",
                                                                                       "PatientAgeAtDiagnosis",
                                                                                       "TimeDiagnosisToDeath",
                                                                                       "TimeFollowUp"),
                                                                 CCP.ADS.Patient = c("Sex",
                                                                                     "LastVitalStatus",
                                                                                     "CausesOfDeath",
                                                                                     "CountDiagnoses")))


Widget.ServerExplorer(ServerWorkspaceInfo = ServerWorkspaceInfo,
                      ExplorationData = Exploration,
                      EnableLiveConnection = FALSE,
                      RunAutonomously = FALSE,
                      UseVirtualConnections = FALSE)


#===============================================================================
# First Cohort Exploration
#===============================================================================

# CohortDescription <- ds.CCP.GetCohortDescription(DataSetName = "CCP.AugmentedDataSet",
#                                                  Stage = "Augmented")
#
#
# # Transform data into display-friendly time series tables using auxiliary function 'DisplayTimeSeries()'
# PatientCount.TimeSeries <- DisplayTimeSeries(TimeSeriesData = CohortDescription$CohortSize.OverTime,
#                                              TimePointFeature = "DiagnosisYear",
#                                              ValueFeature = "PatientCount",
#                                              GroupingFeature = "Server",
#                                              IncludeMissingTimePoints = TRUE)
#
# View(PatientCount.TimeSeries)
#
# # Create a plot of diagnosis years using 'MakeColumnPlot()'
# Plot <- CohortDescription$CohortSize.OverTime %>%
#             filter(Server != "All") %>%
#             MakeColumnPlot(XFeature = DiagnosisYear,
#                            YFeature = PatientCount,
#                            GroupingFeature = Server)
#
# # Plot of gender...
# Plot <- CohortDescription$GenderDistribution %>%
#             filter(Server != "All") %>%
#             MakeColumnPlot(XFeature = Gender,
#                            YFeature = N,
#                            GroupingFeature = Server)
#
# # ... and age distributions
# Plot <- CohortDescription$AgeDistribution %>%
#             filter(Server != "All") %>%
#             MakeColumnPlot(XFeature = AgeGroup,
#                            YFeature = N,
#                            GroupingFeature = Server)

#===============================================================================
# Process ADS tables
#===============================================================================

ds.GetSampleStatistics(TableName = "CCP.ADS.Diagnosis",
                       FeatureName = "PatientAgeAtDiagnosis",
                       GroupingFeatureName = "TNM.M",
                       DSConnections = CCPConnections)


ds.GetFrequencyTable(TableName = "CCP.ADS.Diagnosis",
                     FeatureName = "TNM.T",
                     GroupingFeatureName = "TNM.M",
                     DSConnections = CCPConnections)


#===============================================================================
# Process ADS tables
#===============================================================================

# As of this date (07/25), table joining is not yet implemented in 'dsTidyverse' but you can use this built-in FREDA function:
Messages <- ds.JoinTables(TableNameA = "CCP.ADS.Patient",
                          TableNameB = "CCP.ADS.Diagnosis",
                          ByStatement = "PatientID",
                          OutputName = "PatDiag")

# Now, let's filter the resulting data.frame for patients with only one primary cancer diagnosis (so no multiple primary malignomes):
dsTidyverseClient::ds.filter(df.name = "PatDiag",
                             tidy_expr = list(CountDiagnoses == 1),
                             newobj = "PatDiag.Single")

# You can also apply multiple filter criteria:
dsTidyverseClient::ds.filter(df.name = "PatDiag",
                             tidy_expr = list(CountDiagnoses == 1,
                                              Sex == "Female"),
                             newobj = "PatDiag.Single.Female")



#===============================================================================
# Perform exemplary analyses
#===============================================================================

CrossTab <- ds.GetCrossTab(TableName = "CCP.ADS.Patient",
                           FeatureNames = c("Sex", "LastVitalStatus"),
                           RemoveNA = TRUE)

ds.MutateTable(TableName = "ADS_Diagnosis",
               MutateExpression = "UICCStageClassification = case_when(str_starts(TNM_T, '3') ~ 'III', .default = '<NA>')",
               OutputName = "TestUICC")



#-------------------------------------------------------------------------------

Test <- ds.GetTTEModel(TableName = "AnalysisDataSet",
                       TimeFeature = "TimeFollowUp",
                       EventFeature = "IsDocumentedDeceased",
                       ModelType = "survfit",
                       CovariateA = "UICCStageCategory",
                       #CovariateB = "UICCStageCategory",
                       MinFollowUpTime = 20)

library(ggplot2)

Plot <- ggplot2::ggplot(Test$All, ggplot2::aes(x = time, y = surv, color = .data[["strata"]])) +
            ggplot2::geom_step() +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "Time", y = "Survival probability", color = "strata") +
            ggplot2::theme(legend.title = ggplot2::element_text(size = 10), legend.text = ggplot2::element_text(size = 10))




#===============================================================================
# Log out from all servers
#===============================================================================

DSI::datashield.logout(CCPConnections)



