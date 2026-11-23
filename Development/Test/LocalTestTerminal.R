

library(dplyr)
library(purrr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load CCP test data as source data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCP.SourceDataSet <- readRDS("../Data/CCP/CCPTestData2026.rds")

CCP.FREDAModule <- MakeFREDAModule(ModuleIdentifier = "CCP",
                                   ExcelFilePath = file.path("C:", "ARBEIT Lokal", "dsCCPhos", "Development", "Data", "ModuleData", "FREDAModule_CCP.xlsx"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform preparatory operations prior to refinement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SDSPreparation <- PrepareSourceData(SourceDataSet = SourceDataSet,
#                                     Module.S = "CCP",
#                                     Conversion.IntoCharacter.S = "All",
#                                     #Conversion.DateIntoPOSIXct.S = .encode_tidy_eval("list('.All' = c('%Y%m%d%H%M', '%Y%m%d', '%Y-%m-%d'))", .get_encode_dictionary()),
#                                     RecodeFeatureNames.S = TRUE)

# RawDataSet <- SDSPreparation$SourceDataSet



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SDSTableCheck <- GetDataSetCheck(DataSet = CCP.SourceDataSet,
                                 FREDAModule = CCP.FREDAModule,
                                 Stage = "Source")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Refine data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RefinementOutput <- RefineData(SourceDataSet = SourceDataSet,
                               FREDAModule = CCP.FREDAModule)

CCP.RefinedDataSet <- RefinementOutput$DataSet


FredaGUI::Widget.RefinementReport(RefinementReport = RefinementOutput$Report)

RDSTableCheck <- GetDataSetCheck(DataSetName = CCP.RefinedDataSet,
                                 FREDAModule = CCP.FREDAModule,
                                 Stage = "Refined")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Enrich data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

EnrichmentOutput <- dsCCPhos::EnrichDataDS(RefinedDataSetName.S = "CCP.RefinedDataSet")

EDS <- EnrichmentOutput$EnrichedDataSet

EDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "EDS")


CCP.EDS.Diagnosis <- EDS$Diagnosis


EDS.Patient <- EDS$Patient %>%
                    filter(CountDiagnoses == 1)


Analysis <- JoinTablesDS(TableNameA.S = "EDS$Patient",
                         TableNameB.S = "EDS$Diagnosis",
                         ByStatement.S = "PatientID")

Test <- GetSampleStatisticsDS(TableName.S = "Analysis",
                              FeatureName.S = "PatientAgeAtDiagnosis",
                              GroupingFeatureName.S = "UICCStageCategory",
                              ReturnECDF.S = TRUE)

Test <- GetFrequencyTableDS(TableName.S = "EDS$Diagnosis",
                            FeatureName.S = "DiagnosisID")

#
#
# SampleStatistics <- GetSampleStatisticsDS(TableName.S = "EDS$Patients",
#                                           FeatureName.S = "LastVitalStatus")
# SampleStatistics$Statistics



GetFrequencyTableDS(TableName.S = "CCP.EDS.Diagnosis",
                    FeatureName.S = "ICD10Code")




install.packages("pkgnet")
library(pkgnet)

report <- CreatePackageReport("Freda")


