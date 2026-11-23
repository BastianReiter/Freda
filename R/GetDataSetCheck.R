
#' GetDataSetCheck
#'
#' Checks out a data set (\code{list} of \code{data.frames}) and returns an informative list object. Options:
#' \enumerate{
#'    \item The data set can be inspected naively without passing any additional information about it
#'    \item Meta data like required table and feature names as well as eligible value sets can be passed explicitly
#'    \item A Freda Module containing data model meta data can be passed }
#'
#' @param DataSet \code{list} of \code{data.frames} - Data Set object
#' @param FREDAModule Optional \code{list} - A FREDA module containing meta data and processing settings
#' @param RequiredTableNames Optional \code{character vector} - Names of required tables
#' @param RequiredFeatureNames Optional \code{list} of \code{character vectors} - Names of required features - Default: \code{names(Table)}
#' @param EligibleValueSets Optional \code{list} of \code{character vectors} containing sets of eligible values for corresponding feature.
#' @param Stage Optional \code{string} - Indicating transformation stage of addressed data set. This is relevant for which names and values to look up in module meta data. Options: 'Source' / 'Refined' / 'Enriched'
#'
#' @return A \code{list} containing meta data about tables in a data set
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetDataSetCheck <- function(DataSet,
                            FREDAModule = NULL,
                            RequiredTableNames = NULL,
                            RequiredFeatureNames = NULL,
                            EligibleValueSets = NULL,
                            Stage = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSet <- CCP.SourceDataSet
  # DataSet <- readRDS(file = "../dsFredaP21/Development/Data/RealData/SourceDataSet.rds")
  # DataSet <- EDS
  # FREDAModule <- FREDAModule.CCP
  # RequiredTableNames <- NULL
  # RequiredFeatureNames <- NULL
  # EligibleValueSets <- NULL
  # Stage <- "Source"

  # --- Argument Validation ---
  assert_that(is.list(DataSet))
  if (!is.null(FREDAModule)) { assert_that(is.list(FREDAModule),
                                           is.string(Stage), Stage %in% c("Source", "Refined", "Enriched")) }
  if (!is.null(RequiredTableNames)) { assert_that(is.character(RequiredTableNames)) }
  if (!is.null(RequiredFeatureNames)) { assert_that(is.list(RequiredFeatureNames)) }
  if (!is.null(EligibleValueSets)) { assert_that(is.list(EligibleValueSets)) }

#-------------------------------------------------------------------------------

  # Initiate defaults (if no Freda module or other meta data is passed)
  if (is.null(RequiredTableNames)) { RequiredTableNames <- names(DataSet) }
  if (is.null(RequiredFeatureNames)) { RequiredFeatureNames <- list() }
  if (is.null(EligibleValueSets)) { EligibleValueSets <- list() }


  # Get meta data from package-owned objects in case the data set to be checked belongs to a registered Freda Module (like 'CCP' or 'P21')
  if (!is.null(FREDAModule))
  {
      # Reassign for easier handling
      MetaData <- FREDAModule$MetaData

      if (Stage %in% c("Source", "Refined"))
      {
          # If module meta data is available, get required table and feature names as well as EligibleValueSets from it
          if (length(MetaData$Tables) > 0 & length(MetaData$Features) > 0 & length(MetaData$Values) > 0)
          {
              TableNameColumn <- "TableName.Refined"      # Once tables are loaded into R session they should already have 'Refined' table names ('TableName.Source' refers to table names in Opal DB)
              FeatureNameColumn <- "FeatureName.Refined"      # Per current default in module-specific functions, feature names are directly recoded into 'Refined' form upon SDS loading

              # Defining relevant column names depending on transformation stage ('Source' or 'Refined')
              # FeatureNameColumn <- paste0("FeatureName.", Stage)
              ValueColumn <- paste0("Value.", Stage)

              # Get required table names as character vector
              RequiredTableNames <- MetaData$Tables %>%
                                          pull({{ TableNameColumn }})

              # Get required feature names as list of character vectors (one for each table)
              RequiredFeatureNames <- RequiredTableNames %>%
                                            map(\(tablename) MetaData$Features %>%
                                                                  filter(TableName.Refined == tablename) %>%
                                                                  pull({{ FeatureNameColumn }})) %>%
                                            set_names(nm = RequiredTableNames)

              # Get sets of eligible value sets as a list of lists of character vectors
              EligibleValueSets <- RequiredTableNames %>%
                                        map(function(tablename)
                                            {
                                                RelevantFeatures <- MetaData$Values %>%
                                                                        filter(Table == tablename) %>%
                                                                        pull({{ FeatureNameColumn }}) %>%
                                                                        unique()

                                                ValueSet <- RelevantFeatures %>%
                                                                map(\(featurename) MetaData$Values %>%
                                                                                        filter(Table == tablename,
                                                                                               .data[[FeatureNameColumn]] == featurename) %>%
                                                                                        pull({{ ValueColumn }})) %>%
                                                                set_names(nm = RelevantFeatures)

                                                return(ValueSet)
                                            }) %>%
                                        set_names(nm = RequiredTableNames)
          }
      }

      if (Stage == "Enriched")
      {
          # If module meta data for Enriched Data Set (EDS) is available, get required table and feature names as well as EligibleValueSets from it
          if (length(FREDAModule$EDSMetaData) > 0)
          {
              # Get required table names as character vector
              RequiredTableNames <- FREDAModule$EDSMetaData %>%
                                          pull(TableName) %>%
                                          unique()

              # Get required feature names as list of character vectors (one for each table)
              RequiredFeatureNames <- RequiredTableNames %>%
                                            map(\(tablename) FREDAModule$EDSMetaData %>%
                                                                  filter(TableName == tablename) %>%
                                                                  pull(FeatureName) %>%
                                                                  unique()) %>%
                                            set_names(nm = RequiredTableNames)

              # Get sets of eligible value sets as a list of lists of character vectors
              EligibleValueSets <- RequiredTableNames %>%
                                        map(function(tablename)
                                            {
                                                RelevantFeatures <- FREDAModule$EDSMetaData %>%
                                                                        filter(TableName == tablename,
                                                                               HasEligibleValueSet == TRUE) %>%
                                                                        pull(FeatureName) %>%
                                                                        unique()

                                                ValueSet <- RelevantFeatures %>%
                                                                map(\(featurename) FREDAModule$EDSMetaData %>%
                                                                                        filter(TableName == tablename,
                                                                                               FeatureName == featurename) %>%
                                                                                        pull(Value)) %>%
                                                                set_names(nm = RelevantFeatures)

                                                return(ValueSet)
                                            }) %>%
                                        set_names(nm = RequiredTableNames)
          }
      }
  }


  # Create Table check templates for all required Data Set tables ("assume as empty/missing")
  DataSetCheckTemplate <- RequiredTableNames %>%
                              map(function(tablename)
                                  {
                                      CheckTable(Table = NULL,
                                                 RequiredFeatureNames = RequiredFeatureNames[[tablename]])

                                  }) %>%
                              set_names(RequiredTableNames)


  # Go through actually existing tables in 'DataSet' and check for feature completeness as well as feature types, row counts and rates of non-missing values
  DataSetCheckExisting <- DataSet %>%
                              imap(function(Table, tablename)
                                   {
                                      if (length(Table) > 0 && nrow(Table) > 0)
                                      {
                                          CheckTable(Table = Table,
                                                     RequiredFeatureNames = RequiredFeatureNames[[tablename]],
                                                     EligibleValueSets = EligibleValueSets[[tablename]])
                                      }
                                      else { return(NULL) }
                                   }) %>%
                              compact()      # This removes all NULL elements from list


  # Replace info in TableCheckTemplate with info about existing tables to get coherent summary
  DataSetCheck <- DataSetCheckTemplate %>%
                      imap(function(Table, tablename)
                           {
                              if (tablename %in% names(DataSetCheckExisting)) { return(DataSetCheckExisting[[tablename]]) }
                              else { return(Table) }
                           })

#-------------------------------------------------------------------------------
  return(DataSetCheck)
}
