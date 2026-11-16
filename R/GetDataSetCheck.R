
#' GetDataSetCheck
#'
#' Checks out a data set (\code{list} of \code{data.frames}) and returns an informative list object. Options:
#' \enumerate{
#'    \item The data set can be inspected naively without passing any additional information about it
#'    \item Meta data like required table and feature names as well as eligible value sets can be passed explicitly
#'    \item A Module identifier from a list of registered modules can be passed ('CCP' / 'P21' / ...), which leads to meta data being taken from a linked package }
#'
#' @param DataSetName \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param RequiredTableNames Optional \code{character vector} - Names of required tables
#' @param RequiredFeatureNames Optional \code{list} of \code{character vectors} - Names of required features - Default: \code{names(Table)}
#' @param EligibleValueSets Optional \code{list} of \code{character vectors} containing sets of eligible values for corresponding feature.
#' @param Module Optional \code{string} identifying a defined data set and the corresponding meta data (Examples: 'CCP' / 'P21')
#' @param Stage Optional \code{string} - Indicating transformation stage of addressed data set. This is relevant for which names and values to look up in module meta data. Options: 'Raw' / 'Curated' / 'Augmented'
#'
#' @return A \code{list} containing meta data about tables in a data set
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetDataSetCheck <- function(DataSetName,
                            RequiredTableNames = NULL,
                            RequiredFeatureNames = NULL,
                            EligibleValueSets = NULL,
                            Module = NULL,
                            Stage = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSetName <- "P21.RawDataSet"
  # DataSet <- readRDS(file = "../dsFredaP21/Development/Data/RealData/RawDataSet.rds")
  # DataSet <- ADS
  # RequiredTableNames <- NULL
  # RequiredFeatureNames <- NULL
  # EligibleValueSets <- NULL
  # Module <- "P21"
  # Stage <- "Raw"

  # --- Argument Validation ---
  assert_that(is.string(DataSetName))
  if (!is.null(RequiredTableNames)) { assert_that(is.character(RequiredTableNames)) }
  if (!is.null(RequiredFeatureNames)) { assert_that(is.list(RequiredFeatureNames)) }
  if (!is.null(EligibleValueSets)) { assert_that(is.list(EligibleValueSets)) }
  if (!is.null(Module)) { assert_that(is.string(Module), Module %in% names(dsFreda::Meta.Modules),
                                        is.string(Stage), Stage %in% c("Raw", "Curated", "Augmented")) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  DataSet <- eval(parse(text = DataSetName), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Initiate defaults (if no meta data or module name is passed)
  if (is.null(RequiredTableNames)) { RequiredTableNames <- names(DataSet) }
  if (is.null(RequiredFeatureNames)) { RequiredFeatureNames <- list() }
  if (is.null(EligibleValueSets)) { EligibleValueSets <- list() }


  # Get meta data from package-owned objects in case the data set to be checked belongs to a registered Freda Module (like 'CCP' or 'P21')
  if (!is.null(Module) && (Module %in% names(dsFreda::Meta.Modules)))
  {
      # Get package name from 'Meta.Modules'
      ModulePackageName <- dsFreda::Meta.Modules[[Module]]

      if (Stage %in% c("Raw", "Curated"))
      {
          # Get package-owned module-specific meta data
          Meta.Tables.Module <- eval(parse(text = paste0(ModulePackageName, "::Meta.Tables")))
          Meta.Features.Module <- eval(parse(text = paste0(ModulePackageName, "::Meta.Features")))
          Meta.Values.Module <- eval(parse(text = paste0(ModulePackageName, "::Meta.Values")))

          # If module meta data is available, get required table and feature names as well as EligibleValueSets from it
          if (length(Meta.Tables.Module) > 0 & length(Meta.Features.Module) > 0 & length(Meta.Values.Module) > 0)
          {
              TableNameColumn <- "TableName.Curated"      # Once tables are loaded into R session they should already have 'curated' table names ('TableName.Raw' refers to table names in Opal DB)
              FeatureNameColumn <- "FeatureName.Curated"      # Per current default in module-specific functions, feature names are directly recoded into 'Curated' form upon RDS loading

              # Defining relevant column names depending on transformation stage ('Raw' or 'Curated')
              # FeatureNameColumn <- paste0("FeatureName.", Stage)
              ValueColumn <- paste0("Value.", Stage)

              # Get required table names as character vector
              RequiredTableNames <- Meta.Tables.Module %>%
                                          pull({{ TableNameColumn }})

              # Get required feature names as list of character vectors (one for each table)
              RequiredFeatureNames <- RequiredTableNames %>%
                                            map(\(tablename) Meta.Features.Module %>%
                                                                  filter(TableName.Curated == tablename) %>%
                                                                  pull({{ FeatureNameColumn }})) %>%
                                            set_names(nm = RequiredTableNames)

              # Get sets of eligible value sets as a list of lists of character vectors
              EligibleValueSets <- RequiredTableNames %>%
                                        map(function(tablename)
                                            {
                                                RelevantFeatures <- Meta.Values.Module %>%
                                                                        filter(Table == tablename) %>%
                                                                        pull({{ FeatureNameColumn }}) %>%
                                                                        unique()

                                                ValueSet <- RelevantFeatures %>%
                                                                map(\(featurename) Meta.Values.Module %>%
                                                                                        filter(Table == tablename,
                                                                                               .data[[FeatureNameColumn]] == featurename) %>%
                                                                                        pull({{ ValueColumn }})) %>%
                                                                set_names(nm = RelevantFeatures)

                                                return(ValueSet)
                                            }) %>%
                                        set_names(nm = RequiredTableNames)
          }
      }

      if (Stage == "Augmented")
      {
          # Get package-owned module-specific meta data
          Meta.ADS.Module <- eval(parse(text = paste0(ModulePackageName, "::Meta.ADS")))

          # If module meta data is available, get required table and feature names as well as EligibleValueSets from it
          if (length(Meta.ADS.Module) > 0)
          {
              # Get required table names as character vector
              RequiredTableNames <- Meta.ADS.Module %>%
                                          pull(TableName) %>%
                                          unique()

              # Get required feature names as list of character vectors (one for each table)
              RequiredFeatureNames <- RequiredTableNames %>%
                                            map(\(tablename) Meta.ADS.Module %>%
                                                                  filter(TableName == tablename) %>%
                                                                  pull(FeatureName) %>%
                                                                  unique()) %>%
                                            set_names(nm = RequiredTableNames)

              # Get sets of eligible value sets as a list of lists of character vectors
              EligibleValueSets <- RequiredTableNames %>%
                                        map(function(tablename)
                                            {
                                                RelevantFeatures <- Meta.ADS.Module %>%
                                                                        filter(TableName == tablename,
                                                                               HasEligibleValueSet == TRUE) %>%
                                                                        pull(FeatureName) %>%
                                                                        unique()

                                                ValueSet <- RelevantFeatures %>%
                                                                map(\(featurename) Meta.ADS.Module %>%
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
