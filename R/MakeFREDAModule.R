
#' MakeFREDAModule
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#'
#' Create a FREDA Module - a \code{list} object containing meta data for a specific data model and settings on how to refine data with FREDA functionality.
#'
#' @param ModuleIdentifier \code{string} - Identifying a module for referencing purposes
#' @param ExcelFilePath \code{string} - Absolute file path to an excel file that contains meta data and settings data for a specific data model
#'
#' @return A \code{list} representing a FREDA module:
#'              \itemize{ \item MetaData \code{list}
#'                          \itemize{ \item Tables \code{data.frame}
#'                                    \item Features \code{data.frame}
#'                                    \item Values \code{data.frame}}
#'                        \item Settings \code{list}
#'                          \itemize{ \item RefinementProcess \code{data.frame}
#'                                    \item DataRemediation \code{list}
#'                                      \itemize{ \item Process \code{data.frame}
#'                                                \item TransformativeExpressions \code{data.frame}
#'                                                \item StringExtraction \code{data.frame}
#'                                                \item Dictionary \code{data.frame}
#'                                                \item FuzzyStringMatching \code{data.frame}}
#'                                    \item FeatureRequirements \code{data.frame}
#'                                    \item FeatureTracking \code{data.frame}
#'                                    \item PrimaryTableCleaning \code{data.frame}
#'                                    \item RecordSubsumption \code{data.frame}
#'                                    \item SecondaryTableCleaning \code{data.frame}
#'                                    \item TableNormalization \code{data.frame}}}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MakeFREDAModule <- function(ModuleIdentifier,
                            ExcelFilePath)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ExcelFilePath <- file.path("C:", "ARBEIT Lokal", "dsCCPhos", "Development", "Data", "ModuleData", "FREDAModule_CCP.xlsx")

  # --- Argument Validation ---
  assert_that(is.string(ModuleIdentifier),
              is.string(ExcelFilePath))


# Read in Package Data from xlsx-file
#-------------------------------------------------------------------------------

  Sheetnames <- c("Meta.Tables",
                  "Meta.Features",
                  "Meta.Values",
                  "Proc.TableNormalization",
                  "Set.RefinementProcess",
                  "Set.PrimaryTableCleaning",
                  "Set.SecondaryTableCleaning",
                  "Set.RecordSubsumption",
                  "Set.FeatureRequirements",
                  "Set.FeatureTracking",
                  "Set.DataRemediation",
                  "Set.TransformativeExpressions",
                  "Set.StringExtraction",
                  "Set.Dictionary",
                  "Set.FuzzyStringMatching")

  for (sheetname in Sheetnames)
  {
      Table <- readxl::read_excel(path = ExcelFilePath,
                                  sheet = sheetname,
                                  skip = 2,
                                  col_types = "text")

      ColumnTypes <- readxl::read_excel(path = ExcelFilePath,
                                        sheet = sheetname,
                                        range = readxl::cell_rows(2),
                                        col_names = colnames(Table)) %>%
                          tidyr::pivot_longer(everything(),
                                              names_to = "Column",
                                              values_to = "Type")

      if (nrow(ColumnTypes) > 0)
      {
          for (i in 1:nrow(ColumnTypes))
          {
              Table <- Table %>%
                          mutate(across(all_of(ColumnTypes$Column[i]),
                                        ~ Freda::FormatData(.x, ColumnTypes$Type[i])))
          }
      }

      assign(x = sheetname,
             value = Table)
  }

#-------------------------------------------------------------------------------
  return(list(ID = ModuleIdentifier,
              MetaData = list(Tables = Meta.Tables,
                              Features = Meta.Features,
                              Values = Meta.Values),
              Settings = list(RefinementProcess = Set.RefinementProcess,
                              DataRemediation = list(Process = Set.DataRemediation,
                                                     TransformativeExpressions = Set.TransformativeExpressions,
                                                     StringExtraction = Set.StringExtraction,
                                                     Dictionary = Set.Dictionary,
                                                     FuzzyStringMatching = Set.FuzzyStringMatching),
                              FeatureRequirements = Set.FeatureRequirements,
                              FeatureTracking = Set.FeatureTracking,
                              PrimaryTableCleaning = Set.PrimaryTableCleaning,
                              RecordSubsumption = Set.RecordSubsumption,
                              SecondaryTableCleaning = Set.SecondaryTableCleaning,
                              TableNormalization = Proc.TableNormalization)))
}
