
#' GetFrequencyTable
#'
#' Return table of absolute and relative value frequencies for a nominal / ordinal feature.
#'
#' @param Table \code{data.frame} that contains the feature
#' @param FeatureName \code{string} - Name of feature
#' @param GroupingFeatureName \code{string} - Name of optional grouping feature
#' @param RemoveNA \code{logical} - Indicating whether missing values should be removed prior to frequency calculation - Default: \code{FALSE}
#' @param PrivacySettings \code{list}
#'
#' @return A \code{tibble} containing absolute and relative frequencies
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetFrequencyTable <- function(Table,
                              FeatureName,
                              GroupingFeatureName = NULL,
                              RemoveNA = FALSE,
                              PrivacySettings = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- ADS$Diagnosis
  # FeatureName <- "DiagnosisID"
  # GroupingFeatureName <- "Grading"

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.string(FeatureName),
              is.flag(RemoveNA))
  if (!is.null(GroupingFeatureName)) { assert_that(is.string(GroupingFeatureName))
                                       assert_that(GroupingFeatureName != FeatureName,
                                                   msg = "Values for 'GroupingFeatureName' and 'FeatureName' can not be identical.") }
  if (!is.null(PrivacySettings)) { assert_that(is.list(PrivacySettings)) }

#-------------------------------------------------------------------------------

  # Stop if 'FeatureName' is not part of 'Table' or if does not have a suitable class
  assert_that(FeatureName %in% names(Table),
              msg = paste0("'", FeatureName, "' is not a valid feature name in the given table."))
  assert_that(class(Table[[FeatureName]]) %in% c("character", "logical", "factor"),
              msg = paste0("The specified feature '", FeatureName, "' is of class '", class(Table[[FeatureName]]), "' and therefore not suitable."))

  # Optionally perform corresponding validation for 'GroupingFeatureName'
  if (!is.null(GroupingFeatureName))
  {
      assert_that(GroupingFeatureName %in% names(Table),
                  msg = paste0("'", GroupingFeatureName, "' is not a valid feature name in the given table."))
      assert_that(class(Table[[GroupingFeatureName]]) %in% c("character", "logical", "factor"),
                  msg = paste0("The specified grouping feature '", GroupingFeatureName, "' is of class '", class(Table[[GroupingFeatureName]]), "' and therefore not suitable."))
  }

#-------------------------------------------------------------------------------

  # If no NThreshold is given in settings, set NThreshold to -1, which effectively prevents subsequent masking
  if (is.na(PrivacySettings$NThreshold)) { PrivacySettings$NThreshold <- -1 }

  # Check proportion of number of unique feature values to number of table rows
  if (!is.na(PrivacySettings$GetFrequencyTable.Threshold.ProportionUniqueValuesToRows))
  {
      assert_that(length(unique(Table[[FeatureName]])) / nrow(Table) < PrivacySettings$GetFrequencyTable.Threshold.ProportionUniqueValuesToRows,
                  msg = "Abort due to disclosure risk: Too many unique feature values in proportion to table rows!")
  }

#-------------------------------------------------------------------------------

  # Grouping of 'Table' depending on passing of additional grouping variable
  if (!is.null(GroupingFeatureName))
  {
      FrequencyTable <- Table %>%
                            select({{ GroupingFeatureName}},
                                   {{ FeatureName }}) %>%
                            group_by(!!sym(GroupingFeatureName),
                                     !!sym(FeatureName))
  } else {

      FrequencyTable <- Table %>%
                            select({{ FeatureName }}) %>%
                            group_by(!!sym(FeatureName))
  }

  # Tibble containing absolute and relative frequencies
  FrequencyTable <- FrequencyTable %>%
                            summarize(AbsoluteFrequency = n()) %>%
                            mutate(RelativeFrequency = AbsoluteFrequency / sum(AbsoluteFrequency)) %>%
                            arrange(desc(AbsoluteFrequency), .by_group = TRUE) %>%
                        ungroup() %>%
                        mutate(NThreshold = PrivacySettings$NThreshold,
                               IsMasked = case_when(AbsoluteFrequency <= PrivacySettings$NThreshold ~ TRUE,
                                                    .default = FALSE),
                               AbsoluteFrequency = case_when(IsMasked == TRUE ~ NA,
                                                             .default = AbsoluteFrequency),
                               RelativeFrequency = case_when(IsMasked == TRUE ~ NA,
                                                             .default = RelativeFrequency))

#-------------------------------------------------------------------------------
  return(as.data.frame(FrequencyTable))
}


