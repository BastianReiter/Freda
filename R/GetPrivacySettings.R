
#' MakePrivacySettings
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#'
#' Create a \code{list} of parameters used as settings for privacy and disclosure control in FREDA functionality and output.
#'
#' @param Profile Optional \code{string} - Choosing predefined settings with one of the following keywords: 'Development' / 'Loose' / 'Strict'. The other settings can still be modified.
#' @param NThreshold \code{integer}
#' @param FilterTable.MinN \code{integer}
#' @param GetFrequencyTable.Threshold.ProportionUniqueValuesToRows \code{double}
#' @param GetTestData.Enable \code{logical}
#' @param GetTestData.Mode \code{string}
#' @param GetTestData.MaxSampleSize \code{integer}
#' @param GetReportingObject.AllowedObjectNames \code{character}
#'
#' @return A \code{list} containing privacy settings
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MakePrivacySettings <- function(Profile = NA,
                                NThreshold = NA,
                                FilterTable.MinN = NA,
                                GetFrequencyTable.Threshold.ProportionUniqueValuesToRows = NA,
                                GetTestData.Enable = NA,
                                GetTestData.Mode = NA,
                                GetTestData.MaxSampleSize = NA,
                                GetReportingObject.AllowedObjectNames = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  if (!is.na(Profile)) { assert_that(is.string(Profile)) }
  if (!is.na(NThreshold)) { assert_that(is.count(NThreshold)) }
  if (!is.na(FilterTable.MinN)) { assert_that(is.count(FilterTable.MinN)) }
  if (!is.na(GetTestData.Enable)) { assert_that(is.flag(GetTestData.Enable)) }
  if (!is.na(GetTestData.Mode)) { assert_that(is.string(GetTestData.Mode)) }
  if (!is.na(GetTestData.MaxSampleSize)) { assert_that(is.count(GetTestData.MaxSampleSize)) }
  if (!is.null(GetReportingObject.AllowedObjectNames)) { assert_that(is.character(GetReportingObject.AllowedObjectNames)) }

#-------------------------------------------------------------------------------

  # Initiate settings object with generic defaults
  PrivacySettings <- list(NThreshold = NA,
                          FilterTable.MinN = NA,
                          GetFrequencyTable.Threshold.ProportionUniqueValuesToRows = NA,
                          GetTestData.Enable = NA,
                          GetTestData.Mode = NA,
                          GetTestData.MaxSampleSize = NA,
                          GetReportingObject.AllowedObjectNames = NULL)

#-------------------------------------------------------------------------------
# Define different privacy profiles
#-------------------------------------------------------------------------------

  if (Profile == "Development")
  {
      PrivacySettings$NThreshold = NA
      PrivacySettings$FilterTable.MinN = NA
      PrivacySettings$GetFrequencyTable.Threshold.ProportionUniqueValuesToRows = NA
      PrivacySettings$GetTestData.Enable = TRUE
      PrivacySettings$GetTestData.Mode = "ColumnsConnected"
      PrivacySettings$GetTestData.MaxSampleSize = NA
      PrivacySettings$GetReportingObject.AllowedObjectNames = NULL
  }

  if (Profile == "Loose")
  {
      PrivacySettings$NThreshold = 10
      PrivacySettings$FilterTable.MinN = 5
      PrivacySettings$GetFrequencyTable.Threshold.ProportionUniqueValuesToRows = 0.05
      PrivacySettings$GetTestData.Enable = TRUE
      PrivacySettings$GetTestData.Mode = "ColumnsDisconnected"
      PrivacySettings$GetTestData.MaxSampleSize = 10
      PrivacySettings$GetReportingObject.AllowedObjectNames = NULL
  }

  if (Profile == "Strict")
  {
      PrivacySettings$NThreshold = 5
      PrivacySettings$FilterTable.MinN = 5
      PrivacySettings$GetFrequencyTable.Threshold.ProportionUniqueValuesToRows = 0.05
      PrivacySettings$GetTestData.Enable = FALSE
      PrivacySettings$GetTestData.Mode = NA
      PrivacySettings$GetTestData.MaxSampleSize = NA
      PrivacySettings$GetReportingObject.AllowedObjectNames = NULL
  }

#-------------------------------------------------------------------------------
# Final modification of settings if passed in specific arguments
#-------------------------------------------------------------------------------

  if (!is.na(NThreshold)) PrivacySettings$NThreshold <- NThreshold
  if (!is.na(FilterTable.MinN)) PrivacySettings$FilterTable.MinN <- FilterTable.MinN
  if (!is.na(GetFrequencyTable.Threshold.ProportionUniqueValuesToRows)) PrivacySettings$GetFrequencyTable.Threshold.ProportionUniqueValuesToRows <- GetFrequencyTable.Threshold.ProportionUniqueValuesToRows
  if (!is.na(GetTestData.Enable)) PrivacySettings$GetTestData.Enable <- GetTestData.Enable
  if (!is.na(GetTestData.Mode)) PrivacySettings$GetTestData.Mode <- GetTestData.Mode
  if (!is.na(GetTestData.MaxSampleSize)) PrivacySettings$GetTestData.MaxSampleSize <- GetTestData.MaxSampleSize
  if (!is.null(GetReportingObject.AllowedObjectNames)) PrivacySettings$GetReportingObject.AllowedObjectNames <- GetReportingObject.AllowedObjectNames


#-------------------------------------------------------------------------------
  return(PrivacySettings)
}
