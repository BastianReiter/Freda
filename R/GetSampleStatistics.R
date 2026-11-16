
#' GetSampleStatistics
#'
#' Calculate common sample statistics for a feature of a data.frame.
#'
#' @param TableName \code{string} - Name of the table that contains the feature
#' @param FeatureName \code{string} - Name of feature
#' @param GroupingFeatureName \code{string} - Name of optional grouping feature
#' @param RemoveNA \code{logical} - Whether NA values should be removed before calculating statistics
#' @param ReturnECDF \code{logical} - Whether an empirical cumulative distribution function should be returned additionally to sample statistics
#'
#' @return A \code{tibble} containing parametric and non-parametric sample statistics
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetSampleStatistics <- function(TableName,
                                FeatureName,
                                GroupingFeatureName = NULL,
                                RemoveNA = TRUE,
                                ReturnECDF = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- ADS$Diagnosis
  # FeatureName <- "PatientAgeAtDiagnosis"
  # GroupingFeatureName <- "TNM.M"
  # RemoveNA <- TRUE
  # ReturnECDF <- TRUE

  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.string(FeatureName),
              is.flag(RemoveNA),
              is.flag(ReturnECDF))
  if (!is.null(GroupingFeatureName)) { assert_that(is.string(GroupingFeatureName))
                                         assert_that(GroupingFeatureName != FeatureName,
                                                     msg = "Values for 'GroupingFeatureName' and 'FeatureName' can not be identical.") }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName), envir = parent.frame())

  # Stop if 'FeatureName' is not part of 'Table' or if does not have a suitable class
  assert_that(FeatureName %in% names(Table),
              msg = paste0("'", FeatureName, "' is not a valid feature name in '", TableName, "'."))
  assert_that(class(Table[[FeatureName]]) %in% c("numeric", "integer", "double"),
              msg = paste0("The specified feature '", FeatureName, "' is of class '", class(Table[[FeatureName]]), "' and therefore not suitable."))

  # Perform corresponding validation for 'GroupingFeatureName'
  if (!is.null(GroupingFeatureName))
  {
      assert_that(GroupingFeatureName %in% names(Table),
                  msg = paste0("'", GroupingFeatureName, "' is not a valid feature name in '", TableName, "'."))
      assert_that(class(Table[[GroupingFeatureName]]) %in% c("character", "logical", "factor"),
                  msg = paste0("The specified grouping feature '", GroupingFeatureName, "' is of class '", class(Table[[GroupingFeatureName]]), "' and therefore not suitable."))
  }

#-------------------------------------------------------------------------------

  # Initiate output object
  Statistics <- NULL

  # Get count of non-missing values in Feature
  N.Valid <- sum(!is.na(Table[[FeatureName]]))

  if (N.Valid > 0)
  {
      # Calculate parametric and non-parametric sample statistics
      Statistics <- as_tibble(Table) %>%
                        { if (RemoveNA == TRUE) { filter(., !is.na({{ FeatureName }})) } else {.} } %>%
                        summarize(N = n(),
                                  q5 = stats::quantile(.data[[FeatureName]], probs = 0.05, na.rm = RemoveNA),
                                  Q1 = stats::quantile(.data[[FeatureName]], probs = 0.25, na.rm = RemoveNA),
                                  Median = stats::median(.data[[FeatureName]], na.rm = RemoveNA),
                                  Q3 = stats::quantile(.data[[FeatureName]], probs = 0.75, na.rm = RemoveNA),
                                  q95 = stats::quantile(.data[[FeatureName]], probs = 0.95, na.rm = RemoveNA),
                                  MAD = stats::mad(.data[[FeatureName]], na.rm = RemoveNA),
                                  Mean = mean(.data[[FeatureName]], na.rm = RemoveNA),
                                  SD = stats::sd(.data[[FeatureName]], na.rm = RemoveNA),
                                  SEM = SD / sqrt(N)) %>%
                        mutate(Mean = case_when(is.nan(Mean) ~ NA,
                                                .default = Mean)) %>%
                        ungroup()

      # If GroupingFeatureName is passed...
      if (!is.null(GroupingFeatureName))
      {
          # Get group-specific output
          Groupwise <- as_tibble(Table) %>%
                            { if (RemoveNA == TRUE) { filter(., !is.na({{ FeatureName }})) } else {.} } %>%
                            group_by(., !!sym(GroupingFeatureName)) %>%
                            summarize(N = n(),
                                      q5 = stats::quantile(.data[[FeatureName]], probs = 0.05, na.rm = RemoveNA),
                                      Q1 = stats::quantile(.data[[FeatureName]], probs = 0.25, na.rm = RemoveNA),
                                      Median = stats::median(.data[[FeatureName]], na.rm = RemoveNA),
                                      Q3 = stats::quantile(.data[[FeatureName]], probs = 0.75, na.rm = RemoveNA),
                                      q95 = stats::quantile(.data[[FeatureName]], probs = 0.95, na.rm = RemoveNA),
                                      MAD = stats::mad(.data[[FeatureName]], na.rm = RemoveNA),
                                      Mean = mean(.data[[FeatureName]], na.rm = RemoveNA),
                                      SD = stats::sd(.data[[FeatureName]], na.rm = RemoveNA),
                                      SEM = SD / sqrt(N)) %>%
                            mutate(Mean = case_when(is.nan(Mean) ~ NA,
                                                .default = Mean)) %>%
                            ungroup()

          # Create Extra column for later rbinding
          Statistics <- Statistics %>%
                            mutate(dummy = "AcrossGroups", .before = 1)

          # Harmonize column names for rbinding
          colnames(Statistics) <- colnames(Groupwise)

          Statistics <- rbind(Statistics,
                              Groupwise)
      }
  }

#-------------------------------------------------------------------------------

  ECDF <- NULL

  # if (ReturnECDF == TRUE)
  # {
  #       ECDF <- Table %>%
  #                 { if (RemoveNA == TRUE) { filter(., !is.na({{ FeatureName }})) } else {.} } %>%
  #                 { if (!is.null(GroupingFeatureName)) { group_by(., !!sym(GroupingFeatureName)) } else {.} } %>%
  #                 summarize(N = n(),
  #                           ECDF = list(ecdf(.data[[FeatureName]])))
  # }

#-------------------------------------------------------------------------------
  return(list(Statistics = Statistics,
              ECDF = ECDF))
}


