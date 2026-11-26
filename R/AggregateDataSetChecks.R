
#' AggregateDataSetChecks
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Process single or multiple site-specific DataSetCheck objects returned by \code{Freda::GetDataSetCheck}, creating aggregated reporting objects.
#'
#' @param DataSetChecks \code{list} of site-specific DataSetCheck objects returned by \code{Freda::GetDataSetCheck()}.
#'
#' @return A \code{list} containing compiled meta data about data set tables:
#'         \itemize{\item TableStatus
#'                  \item TableRecordCounts
#'                  \item FeatureExistence
#'                  \item FeatureTypes
#'                  \item NonMissingValueCounts
#'                  \item NonMissingValueRates
#'                  \item EligibleValueCounts
#'                  \item EligibleValueRates}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AggregateDataSetChecks <- function(DataSetChecks)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSetChecks <- list(Test = SDSTableCheck)

  # --- Argument Validation ---
  assert_that(is.list(DataSetChecks))

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Transform into aggregated report objects
#-------------------------------------------------------------------------------

  # Create data frame containing "traffic light" info about existence/completeness of data set tables
  TableStatus <- DataSetChecks %>%
                      map(function(SiteDataSetCheck)
                          {
                              SiteTableStatus <- SiteDataSetCheck %>%
                                                        map_chr(function(TableInfo)
                                                                {
                                                                    Status <- "grey"

                                                                    if (TableInfo$TableExists == FALSE) { Status <- "red" }
                                                                    else if (TableInfo$TableExists == TRUE & TableInfo$TableComplete == TRUE) { Status <- "green" }
                                                                    else if (TableInfo$TableExists == TRUE & TableInfo$TableComplete == FALSE) { Status <- "yellow" }

                                                                    CountExistingFeatures <- sum(TableInfo$FeatureCheckOverview$Exists)
                                                                    CountTotalFeatures <- nrow(TableInfo$FeatureCheckOverview)

                                                                    if (Status != "grey") { Status <- paste0(Status, " (", CountExistingFeatures, "/", CountTotalFeatures, ")") }

                                                                }) %>%
                                                        rbind() %>%
                                                        as_tibble()
                          }) %>%
                      list_rbind(names_to = "SiteName") %>%
                      mutate(CheckSummary = case_when(if_all(-SiteName, ~ str_starts(.x, "green")) ~ "green",
                                                      if_any(-SiteName, ~ str_starts(.x, "red")) ~ "red",
                                                      if_any(-SiteName, ~ str_starts(.x, "yellow")) ~ "yellow",
                                                      TRUE ~ "grey"))


  # Create list of data frames (one per data set table) containing table record counts at different Sites
  TableRecordCounts <- DataSetChecks %>%
                            list_transpose() %>%
                            map(function(TableInfo)
                                {
                                    TableInfo %>%
                                         map(\(SiteTableInfo) tibble(RecordCount = SiteTableInfo$RecordCount)) %>%
                                         list_rbind(names_to = "SiteName")
                                })


  # Create list of data frames (one per data set table) containing info about existence of table features
  FeatureExistence <- DataSetChecks %>%
                          list_transpose() %>%
                          map(function(TableInfo)
                              {
                                  TableInfo %>%
                                      map(\(SiteTableInfo) SiteTableInfo$FeatureCheckOverview %>% select(Feature, Exists)) %>%
                                      list_rbind(names_to = "SiteName") %>%
                                      pivot_wider(names_from = Feature,
                                                  values_from = Exists)
                              })


  # Create list of data frames (one per data set table) containing table's feature types
  FeatureTypes <- DataSetChecks %>%
                      list_transpose() %>%
                      map(function(TableInfo)
                          {
                              TableInfo %>%
                                  map(\(SiteTableInfo) SiteTableInfo$FeatureCheckOverview %>% select(Feature, Type)) %>%
                                  list_rbind(names_to = "SiteName") %>%
                                  pivot_wider(names_from = Feature,
                                              values_from = Type)
                          })


  # Create list of data.frames (one per data set table) containing feature-specific non-missing value counts
  NonMissingValueCounts <- DataSetChecks %>%
                              list_transpose() %>%
                              map(function(TableInfo)
                                  {
                                      TableInfo %>%
                                          map(\(SiteTableInfo) SiteTableInfo$FeatureCheckOverview %>% select(Feature, NonMissingValueCount)) %>%
                                          list_rbind(names_to = "SiteName") %>%
                                          pivot_wider(names_from = Feature,
                                                      values_from = NonMissingValueCount)
                                  })


  # Create list of data.frames (one per data set table) containing feature-specific non-missing value rates
  NonMissingValueRates <- NonMissingValueCounts %>%
                              imap(function(TableInfo, tablename)
                                   {
                                      TableInfo %>%
                                          left_join(TableRecordCounts[[tablename]], by = join_by(SiteName)) %>%      # Get record counts from 'TableCheckOverview'
                                          mutate(across(-c(SiteName, RecordCount), ~ .x / RecordCount)) %>%
                                          select(-RecordCount)
                                   })


  # Create list of data frames (one per data set table) containing feature-specific eligible value counts
  EligibleValueCounts <- DataSetChecks %>%
                              list_transpose() %>%
                              map(function(TableInfo)
                                  {
                                      TableInfo %>%
                                          map(\(SiteTableInfo) SiteTableInfo$FeatureCheckOverview %>% select(Feature, EligibleValueCount)) %>%
                                          list_rbind(names_to = "SiteName") %>%
                                          pivot_wider(names_from = Feature,
                                                      values_from = EligibleValueCount)
                                  })


  # Create list of data frames (one per data set table) containing feature-specific eligible value rates
  EligibleValueRates <- EligibleValueCounts %>%
                            imap(function(TableInfo, tablename)
                                 {
                                    TableInfo %>%
                                        left_join(TableRecordCounts[[tablename]], by = join_by(SiteName)) %>%      # Get record counts from 'TableCheckOverview'
                                        mutate(across(-c(SiteName, RecordCount), ~ .x / RecordCount)) %>%
                                        select(-RecordCount)
                                 })


#-------------------------------------------------------------------------------
  return(list(TableStatus = TableStatus,
              TableRecordCounts = TableRecordCounts,
              FeatureExistence = FeatureExistence,
              FeatureTypes = FeatureTypes,
              NonMissingValueCounts = NonMissingValueCounts,
              NonMissingValueRates = NonMissingValueRates,
              EligibleValueCounts = EligibleValueCounts,
              EligibleValueRates = EligibleValueRates))
}
