
#' AggregateRefinementReports
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Process single or multiple site-specific Refinement Report objects returned by \code{Freda::RefineData()}, creating aggregated reporting objects.
#'
#' @param RefinementReports \code{list} of site-specific RefinementReport objects returned by \code{Freda::RefineData()}.
#'
#' @return A \code{list} of the following structure:
#'         \itemize{  \item Log
#'                    \item Counter
#'                        \itemize{ \item DataSetLevel
#'                                  \item TableLevel
#'                                  \item Details }
#'                    \item DataHarmonization
#'                        \itemize{ \item Reports
#'                                      \itemize{ \item DataSetLevel
#'                                                \item TableLevel
#'                                                \item FeatureLevel
#'                                                \item ValueLevel
#'                                                \item ValueSets }
#'                                  \item Monitors
#'                                      \itemize{ \item TableLevel
#'                                                \item FeatureLevel
#'                                                \item ValueLevel }}}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AggregateRefinementReports <- function(RefinementReports)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # RefinementReports <- list(Test = RefinementOutput$Report)

  # --- Argument Validation ---
  assert_that(is.list(RefinementReports))

#-------------------------------------------------------------------------------

#===============================================================================
# - OVERVIEW -
#===============================================================================
#   A)  Rearrangement of LOG report objects
#   B)  Compilation of PROCESS report
#   C)  Compilation of COUNTER data
#   D)  Compilation of DATA HARMONIZATION Reports and Monitors
#-------------------------------------------------------------------------------


#===============================================================================
# A) Rearrangement of LOG report objects
#===============================================================================

  Log <- RefinementReports %>%
              list_transpose() %>%
              pluck("Log")


#===============================================================================
# B) Compilation of PROCESS report
#===============================================================================

  ProcessReport <- RefinementReports %>%
                        list_transpose() %>%
                        pluck("Process") %>%
                        map(\(X) as_tibble(X)) %>%
                        list_rbind(names_to = "Site")


#===============================================================================
# C) Compilation of COUNTER data
#===============================================================================
#   1) Cumulate stage-level Counter data as basis for summaries
#   2) Create Summary View from stage-level data and split into table-level and data-set-level summary
#   3) Extensive Counter data on sub-stage-level (details), no cumulation
#-------------------------------------------------------------------------------

# C 1) COUNTER SUMMARY on DATA SET level
#-------------------------------------------------------------------------------

  Counter.StageLevel.Sites <- RefinementReports %>%
                                    map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                                    pluck("Counter", "Extensive") %>%
                                                                    map(\(TableCounter) TableCounter %>% pluck("StageLevel")) %>%
                                                                    list_rbind(names_to = "Table")) %>%
                                    list_rbind(names_to = "Site")

  Counter.StageLevel.Cumulated <- Counter.StageLevel.Sites %>%
                                      group_by(ProcessingStage, Table) %>%
                                          summarize(Site = "All",
                                                    across(starts_with("Count") & !CountLevel & !contains("Proportion"),
                                                           ~ sum(.x, na.rm = TRUE))) %>%
                                          mutate(CountRecords.Change.Proportion = case_when(CountRecords.Prior > 0 ~ CountRecords.Change / CountRecords.Prior,
                                                                                            .default = NA_real_),
                                                 CountRootSubjects.Change.Proportion = case_when(CountRootSubjects.Prior > 0 ~ CountRootSubjects.Change / CountRootSubjects.Prior,
                                                                                                 .default = NA_real_),
                                                 CountSeedSubjects.Change.Proportion = case_when(CountSeedSubjects.Prior > 0 ~ CountSeedSubjects.Change / CountSeedSubjects.Prior,
                                                                                                 .default = NA_real_))

  Counter.StageLevel <- Counter.StageLevel.Cumulated %>%
                            bind_rows(Counter.StageLevel.Sites)


# C 2) Create COUNTER SUMMARY views from Stage-level data
#-------------------------------------------------------------------------------

  Counter.Summary <- Counter.StageLevel %>%
                          select(Site,
                                 ProcessingStage,
                                 Table,
                                 CountRecords.Change,
                                 CountRecords.Change.Proportion,
                                 CountRecords.Post,
                                 CountRootSubjects.Change,
                                 CountRootSubjects.Change.Proportion,
                                 CountRootSubjects.Post,
                                 CountSeedSubjects.Change,
                                 CountSeedSubjects.Change.Proportion,
                                 CountSeedSubjects.Post) %>%
                          rename(CountRecords = "CountRecords.Post",
                                 CountRootSubjects = CountRootSubjects.Post,
                                 CountSeedSubjects = CountSeedSubjects.Post) %>%
                          mutate(ProcessingStage = str_remove_all(ProcessingStage, " ")) %>%
                          pivot_wider(names_from = ProcessingStage,
                                      names_glue = "{ProcessingStage}.{.value}",
                                      values_from = !c(Site, ProcessingStage, Table)) %>%
                          select(Site,
                                 Table,
                                 Initial.CountRecords,
                                 Initial.CountRootSubjects,
                                 Initial.CountSeedSubjects,
                                 starts_with("PrimaryTableCleaning"),
                                 starts_with("TableNormalization"),
                                 starts_with("SecondaryTableCleaning"),
                                 starts_with("RecordSubsumption")) %>%
                          mutate(Final.CountRecords = RecordSubsumption.CountRecords,
                                 Final.CountRecords.Change = Final.CountRecords - Initial.CountRecords,
                                 Final.CountRecords.Change.Proportion = case_when(Initial.CountRecords > 0 ~ Final.CountRecords.Change / Initial.CountRecords,
                                                                                  .default = NA_real_),
                                 Final.CountRootSubjects = RecordSubsumption.CountRootSubjects,
                                 Final.CountRootSubjects.Change = Final.CountRootSubjects - Initial.CountRootSubjects,
                                 Final.CountRootSubjects.Change.Proportion = case_when(Initial.CountRootSubjects > 0 ~ Final.CountRootSubjects.Change / Initial.CountRootSubjects,
                                                                                       .default = NA_real_),
                                 Final.CountSeedSubjects = RecordSubsumption.CountSeedSubjects,
                                 Final.CountSeedSubjects.Change = Final.CountSeedSubjects - Initial.CountSeedSubjects,
                                 Final.CountSeedSubjects.Change.Proportion = case_when(Initial.CountSeedSubjects > 0 ~ Final.CountSeedSubjects.Change / Initial.CountSeedSubjects,
                                                                                       .default = NA_real_))


  # TABLE-LEVEL Counter summaries
  #------------------------------
  Counter.TableLevel <- Counter.Summary %>%
                            filter(Table != ".All") %>%
                            split(.$Table) %>%
                                map(\(X) X %>% select(-Table))


  # DATA-SET-LEVEL Counter summaries
  #---------------------------------

  # First, get Site-specific data set table counts from RefinementReports
  TableCount <- RefinementReports %>%
                    map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                          pluck("Counter", "Summary", "DataSetLevel") %>%
                                                          select(CountTables)) %>%
                    list_rbind(names_to = "Site") %>%
                    AddCumulativeRow()

  # Add table counts to Counter summary
  Counter.DataSetLevel <- Counter.Summary %>%
                              filter(Table == ".All") %>%
                              left_join(TableCount, by = join_by(Site)) %>%
                              select(-Table) %>%
                              relocate(CountTables, .before = 2)


# C 3) Extensive COUNTER data on sub-stage-level (details), no cumulation necessary
#-------------------------------------------------------------------------------

  # Compile relevant COUNTER DETAILS data in cumulated data.frame (all Sites, all tables) and then split into table-specific data.frames
  Counter.Details <- RefinementReports %>%
                          map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                          pluck("Counter", "Extensive") %>%
                                                          map(\(TableCounter) TableCounter %>% pluck("Details")) %>%
                                                          list_rbind(names_to = "Table")) %>%
                          list_rbind(names_to = "Site") %>%
                          select(Table,
                                 Site,
                                 Timestamp,
                                 ProcessingStage,
                                 ProcessTopic,
                                 ProcessTopic.Subgroup,
                                 MessageClass,
                                 Message,
                                 CountRecords.Removed,
                                 CountRecords.Added,
                                 CountRootSubjects.Affected,
                                 CountSeedSubjects.Affected) %>%
                          filter(CountRecords.Removed > 0 | CountRecords.Added > 0) %>%
                          arrange(Timestamp) %>%
                          split(.$Site) %>%
                              map(\(X) X %>%
                                        select(-Site) %>%
                                        split(.$Table) %>%
                                            map(\(X) X %>% select(-Table)))


#===============================================================================
# D) Compilation of DATA HARMONIZATION Reports and Monitors
#===============================================================================
#   1) Data Harmonization Reports on data-set-level
#   2) Data Harmonization Reports on table-level
#   3) Data Harmonization Reports on feature-level
#   4) Data Harmonization Reports on value-level
#   5) Data Harmonization Stage-specific value sets
#   6) Data Harmonization Monitors on table-level
#   7) Data Harmonization Monitors on feature-level
#   8) Data Harmonization Monitors on value-level
#
#-------------------------------------------------------------------------------

# D 1) Data Harmonization REPORTS on DATA-SET-LEVEL
#-------------------------------------------------------------------------------

  # Get all Site-specific data-set-level data harmonization reports and row-bind them
  DataHarmonization.Report.DataSetLevel.Sites <- RefinementReports %>%
                                                        map(\(X) X %>% pluck("DataHarmonization", "Reports", "DataSetLevel")) %>%
                                                        list_rbind(names_to = "Site")

  DataHarmonization.Report.DataSetLevel.Cumulated <- DataHarmonization.Report.DataSetLevel.Sites %>%
                                                          summarize(Site = "All",
                                                                    CountAffectedTables = max(CountAffectedTables),
                                                                    CountTrackedFeatures = max(CountTrackedFeatures),
                                                                    across(starts_with("CountValues"), ~ sum(.x, na.rm = FALSE)),
                                                                    ProportionValues.Harmonized = ifelse(is.na(CountValues.Ineligible.Source) | CountValues.Ineligible.Source == 0,
                                                                                                         NA,
                                                                                                         CountValues.Harmonized / CountValues.Ineligible.Source))

  DataHarmonization.Report.DataSetLevel <- DataHarmonization.Report.DataSetLevel.Cumulated %>%
                                                bind_rows(DataHarmonization.Report.DataSetLevel.Sites)


# D 2) Data Harmonization REPORTS on TABLE-LEVEL
#-------------------------------------------------------------------------------

  # Get all Site-specific table-level data harmonization reports and row-bind them
  DataHarmonization.Report.TableLevel.Sites <- RefinementReports %>%
                                                      map(\(X) X %>% pluck("DataHarmonization", "Reports", "TableLevel")) %>%
                                                      list_rbind(names_to = "Site")

  DataHarmonization.Report.TableLevel.Cumulated <- DataHarmonization.Report.TableLevel.Sites %>%
                                                        group_by(Table) %>%
                                                        summarize(Site = "All",
                                                                  CountTrackedFeatures = max(CountTrackedFeatures),
                                                                  across(starts_with("CountValues"), ~ sum(.x, na.rm = FALSE)),
                                                                  ProportionValues.Harmonized = ifelse(is.na(CountValues.Ineligible.Source) | CountValues.Ineligible.Source == 0,
                                                                                                       NA,
                                                                                                       CountValues.Harmonized / CountValues.Ineligible.Source))

  DataHarmonization.Report.TableLevel <- DataHarmonization.Report.TableLevel.Cumulated %>%
                                              bind_rows(DataHarmonization.Report.TableLevel.Sites) %>%
                                              split(.$Table) %>%
                                                  map(\(X) X %>% select(-Table))


# D 3) Data Harmonization REPORTS on FEATURE-LEVEL
#-------------------------------------------------------------------------------

  # Get all Site-specific feature-level data harmonization reports and row-bind them
  DataHarmonization.Report.FeatureLevel.Sites <- RefinementReports %>%
                                                        map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                                                        pluck("DataHarmonization", "Reports", "FeatureLevel") %>%
                                                                                        list_rbind(names_to = "Table")) %>%
                                                        list_rbind(names_to = "Site")

  DataHarmonization.Report.FeatureLevel.Cumulated <- DataHarmonization.Report.FeatureLevel.Sites %>%
                                                          group_by(Table, Feature) %>%
                                                              summarize(Site = "All",
                                                                        across(starts_with("Count"), ~ sum(.x, na.rm = TRUE))) %>%
                                                          ungroup() %>%
                                                          mutate(ProportionValues.Harmonized = case_when(is.na(CountValues.Ineligible.Source) | CountValues.Ineligible.Source == 0 ~ NA,
                                                                                                         .default = CountValues.Harmonized / CountValues.Ineligible.Source)) %>%
                                                          relocate(Site, .before = 1)

  DataHarmonization.Report.FeatureLevel <- DataHarmonization.Report.FeatureLevel.Cumulated %>%
                                                bind_rows(DataHarmonization.Report.FeatureLevel.Sites) %>%
                                                split(.$Table) %>%
                                                    map(\(X) X %>%
                                                              select(-Table) %>%
                                                              split(.$Feature) %>%
                                                                  map(\(X) X %>% select(-Feature)))


# D 4) Data Harmonization REPORTS on VALUE-LEVEL
#-------------------------------------------------------------------------------

  # Get all Site-specific value-level data harmonization reports and row-bind them
  DataHarmonization.Report.ValueLevel.Sites <- RefinementReports %>%
                                                      map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                                                      pluck("DataHarmonization", "Reports", "ValueLevel") %>%
                                                                                      list_rbind(names_to = "Table")) %>%
                                                      list_rbind(names_to = "Site")

  DataHarmonization.Report.ValueLevel.Cumulated <- DataHarmonization.Report.ValueLevel.Sites %>%
                                                        group_by(Table, Feature, Value.Source) %>%
                                                            arrange(desc(IsOccurring), .by_group = TRUE) %>%      # Make sure that a Site-specific row can be on top, where 'IsOccurring' is TRUE
                                                            summarize(Site = "All",
                                                                      Count = sum(Count, na.rm = TRUE),
                                                                      across(!Count, ~ .x[1])) %>%      # Take first row in group for all columns other than 'Count'
                                                                                                        # Note: This assumes that different harmonization results for different Sites are not possible. Possibly needs to be adjusted if non-deterministic data remediation methods are applied in the future.
                                                        ungroup() %>%
                                                        relocate(Site, .before = 1)

  DataHarmonization.Report.ValueLevel <- DataHarmonization.Report.ValueLevel.Cumulated %>%
                                              bind_rows(DataHarmonization.Report.ValueLevel.Sites) %>%
                                              split(.$Table) %>%
                                                    map(\(X) X %>%
                                                              select(-Table) %>%
                                                              split(.$Feature) %>%
                                                                  map(\(X) X %>%
                                                                            select(-Feature) %>%
                                                                            split(.$Value.Source) %>%
                                                                                map(\(X) X %>% select(-Value.Source))))


# D 5) Full Value Sets for all harmonization stages (Source, Remediated, Recoded, Final)
#-------------------------------------------------------------------------------

  DataHarmonization.ValueSets.Sites <- RefinementReports %>%
                                              map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                                              pluck("DataHarmonization", "Reports", "ValueSets") %>%
                                                                              map(\(TableValueSets) TableValueSets %>%
                                                                                                        map(\(FeatureValueSet) FeatureValueSet %>%
                                                                                                                                    list_rbind(names_to = "HarmonizationStage")) %>%
                                                                                                        list_rbind(names_to = "Feature")) %>%
                                                                              list_rbind(names_to = "Table")) %>%
                                              list_rbind(names_to = "Site")

  DataHarmonization.ValueSets.Cumulated <- DataHarmonization.ValueSets.Sites %>%
                                                group_by(Table, Feature, HarmonizationStage, Value) %>%
                                                    arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                    summarize(Site = "All",
                                                              across(c(IsOccurring, IsEligible), ~ .x[1]),
                                                              Count = sum(Count, na.rm = TRUE)) %>%
                                                group_by(Table, Feature, HarmonizationStage) %>%
                                                    mutate(Proportion = { if (sum(Count, na.rm = TRUE) == 0) { NA } else { Count / sum(Count, na.rm = TRUE) } }) %>%
                                                ungroup() %>%
                                                relocate(Site, .before = 1)

  DataHarmonization.ValueSets <- DataHarmonization.ValueSets.Cumulated %>%
                                      bind_rows(DataHarmonization.ValueSets.Sites) %>%
                                      split(.$Site) %>%
                                                    map(\(X) X %>%
                                                              select(-Site) %>%
                                                              split(.$Table) %>%
                                                                  map(\(X) X %>%
                                                                            select(-Table) %>%
                                                                            split(.$Feature) %>%
                                                                                map(\(X) X %>%
                                                                                          select(-Feature) %>%
                                                                                          split(.$HarmonizationStage) %>%
                                                                                              map(\(X) X %>% select(-HarmonizationStage)))))


# D 6) Data Harmonization MONITORS on TABLE-LEVEL
#-------------------------------------------------------------------------------

  # Get all Site-specific table-level data harmonization monitors and row-bind them
  DataHarmonization.Monitor.TableLevel.Sites <- RefinementReports %>%
                                                      map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                                                      pluck("DataHarmonization", "Monitors", "TableLevel") %>%
                                                                                      list_rbind(names_to = "Table")) %>%
                                                      list_rbind(names_to = "Site")

  DataHarmonization.Monitor.TableLevel.Cumulated <- DataHarmonization.Monitor.TableLevel.Sites %>%
                                                        group_by(Table, Eligibility) %>%
                                                            summarize(Site = "All",
                                                                      across(starts_with("Count"), ~ sum(.x))) %>%
                                                            mutate(across(starts_with("Count."), ~ { if (sum(.x) == 0) NA else .x / sum(.x) },
                                                                          .names = "Proportion.{.col}")) %>%      # Create proportional value columns
                                                            rename_with(~ str_replace(.x, ".Count.", "."), starts_with("Proportion.")) %>%
                                                        ungroup() %>%
                                                        relocate(Site, .before = 1)

  DataHarmonization.Monitor.TableLevel <- DataHarmonization.Monitor.TableLevel.Cumulated %>%
                                              bind_rows(DataHarmonization.Monitor.TableLevel.Sites) %>%
                                              split(.$Site) %>%
                                                  map(\(X) X %>%
                                                            select(-Site) %>%
                                                            split(.$Table) %>%
                                                                map(\(X) X %>% select(-Table)))


# D 7) Data Harmonization MONITORS on FEATURE-LEVEL
#-------------------------------------------------------------------------------

  # Get all Site-specific feature-level data harmonization monitors and row-bind them
  DataHarmonization.Monitor.FeatureLevel.Sites <- RefinementReports %>%
                                                        map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                                                        pluck("DataHarmonization", "Monitors", "FeatureLevel") %>%
                                                                                        list_rbind(names_to = "Table")) %>%
                                                        list_rbind(names_to = "Site")

  DataHarmonization.Monitor.FeatureLevel.Cumulated <- DataHarmonization.Monitor.FeatureLevel.Sites %>%
                                                          group_by(Table, Feature, Eligibility) %>%
                                                              summarize(Site = "All",
                                                                        across(starts_with("Count"), ~ sum(.x))) %>%
                                                              mutate(across(starts_with("Count."), ~ { if (sum(.x) == 0) NA else .x / sum(.x) },
                                                                            .names = "Proportion.{.col}")) %>%      # Create proportional value columns
                                                              rename_with(~ str_replace(.x, ".Count.", "."), starts_with("Proportion.")) %>%
                                                          ungroup() %>%
                                                          relocate(Site, .before = 1)

  DataHarmonization.Monitor.FeatureLevel <- DataHarmonization.Monitor.FeatureLevel.Cumulated %>%
                                                bind_rows(DataHarmonization.Monitor.FeatureLevel.Sites) %>%
                                                    split(.$Site) %>%
                                                    map(\(X) X %>%
                                                              select(-Site) %>%
                                                              split(.$Table) %>%
                                                                  map(\(X) X %>%
                                                                            select(-Table) %>%
                                                                            split(.$Feature) %>%
                                                                                map(\(X) X %>% select(-Feature))))


# D 8) Data Harmonization MONITORS on VALUE-LEVEL
#-------------------------------------------------------------------------------

  # Get all Site-specific value-level data harmonization monitors and row-bind them
  DataHarmonization.Monitor.ValueLevel.Sites <- RefinementReports %>%
                                                      map(\(SiteRefinementReport) SiteRefinementReport %>%
                                                                                      pluck("DataHarmonization", "Monitors", "ValueLevel") %>%
                                                                                      list_rbind(names_to = "Table")) %>%
                                                      list_rbind(names_to = "Site")

  DataHarmonization.Monitor.ValueLevel.Cumulated <- DataHarmonization.Monitor.ValueLevel.Sites %>%
                                                        group_by(Table, Feature, Value.Source) %>%
                                                            arrange(desc(IsOccurring), .by_group = TRUE) %>%      # Make sure that a Site-specific row can be on top where 'IsOccurring' is TRUE
                                                            summarize(Site = "All",
                                                                      across(!starts_with("Count"), ~ .x[1]),      # Take first row in group for all columns other than the ones starting with 'Count'
                                                                                                                   # Note: This assumes that different harmonization paths for different Sites are not possible. Possibly needs to be adjusted if non-deterministic data remediation methods are applied in the future.
                                                                      across(starts_with("Count"), ~ sum(.x, na.rm = TRUE))) %>%
                                                        ungroup() %>%
                                                        relocate(Site, .before = 1)

  DataHarmonization.Monitor.ValueLevel <- DataHarmonization.Monitor.ValueLevel.Cumulated %>%
                                              bind_rows(DataHarmonization.Monitor.ValueLevel.Sites) %>%
                                              group_by(Site, Table) %>%
                                                  arrange(Feature,
                                                          desc(IsOccurring),
                                                          desc(IsEligible.Source),
                                                          desc(IsEligible.Remediated),
                                                          Value.Source,
                                                          .by_group = TRUE) %>%
                                              ungroup() %>%
                                              split(.$Site) %>%
                                                  map(\(X) X %>%
                                                            select(-Site) %>%
                                                            split(.$Table) %>%
                                                                map(\(X) X %>% select(-Table)))


#-------------------------------------------------------------------------------
  return(list(Log = Log,
              Process = ProcessReport,
              Counter = list(DataSetLevel = Counter.DataSetLevel,
                             TableLevel = Counter.TableLevel,
                             StageLevel = Counter.StageLevel,
                             Details = Counter.Details),
              DataHarmonization = list(Reports = list(DataSetLevel = DataHarmonization.Report.DataSetLevel,
                                                      TableLevel = DataHarmonization.Report.TableLevel,
                                                      FeatureLevel = DataHarmonization.Report.FeatureLevel,
                                                      ValueLevel = DataHarmonization.Report.ValueLevel,
                                                      ValueSets = DataHarmonization.ValueSets),
                                       Monitors = list(TableLevel = DataHarmonization.Monitor.TableLevel,
                                                       FeatureLevel = DataHarmonization.Monitor.FeatureLevel,
                                                       ValueLevel = DataHarmonization.Monitor.ValueLevel))))
}
