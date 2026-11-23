
#' PrepareSourceData
#'
#' Perform basic preparatory transformations on SourceDataSet prior to Refinement:
#' \itemize{  \item Feature name harmonization
#'            \item Add ID feature (running number) for tables without primary key feature
#'            \item Type- or format-conversion of features }
#'
#' @param SourceDataSet \code{list} of \code{data.frames} - Source Data Set
#' @param FREDAModule \code{list} - A FREDA module containing meta data and processing settings
#' @param FeatureNames.Dictionary Optional \code{list} containing dictionary data for source feature name harmonization (Form: \code{list(Department = c(FAB = "Fachabteilung"))})
#' @param FeatureNames.FuzzyStringMatching.Run \code{logical} - Whether to use fuzzy string matching to harmonize source feature names
#' @param FeatureNames.FuzzyStringMatching.PreferredMethod \code{logical} - Selects the method \code{stringdist()} uses (see \code{stringdist} documentation) - Default: 'jw'
#' @param FeatureNames.FuzzyStringMatching.Tolerance \code{logical} - Number between 0 and 1 relating to normalized distance between to strings (1 meaning furthest distance, 0 meaning no distance). If a string in 'Vector' is not similar enough to any of the 'EligibleStrings' and its minimal harmonized distance exceeds this number, it is set \code{NA}. - Default: 0.2
#' @param AddIDFeature.Do \code{logical} - Whether to add an ID feature (running number) to tables
#' @param AddIDFeature.IDFeatureName \code{string} - The name of the new ID feature
#' @param AddIDFeature.OverwriteExistingIDFeature \code{logical} - Whether to overwrite an existing feature with the same name
#' @param Conversion.IntoCharacter \code{string} - Controls conversion of certain features in data set tables into character type. One of 'None' / 'All' / 'Date'. - Default: 'None'
#' @param Conversion.DateIntoPOSIXct \code{string} - An encoded string representing a list with character vectors containing date formats used in argument 'tryFormats' in function \code{base::as.POSIXct}. List element should be either '.All' for all date features or specific date feature names. - Default: \code{NULL}
#' @param RecodeFeatureNames \code{logical} - Indicating whether (after primary harmonization) feature names should be recoded from 'source' to 'refined' feature names according to FREDAModule-specific meta data
#'
#' @return A \code{list} containing
#'          \itemize{ \item The input SourceDataSet (\code{list}) with harmonized (and optionally refined) feature names
#'                    \item The original input SourceDataSet (\code{list})
#'                    \item Messages (\code{character vector}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PrepareSourceData <- function(SourceDataSet,
                              FREDAModule,
                              FeatureNames.Dictionary = list(),
                              FeatureNames.FuzzyStringMatching.Run = FALSE,
                              FeatureNames.FuzzyStringMatching.PreferredMethod = "jw",
                              FeatureNames.FuzzyStringMatching.Tolerance = 0.2,
                              AddIDFeature.Do = FALSE,
                              AddIDFeature.IDFeatureName = "ID",
                              AddIDFeature.OverwriteExistingIDFeature = FALSE,
                              Conversion.IntoCharacter = "None",
                              Conversion.DateIntoPOSIXct = NULL,
                              RecodeFeatureNames = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # SourceDataSet <- P21.SourceDataSet
  # FREDAModule <- P21.FREDAModule
  # FeatureNames.Dictionary <- list(Department = c(FAB = "Fachabteilung"))
  # FeatureNames.FuzzyStringMatching.Run <- TRUE
  # FeatureNames.FuzzyStringMatching.PreferredMethod <- "jw"
  # FeatureNames.FuzzyStringMatching.Tolerance <- 0.2
  # AddIDFeature.Do <- FALSE
  # AddIDFeature.IDFeatureName <- "ID"
  # AddIDFeature.OverwriteExistingIDFeature <- FALSE
  # Conversion.IntoCharacter <- "None"
  # Conversion.DateIntoPOSIXct <- .encode_tidy_eval("list('.All' = c('%Y%m%d%H%M', '%Y%m%d', '%Y-%m-%d'))", .get_encode_dictionary())
  # RecodeFeatureNames <- TRUE

  # --- Argument Validation ---
  assert_that(is.list(SourceDataSet),
              is.list(FREDAModule),
              is.list(FeatureNames.Dictionary),
              is.flag(FeatureNames.FuzzyStringMatching.Run),
              is.string(FeatureNames.FuzzyStringMatching.PreferredMethod),
              is.number(FeatureNames.FuzzyStringMatching.Tolerance),
              is.flag(AddIDFeature.Do),
              is.string(Conversion.IntoCharacter),
              is.flag(RecodeFeatureNames))
  if (FeatureNames.FuzzyStringMatching.Tolerance < 0 | FeatureNames.FuzzyStringMatching.Tolerance > 1) { stop("ERROR: Value of argument 'FeatureNames.FuzzyStringMatching.Tolerance' must be between 0 and 1.") }
  if (!is.null(AddIDFeature.IDFeatureName)) { assert_that(is.string(AddIDFeature.IDFeatureName)) }
  if (!is.null(AddIDFeature.OverwriteExistingIDFeature)) { assert_that(is.flag(AddIDFeature.OverwriteExistingIDFeature)) }
  if (!(Conversion.IntoCharacter %in% c("None", "All", "Date"))) { stop("ERROR: Value of argument 'Conversion.IntoCharacter' must be one of 'None' / 'All' / 'Date'.") }
  if (!is.null(Conversion.DateIntoPOSIXct)) { assert_that(is.string(Conversion.DateIntoPOSIXct)) }

#===============================================================================
# - OVERVIEW -
#===============================================================================
#   A)  Optionally add ID feature
#   B)  Try to harmonize source feature names
#   C)  Optionally recode feature names according to feature meta data
#   D)  Optionally convert features to character type
#   E)  Optionally convert date features to POSIXct type
#-------------------------------------------------------------------------------

  # Save a 'backup' of the input data set (this will be part of output so changes to feature names can be tracked)
  OriginalSourceDataSet <- SourceDataSet

  # Decode string 'Conversion.DateIntoPOSIXct' and recreate list object from the result
  if (length(Conversion.DateIntoPOSIXct) > 0)
  {
      Conversion.DateIntoPOSIXct <- eval(parse(text = .decode_tidy_eval(Conversion.DateIntoPOSIXct, .get_encode_dictionary())))
      assert_that(is.list(Conversion.DateIntoPOSIXct))
  }

#-------------------------------------------------------------------------------

  # Initiate output messaging object
  Messages <- list(SourceFeatureNames = character())


# Performing preparatory operations
#-------------------------------------------------------------------------------
  Preparation <- SourceDataSet %>%
                    imap(function(Table, tablename)
                         {
                            # Initiate messaging vector for current 'Table'
                            CurrentMessages <- character()

                            #---------------------------------------------------
                            # A) Optionally add ID feature (running number) to Table
                            #---------------------------------------------------
                            if (AddIDFeature.Do == TRUE)
                            {
                                TableHasIDFeature <- AddIDFeature.IDFeatureName %in% names(Table)

                                if (TableHasIDFeature == FALSE || (TableHasIDFeature == TRUE & AddIDFeature.OverwriteExistingIDFeature == TRUE))
                                {
                                    Table <- Table %>%
                                                eval(expr = parse(text = paste0("mutate(., '",
                                                                                AddIDFeature.IDFeatureName,
                                                                                "' = 1:n(), .before = 1)")))
                                }
                            }


                            #---------------------------------------------------
                            # B) Try to harmonize Source feature names using fuzzy string matching and dictionary data
                            #---------------------------------------------------

                            # Get expected feature names from FREDAModule-specific meta data
                            EligibleFeatureNames <- FREDAModule$MetaData$Features %>%
                                                        filter(TableName.Refined == tablename) %>%
                                                        select(FeatureName.Source,
                                                               FeatureName.Refined) %>%
                                                        unlist(use.names = FALSE)

                            # Get Dictionary (character vector) from passed list
                            FeatureNames.Dictionary <- FeatureNames.Dictionary[[tablename]]

                            # Initiate 'HarmonizedFeatureNames'
                            HarmonizedFeatureNames <- names(Table)

                            # Optionally try Fuzzy String Matching first (also matching to Dictionary look up values if possible, because these will subsequentially turn into eligible feature names)
                            if (FeatureNames.FuzzyStringMatching.Run == TRUE)
                            {
                                HarmonizedFeatureNames <- GetFuzzyStringMatches(Vector = names(Table),
                                                                                EligibleStrings = c(EligibleFeatureNames, names(FeatureNames.Dictionary)),
                                                                                PreferredMethod = FeatureNames.FuzzyStringMatching.PreferredMethod,
                                                                                Tolerance = FeatureNames.FuzzyStringMatching.Tolerance)
                            }

                            # Try Dictionary Look-up, if dictionary data is passed
                            if (length(FeatureNames.Dictionary) > 0)
                            {
                                HarmonizedFeatureNames <- if_else(is.na(FeatureNames.Dictionary[HarmonizedFeatureNames]),
                                                                  HarmonizedFeatureNames,
                                                                  FeatureNames.Dictionary[HarmonizedFeatureNames])
                            }


                            #---------------------------------------------------
                            # C) Optionally change feature names according to FREDAModule meta data (FeatureName.Source -> FeatureName.Refined)
                            #---------------------------------------------------

                            # Initiate 'RefinedFeatureNames'
                            RecodedFeatureNames <- NA

                            if (RecodeFeatureNames == TRUE)
                            {
                                # Create dictionary (named character vector) from FREDAModule meta data
                                RecodingDictionary <- FREDAModule$MetaData$Features %>%
                                                          filter(TableName.Refined == tablename) %>%
                                                          select(FeatureName.Source,
                                                                 FeatureName.Refined) %>%
                                                          tibble::deframe()

                                if (length(RecodingDictionary) > 0)
                                {
                                    # Create recoded versions of previously harmonized 'source' feature names
                                    RefinedFeatureNames <- if_else(is.na(RecodingDictionary[HarmonizedFeatureNames]),
                                                                   HarmonizedFeatureNames,
                                                                   RecodingDictionary[HarmonizedFeatureNames])

                                    # Also recode feature names stated in element names of list argument 'Conversion.DateIntoPOSIXct'
                                    if (length(Conversion.DateIntoPOSIXct) > 0)
                                    {
                                        names(Conversion.DateIntoPOSIXct) <- if_else(is.na(RecodingDictionary[names(Conversion.DateIntoPOSIXct)]),
                                                                                       names(Conversion.DateIntoPOSIXct),
                                                                                       RecodingDictionary[names(Conversion.DateIntoPOSIXct)])
                                    }
                                }
                            }


                            #---------------------------------------------------
                            # Create tibble containing transformation tracks of all feature names
                            #---------------------------------------------------

                            FeatureNames <- tibble(Original = names(Table),
                                                   IsEligible.Original = (Original %in% EligibleFeatureNames),
                                                   Harmonized = HarmonizedFeatureNames,
                                                   Refined = RefinedFeatureNames,
                                                   ChosenSourceName = case_when(IsEligible.Original == FALSE ~ Harmonized,
                                                                             .default = Original),
                                                   IsEligible.ChosenSourceName = (ChosenSourceName %in% EligibleFeatureNames),
                                                   HasChangedSourceName = !(Original == ChosenSourceName))

                            # Obtain changed feature names for messaging
                            ChangedSourceNames <- FeatureNames %>%
                                                    filter(HasChangedSourceName == TRUE)

                            if (length(ChangedSourceNames) == 0 || nrow(ChangedSourceNames) == 0)
                            {
                                Message <- paste0("Table '", tablename, "': No changes to source feature names.")
                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages,
                                                     Info = Message)

                            } else {

                                for (i in 1:nrow(ChangedSourceNames))
                                {
                                    Message <- paste0("Table '", tablename, "': Changed feature name '", ChangedSourceNames$Original[i], "' to '", ChangedSourceNames$ChosenSourceName[i], "'.")
                                    cli::cat_bullet(Message, bullet = "info")
                                    CurrentMessages <- c(CurrentMessages,
                                                         Info = Message)
                                }
                            }

                            # Obtain remaining ineligible feature names
                            RemainingIneligibleNames <- FeatureNames %>%
                                                            filter(IsEligible.ChosenSourceName == FALSE)

                            if (length(RemainingIneligibleNames) > 0 && nrow(RemainingIneligibleNames) > 0)
                            {
                                for (i in 1:nrow(RemainingIneligibleNames))
                                {
                                    Message <- paste0("Table '", tablename, "': The feature name '", RemainingIneligibleNames$Original[i], "' is ineligible and could not be harmonized!")
                                    cli::cat_bullet(Message, bullet = "warning", bullet_col = "red")
                                    CurrentMessages <- c(CurrentMessages,
                                                         Warning = Message)
                                }
                            }


                            #---------------------------------------------------
                            # Commit feature renaming
                            #---------------------------------------------------

                            if (RecodeFeatureNames == TRUE)
                            {
                                names(Table) <- FeatureNames$Refined

                            } else {

                                names(Table) <- FeatureNames$ChosenSourceName
                            }


                            #---------------------------------------------------
                            # D) Optionally convert features to character type
                            #---------------------------------------------------

                            if (Conversion.IntoCharacter == "All")
                            {
                                Table <- Table %>%
                                            mutate(across(everything(),
                                                          ~ as.character(.x)))
                                                          #~ vctrs::vec_cast(.x, character())))      # Using vctrs::vec_cast() to avoid implicit conversions performed with base::as.character()

                                Message <- paste0("Table '", tablename, "': Converted ALL features into character type.")
                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages, Info = Message)

                            } else if (Conversion.IntoCharacter == "Date") {

                                CountDateFeatures <- Table %>%
                                                        select(where(~ inherits(.x, c("Date", "POSIXct")))) %>%
                                                        ncol()

                                if (CountDateFeatures == 0)
                                {
                                    Message <- paste0("Table '", tablename, "' had no features of class 'Date' or 'POSIXct'.")

                                } else {

                                    Table <- Table %>%
                                                mutate(across(where(~ inherits(.x, c("Date", "POSIXct"))),
                                                              ~ as.character(.x)))

                                    Message <- paste0("Table '", tablename, "': Converted ", CountDateFeatures, " DATE features into character type.")
                                }

                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages, Info = Message)
                            }


                            #---------------------------------------------------
                            # E) Optionally convert date features (or character features containing date data) into POSIXct
                            #---------------------------------------------------

                            # Get date feature names from FREDAModule meta data
                            DateFeatureNames <- FREDAModule$MetaData$Features %>%
                                                    filter(TableName.Refined == tablename,
                                                           Type == "date") %>%
                                                    { if (RecodeFeatureNames == TRUE) { pull(., FeatureName.Refined) }
                                                      else { pull(., FeatureName.Source) } }

                            # Only keep data feature names that occur in current table
                            DateFeatureNames <- DateFeatureNames[DateFeatureNames %in% names(Table)]

                            if (length(DateFeatureNames) > 0 && length(Conversion.DateIntoPOSIXct) > 0)
                            {
                                # First, check if there is a list element named '.All' and proceed accordingly
                                if (".All" %in% names(Conversion.DateIntoPOSIXct))
                                {
                                    Table <- Table %>%
                                                mutate(across(all_of(DateFeatureNames),
                                                              ~ as.POSIXct(.x,
                                                                           tryFormats = Conversion.DateIntoPOSIXct[[".All"]],
                                                                           tz = "UTC")))

                                    Message <- paste0("Table '", tablename, "': Converted date features ", paste0("'", DateFeatureNames, "'", collapse = " / "), " into POSIXct type after trying the date format(s) ", paste0("'", Conversion.DateIntoPOSIXct[[".All"]], "'", collapse = " / "), ".")
                                    cli::cat_bullet(Message, bullet = "info")
                                    CurrentMessages <- c(CurrentMessages, Info = Message)
                                }

                                # Proceed with other date feature names if there are any other relevant ones
                                for (datefeaturename in names(Conversion.DateIntoPOSIXct))
                                {
                                    if (datefeaturename %in% names(Table))
                                    {
                                        Table <- Table %>%
                                                    mutate(across(all_of(datefeaturename),
                                                                  ~ as.POSIXct(.x,
                                                                               tryFormats = Conversion.DateIntoPOSIXct[[datefeaturename]],
                                                                               tz = "UTC")))

                                        Message <- paste0("Table '", tablename, "': Converted date feature '", datefeaturename, "' into POSIXct type after trying the date format(s) ", paste0("'", Conversion.DateIntoPOSIXct[[datefeaturename]], "'", collapse = " / "), ".")
                                        cli::cat_bullet(Message, bullet = "info")
                                        CurrentMessages <- c(CurrentMessages, Info = Message)
                                    }
                                }
                            }


                            #---------------------------------------------------
                            # Return processed Table and Messages
                            #---------------------------------------------------
                            return(list(Table = Table,
                                        Messages = CurrentMessages))
                         })

# Extract SourceDataSet and Messages
#-------------------------------------------------------------------------------
  SourceDataSet <- Preparation %>% map("Table")
  Messages <- Preparation %>%
                  map("Messages") %>%
                  unlist() %>%
                  set_names(sub(".*\\.", "", names(.)))


# Create and display message about recoding of feature names
#-------------------------------------------------------------------------------
  if (RecodeFeatureNames == TRUE)
  {
      Message <- paste0("After primary feature name harmonization: Recoded feature names to their 'refined' version according to meta data!")
      cli::cat_bullet(Message, bullet = "tick", bullet_col = "green")
      Messages <- c(Messages,
                    Success = Message)
  }

#-------------------------------------------------------------------------------
  return(list(SourceDataSet = SourceDataSet,
              OriginalSourceDataSet = OriginalSourceDataSet,
              Messages = Messages))
}
