
#' GetTableCheck
#'
#' Wrapper function for \code{Freda::CheckTable()}. Checks out a \code{data.frame} and returns an informative list object.
#'
#' @param TableName \code{string} - Name of \code{data.frame} or \code{tibble}
#' @param RequiredFeatureNames Optional \code{character} - Optional names of required features - Default: \code{names()} applied to Table evaluated from \code{TableName}
#' @param EligibleValueSets Optional \code{list} of \code{character vectors} containing sets of eligible values for corresponding features
#' @param GetTemplate \code{logical} - If set to \code{TRUE}, the function returns a template incorporating required feature names without actually checking an existing table
#'
#' @return A \code{list} containing informative meta data about a \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetTableCheck <- function(TableName,
                          RequiredFeatureNames = NULL,
                          EligibleValueSets = NULL,
                          GetTemplate = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(TableName),
              is.flag(GetTemplate))
  if (!is.null(RequiredFeatureNames)) { assert_that(is.character(RequiredFeatureNames)) }
  if (!is.null(EligibleValueSets)) { assert_that(is.list(EligibleValueSets)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Initiate output object
  TableCheck <- NULL

  # If 'Table' is neither data.frame nor tibble, return NULL
  if (!("data.frame" %in% class(Table))) { return(NULL) }

  # Create only template if one of the conditions below is met
  if (length(Table) == 0 || GetTemplate == TRUE)
  {
      TableCheck <- CheckTable(Table = NULL,
                               RequiredFeatureNames = RequiredFeatureNames)

  } else {

      TableCheck <- CheckTable(Table = Table,
                               RequiredFeatureNames = RequiredFeatureNames,
                               EligibleValueSets = EligibleValueSets)
  }

#-------------------------------------------------------------------------------
  return(TableCheck)
}
