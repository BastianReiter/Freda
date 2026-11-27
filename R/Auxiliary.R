

#===============================================================================
#   Freda Internal Auxiliary Functions
#===============================================================================


#===============================================================================
#' AddCumulativeRow
#'
#' Adds a cumulative row to a tibble/data.frame
#'
#' @param DataFrame \code{data.frame}
#' @param StringInNonNumericColumns \code{string} - Default: "All"
#' @keywords internal
#' @export
AddCumulativeRow <- function(DataFrame,
                             StringInNonNumericColumns = "All")
{
  # Get names of all non-numeric columns
  NonNumericColumns <- names(DataFrame %>% select(!where(is.numeric)))

  # Make cumulative row
  CumulativeRow <- DataFrame %>%
                        summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%      # Sum all non-numeric columns
                        bind_cols(setNames(rep(list(StringInNonNumericColumns), length(NonNumericColumns)), NonNumericColumns))      # Re-add non-numeric columns and assign 'StringInNonNumericColumns' as values

  # Row-bind original data.frame and cumulative row
  DataFrame %>%
      bind_rows(CumulativeRow)
}
#===============================================================================



#===============================================================================
#' Counter.New
#'
#' Initiate a Counter \code{tibble} with one or multiple entries
#'
#' @return A \code{tibble}
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Counter.New <- function(ProcessingStage = NA_character_,
                        Table = NA_character_,
                        ProcessTopic = NA_character_,
                        ProcessTopic.Subgroup = NA_character_,
                        CountLevel = NA_character_,
                        CountRecords.Prior = NA_integer_,
                        CountRootSubjects.Prior = NA_integer_,
                        CountSeedSubjects.Prior = NA_integer_,
                        CountRecords.Detected = NA_integer_,
                        CountRootSubjects.Affected = NA_integer_,
                        CountSeedSubjects.Affected = NA_integer_,
                        CountRecords.Nonconforming = NA_integer_,
                        CountRecords.Removed = NA_integer_,
                        CountRecords.Added = NA_integer_,
                        CountRecords.Change = NA_integer_,
                        CountRecords.Change.Proportion = NA_real_,
                        CountRootSubjects.Change = NA_integer_,
                        CountRootSubjects.Change.Proportion = NA_real_,
                        CountSeedSubjects.Change = NA_integer_,
                        CountSeedSubjects.Change.Proportion = NA_real_,
                        CountRecords.Post = NA_integer_,
                        CountRootSubjects.Post = NA_integer_,
                        CountSeedSubjects.Post = NA_integer_,
                        Message = NA_character_,
                        MessageClass = "Info",
                        MessagePriority = 1L,
                        Timestamp = Sys.time(),
                        PrintMessage = FALSE)
#-------------------------------------------------------------------------------
{
  assert_that(is.character(ProcessingStage),
              is.character(Table),
              is.character(ProcessTopic),
              is.character(ProcessTopic.Subgroup),
              is.character(CountLevel),
              is.numeric(CountRecords.Prior),
              is.numeric(CountRootSubjects.Prior),
              is.numeric(CountSeedSubjects.Prior),
              is.numeric(CountRecords.Detected),
              is.numeric(CountRootSubjects.Affected),
              is.numeric(CountSeedSubjects.Affected),
              is.numeric(CountRecords.Nonconforming),
              is.numeric(CountRecords.Removed),
              is.numeric(CountRecords.Added),
              is.numeric(CountRecords.Change),
              is.numeric(CountRecords.Change.Proportion),
              is.numeric(CountRootSubjects.Change),
              is.numeric(CountRootSubjects.Change.Proportion),
              is.numeric(CountSeedSubjects.Change),
              is.numeric(CountSeedSubjects.Change.Proportion),
              is.numeric(CountRecords.Post),
              is.numeric(CountRootSubjects.Post),
              is.numeric(CountSeedSubjects.Post),
              is.character(Message),
              is.character(MessageClass),
              is.numeric(MessagePriority),
              is.time(Timestamp),
              is.flag(PrintMessage))

  Counter <- tibble(ProcessingStage = ProcessingStage,
                    Table = Table,
                    ProcessTopic = ProcessTopic,
                    ProcessTopic.Subgroup = ProcessTopic.Subgroup,
                    CountLevel = CountLevel,
                    CountRecords.Prior = CountRecords.Prior,
                    CountRootSubjects.Prior = CountRootSubjects.Prior,
                    CountSeedSubjects.Prior = CountSeedSubjects.Prior,
                    CountRecords.Detected = CountRecords.Detected,
                    CountRootSubjects.Affected = CountRootSubjects.Affected,
                    CountSeedSubjects.Affected = CountSeedSubjects.Affected,
                    CountRecords.Nonconforming = CountRecords.Nonconforming,
                    CountRecords.Removed = CountRecords.Removed,
                    CountRecords.Added = CountRecords.Added,
                    CountRecords.Change = CountRecords.Change,
                    CountRecords.Change.Proportion = case_when(is.na(CountRecords.Change.Proportion) & CountRecords.Prior > 0 ~ CountRecords.Change / CountRecords.Prior,
                                                               .default = CountRecords.Change.Proportion),
                    CountRootSubjects.Change = CountRootSubjects.Change,
                    CountRootSubjects.Change.Proportion = case_when(is.na(CountRootSubjects.Change.Proportion) & CountRootSubjects.Prior > 0 ~ CountRootSubjects.Change / CountRootSubjects.Prior,
                                                                    .default = CountRootSubjects.Change.Proportion),
                    CountSeedSubjects.Change = CountSeedSubjects.Change,
                    CountSeedSubjects.Change.Proportion = case_when(is.na(CountSeedSubjects.Change.Proportion) & CountSeedSubjects.Prior > 0 ~ CountSeedSubjects.Change / CountSeedSubjects.Prior,
                                                                    .default = CountSeedSubjects.Change.Proportion),
                    CountRecords.Post = CountRecords.Post,
                    CountRootSubjects.Post = CountRootSubjects.Post,
                    CountSeedSubjects.Post = CountSeedSubjects.Post,
                    Message = Message,
                    MessageClass = MessageClass,
                    MessagePriority = MessagePriority,
                    Timestamp = Timestamp)

  if (PrintMessage == TRUE) { Log.Print(Counter) }

  return(Counter)
}

#-------------------------------------------------------------------------------

#' Counter.Add
#'
#' Add one or more records to an existing Counter tibble and optionally print messages in the process.
#'
#' @param Counter \code{data.frame} - An existing Counter.
#' @param Entry \code{data.frame} - New Counter entry
#' @param PrintMessage \code{logical flag} - Whether to print the messages contained in 'Entry'
#' @return The updated Counter \code{data.frame}
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Counter.Add <- function(Counter,
                        Entry,
                        PrintMessage = FALSE)
#-------------------------------------------------------------------------------
{
  assert_that(is.data.frame(Counter),
              is.data.frame(Entry),
              is.flag(PrintMessage))

  Counter <- Counter %>%
                  bind_rows(Entry) %>%
                  fill(ProcessingStage,      # Adopt values from previous rows
                       .direction = "down")

  if (PrintMessage == TRUE) { Log.Print(Entry) }

  return(Counter)
}

#-------------------------------------------------------------------------------

#' Counter.Make
#'
#' Turn a data.frame with some properties of a Counter entry into a full Counter entry
#'
#' @param CounterData \code{data.frame} - data on new Counter entries
#' @param PrintMessage \code{logical flag} - Whether messages should be printed to console
#' @return A \code{data.frame} containing full Counter properties
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Counter.Make <- function(CounterData,
                         PrintMessage = FALSE)
#-------------------------------------------------------------------------------
{
  assert_that(is.data.frame(CounterData))
  assert_that(is.flag(PrintMessage))

  # Select only columns in 'CounterData' that would appear in a Counter entry
  CounterData <- CounterData %>%
                      select(any_of(names(Counter.New())))

  Counter <- do.call(Counter.New, args = c(as.list(CounterData), PrintMessage = PrintMessage))

  return(Counter)
}
#===============================================================================



#===============================================================================
#' GetClass
#'
#' Wrapper around \code{base::class()} with more informative output
#'
#' @param Object Arbitrary R object
#' @return \code{string}
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
GetClass <- function(Object)
{
  Class <- class(Object)[1]      # Some objects return more than one string as class info (e.g. ResourceClient objects). Take only first string for these cases.

  if (tibble::is_tibble(Object)) { Class <- "tibble" }

  return(Class)
}
#===============================================================================



#===============================================================================
#' Log.New
#'
#' Initiate a log report \code{tibble} with one or multiple entries
#'
#' @param PrintMessage \code{logical flag} - Whether to print messages after creation of log
#' @param PrintMessage.Compile \code{logical flag} - Whether to compile a message from different features or just print the content of 'Message'.
#' @param PrintMessage.Compilation \code{string} - Using pseudo-code tags, which features of the input log should be compiled into a printed message.
#'
#' @return A \code{tibble}
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Log.New <- function(Timestamp = Sys.time(),
                    ProcessingStage = NA_character_,
                    Table = NA_character_,
                    ProcessTopic = NA_character_,
                    ProcessTopic.Subgroup = NA_character_,
                    ProcessExecution = NA_character_,
                    Message = NA_character_,
                    MessageClass = "Info",
                    MessagePriority = 1L,
                    PrintMessage = FALSE,
                    PrintMessage.Compile = TRUE,
                    PrintMessage.Compilation = "<$Table$> - <$ProcessTopic$>: <$Message$>")
#-------------------------------------------------------------------------------
{
  assert_that(is.time(Timestamp),
              is.character(ProcessingStage),
              is.character(Table),
              is.character(ProcessTopic),
              is.character(ProcessTopic.Subgroup),
              is.character(ProcessExecution),
              is.character(Message),
              is.character(MessageClass),
              is.numeric(MessagePriority),
              is.flag(PrintMessage),
              is.flag(PrintMessage.Compile),
              is.string(PrintMessage.Compilation))

  Log <- tibble(Timestamp = Timestamp,
                ProcessingStage = ProcessingStage,
                Table = Table,
                ProcessTopic = ProcessTopic,
                ProcessTopic.Subgroup = ProcessTopic.Subgroup,
                ProcessExecution = ProcessExecution,
                Message = Message,
                MessageClass = MessageClass,
                MessagePriority = MessagePriority)

  if (PrintMessage == TRUE) { Log.Print(Log = Log,
                                        .Compile = PrintMessage.Compile,
                                        .Compilation = PrintMessage.Compilation) }

  return(Log)
}

#-------------------------------------------------------------------------------

#' Log.Add
#'
#' Add one or more records to an existing log data.frame and optionally print messages in the process.
#'
#' @param Log \code{data.frame} - An existing log.
#' @param Entry \code{data.frame} - New log entry
#' @param PrintMessage \code{logical flag} - Whether to print the messages contained in 'Log'
#' @param PrintMessage.Compile \code{logical flag} - Whether to compile a message from different features or just print the content of 'Message'.
#' @param PrintMessage.Compilation \code{string} - Using pseudo-code tags, which features of the input log should be compiled into a printed message.
#' @return The updated log \code{data.frame}
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Log.Add <- function(Log,
                    Entry,
                    PrintMessage = FALSE,
                    PrintMessage.Compile = TRUE,
                    PrintMessage.Compilation = "<$Table$> - <$ProcessTopic$>: <$Message$>")
#-------------------------------------------------------------------------------
{
  assert_that(is.data.frame(Log),
              is.data.frame(Entry),
              is.flag(PrintMessage),
              is.flag(PrintMessage.Compile),
              is.string(PrintMessage.Compilation))

  Log <- Log %>%
            bind_rows(Entry) %>%
            fill(ProcessingStage,      # Adopt values from previous rows
                 .direction = "down")

  if (PrintMessage == TRUE) { Log.Print(Log = Entry,
                                        .Compile = PrintMessage.Compile,
                                        .Compilation = PrintMessage.Compilation) }

  return(Log)
}

#-------------------------------------------------------------------------------

#' Log.Make
#'
#' Turn a data.frame with some properties of a log entry into a full log entry
#'
#' @param LogData \code{data.frame} - data on new log entries
#' @param PrintMessage \code{logical flag} - Whether messages should be printed to console
#' @param PrintMessage.Compile \code{logical flag} - Whether to compile a message from different features or just print the content of 'Message'.
#' @param PrintMessage.Compilation \code{string} - Using pseudo-code tags, which features of the input log should be compiled into a printed message.
#' @return A \code{data.frame} containing full log entry properties
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Log.Make <- function(LogData,
                     PrintMessage = FALSE,
                     PrintMessage.Compile = TRUE,
                     PrintMessage.Compilation = "<$Table$> - <$ProcessTopic$>: <$Message$>")
#-------------------------------------------------------------------------------
{
  assert_that(is.data.frame(LogData),
              is.flag(PrintMessage),
              is.flag(PrintMessage.Compile),
              is.string(PrintMessage.Compilation))

  # Select only columns in 'LogData' that would appear in a log entry
  LogData <- LogData %>%
                  select(any_of(names(Log.New())))

  Log <- do.call(Log.New, args = c(as.list(LogData),
                                   PrintMessage = PrintMessage,
                                   PrintMessage.Compile = PrintMessage.Compile,
                                   PrintMessage.Compilation = PrintMessage.Compilation))

  return(Log)
}

#-------------------------------------------------------------------------------

#' Log.Print
#'
#' Print messages contained in a log report data.frame
#'
#' @param Log \code{data.frame} - Log data.frame
#' @param .Compile \code{logical flag} - Whether to compile a message from different features or just print the content of 'Message'.
#' @param .Compilation \code{string} - Using pseudo-code tags, which features of the input log should be compiled into a printed message.
#' @return No return
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Log.Print <- function(Log,
                      .Compile = TRUE,
                      .Compilation = "<$Table$> - <$ProcessTopic$>: <$Message$>")
#-------------------------------------------------------------------------------
{
  assert_that(is.data.frame(Log),
              is.flag(.Compile),
              is.string(.Compilation))

  PrintExpression <- "Message"

  if (.Compile == TRUE)
  {
      PrintExpression <- .Compilation %>%
                              str_replace_all("<\\$(.*?)\\$>", '", if_else(is.na(\\1), "", as.character(\\1)), "') %>%
                              { paste0('str_squish(paste0("', ., '"))') }
  }

  Messages <- Log %>%
                mutate(Message = case_when(.Compile == FALSE ~ Message,
                                           str_starts(MessageClass, "Details.") ~ Message,
                                           .default = !!rlang::parse_expr(PrintExpression)),
                       Message = Message %>% str_replace_all("- :", "") %>% str_trim()) %>%
                select(MessageClass,
                       Message) %>%
                tibble::deframe()

  for (i in 1:length(Messages)) { PrintSoloMessage(Messages[i]) }
}
#===============================================================================



#===============================================================================
#' PrintSoloMessage
#'
#' Print text passed in a one-dimensional (named) character vector. Optionally add symbols and specific formatting based on vector element name.
#'
#' @param message \code{character vector} of length 1 with optional name
#' @export
#-------------------------------------------------------------------------------
PrintSoloMessage <- function(message)
#-------------------------------------------------------------------------------
{
  assert_that(is.vector(message))

  MessageClass <- names(message)
  if (is.null(MessageClass)) { MessageClass <- "Info" }
  MessageClassTypeDetails <- FALSE

  if (str_starts(MessageClass, "Details."))
  {
      cat("   - ")
      MessageClass <- str_remove(MessageClass, "Details.")
  }

  if (MessageClass == "Topic")
  {
      # Print topic string in bold letters (formatted with ANSI code \033...) and with horizontal line underneath
      cat("\n", "\033[1m", as.character(message), "\n", paste0(rep("~", times = stringr::str_length(as.character(message))), collapse = ""), "\033[0m", "\n", sep = "")

  } else if (MessageClass == "Subtopic") {

      cat("\n", paste0(cli::style_bold(cli::style_underline(message)), "\n"))

  } else if (MessageClass == "Special") {

      cat("\n", "\033[1m")
      cli::cat_bullet(as.character(message), bullet = "star")
      cat("\033[0m", "\n")

  } else {

      cli::cat_bullet(as.character(message),
                      bullet = dplyr::case_when(MessageClass == "Info" ~ "info",
                                                MessageClass == "Success" ~ "tick",
                                                MessageClass == "Warning" ~ "warning",
                                                MessageClass == "Failure" ~ "cross",
                                                .default = "none"),
                      bullet_col = dplyr::case_when(MessageClass == "Success" ~ Freda::FredaColors$Green,
                                                    MessageClass == "Warning" ~ Freda::FredaColors$Orange,
                                                    MessageClass == "Failure" ~ Freda::FredaColors$Red,
                                                    .default = "black"))
  }
}
#===============================================================================



#===============================================================================
#' PrintMessages
#'
#' Take list of messages and print them with \code{PrintSoloMessage()}.
#'
#' @param Messages \code{list} List of named vectors
#' @export
#-------------------------------------------------------------------------------
PrintMessages <- function(Messages)
#-------------------------------------------------------------------------------
{
  if (is.vector(Messages))
  {
      for (i in 1:length(Messages)) { PrintSoloMessage(Messages[i]) }

  } else if (is.list(Messages)) {

      purrr::walk(.x = Messages,
            .f = function(Subvector)
                 {
                      cat("\n")

                      for (i in 1:length(Subvector))      # for-loop instead of nested purrr::walk because items in list are vectors
                      {
                          PrintSoloMessage(Subvector[i])
                      }

                      cat("\n")
                 })
  }
}
#===============================================================================



#===============================================================================

# Custom Infix Operator %notin%
#' @noRd
#' @export
'%notin%' <- function(x, y) { !(x %in% y) }

#===============================================================================
