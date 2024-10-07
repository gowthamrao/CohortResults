#' utility function to make sure connection is closed after usage
with_dbc_connection <- function(connection, code) {
  on.exit({
    DatabaseConnector::disconnect(connection)
  })
  eval(substitute(code), envir = connection, enclos = parent.frame())
}

# Create a cohort definition set from test cohorts
loadTestCohortDefinitionSet <- function(cohortIds = NULL, useSubsets = TRUE) {
  if (grepl("testthat", getwd())) {
    cohortPath <- "cohorts"
  } else {
    cohortPath <- file.path("tests", "testthat", "cohorts")
  }

  creationFile <- file.path(cohortPath, "CohortsToCreate.csv")
  cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
    settingsFileName = creationFile,
    sqlFolder = cohortPath,
    jsonFolder = cohortPath,
    cohortFileNameValue = c("cohortId")
  )
  if (!is.null(cohortIds)) {
    cohortDefinitionSet <- cohortDefinitionSet |> dplyr::filter(cohortId %in% cohortIds)
  }

  cohortDefinitionSet$checksum <- computeChecksum(cohortDefinitionSet$sql)

  return(cohortDefinitionSet)
}
