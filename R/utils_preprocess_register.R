#' Function for clearing the register cache
#'
#' @return 0 for success, 1 for failure, invisibly (see `unlink`)
#'
#' @author Daniel Nüst
#' @importFrom R.cache getCacheRootPath
#' @export
register_clear_cache <- function() {
  path <- R.cache::getCacheRootPath()
  message("Deleting cache path ", path)
  unlink(path, recursive = TRUE)
}

#' Function for adding clickable links to the report for each entry in the register table.
#' 
#' @param register_table The register table
#' @param register The register from the register.csv file
#' @return The adjusted register table
add_report_links <- function(register_table, register) {

  reports <- c()

  for (i in seq_len(nrow(register))) {
    config_yml <- get_codecheck_yml(register[i, ]$Repo)

    report <- NA
    if (!is.null(config_yml)) {
      report <- config_yml$report
    }

    reports <- c(reports, report)
  }
  register_table$Report <- reports
  return(register_table)
}

#' Function for adding clickable links to the issue number of each report in the register table.
#' 
#' @param register_table The register table
#' @param register The register from the register.csv file
#' @return The adjusted register table
add_issue_number_links <- function(register_table, register) {
  register_table$Issue <- sapply(
    X = register$Issue,
    FUN = function(issue_id) {
      if (!is.na(issue_id)) {
        paste0(
          "[",
          issue_id,
          "](https://github.com/codecheckers/register/issues/",
          issue_id, ")"
        )
      } else {
        issue_id
      }
    }
  )
  return(register_table)
}

#' Function for adding check time to each report in the register table.
#' 
#' @param register_table The register table
#' @param register The register from the register.csv file
#' @return The adjusted register table
add_check_time <- function(register_table, register) {
  check_times <- c()

  # Looping over the entries in the register
  for (i in seq_len(nrow(register))) {
    config_yml <- get_codecheck_yml(register[i, ]$Repo)

    check_time <- NA
    if (!is.null(config_yml)) {
      check_time <- config_yml$check_time
    }

    check_times <- c(check_times, check_time)
  }
  check_times <- parsedate::parse_date(check_times)
  register_table$`Check date` <- format(check_times, "%Y-%m-%d")

  return(register_table)
}

#' Function for adding codechecker to each report in the register table.
#' 
#' @param register_table The register table
#' @param register The register from the register.csv file
#' @return The adjusted register table
add_codechecker <- function(register_table, register) {
  codecheckers <- vector("list", length = nrow(register))

  # Looping over the entries in the register
  for (i in seq_len(nrow(register))) {
    config_yml <- get_codecheck_yml(register[i, ]$Repo)

    codechecker_info <- list()
    if (!is.null(config_yml)  && !is.null(config_yml$codechecker)) {
      # For each codechecker we enter the data in the form {name} (orcid id: {orcid_id})
      for (codechecker in config_yml$codechecker) {
        if (!((codechecker$name) %in% CONFIG$DICT_ORCID_ID_NAME)){
          CONFIG$DICT_ORCID_ID_NAME[codechecker$ORCID] <- codechecker$name
        }
        #! If they want codechecker column
        #formatted_string <- paste0(codechecker$name, " (ORCID: ", toString(codechecker$ORCID), ")")
        codechecker_info <- c(codechecker_info, codechecker$ORCID)
      }
    }
    codecheckers[[i]] <- codechecker_info
  }
  register_table$`Codechecker` <- codecheckers
  return(register_table)
}

create_temp_register_with_codechecker <- function(register_table){
  # Apply toJSON to each element in the `Codechecker` column
  register_table$Codechecker <- sapply(register_table$Codechecker, toJSON, auto_unbox = TRUE)
  write.csv(register_table, CONFIG$DIR_TEMP_REGISTER_CODECHECKER)
}

#' Function for preprocessing the register to create and return the preprocessed register table.
#' @param register The register
#' @return The preprocessed register table
preprocess_register <- function(register, filter_by) {
    register_table <- register

    if ("codecheckers" %in% filter_by){
      # Adding the codechecker column which is needed for filtering by codechecker later
      register_table <- add_codechecker(register_table, register)
      # Creating a temp register.csv file with a codechecker column which is needed to 
      # filter the registers by codecheckers
      create_temp_register_with_codechecker(register_table)
    }
    register_table <- add_report_links(register_table, register)
    register_table <- add_issue_number_links(register_table, register)
    register_table <- add_check_time(register_table, register)
    return(register_table)
}