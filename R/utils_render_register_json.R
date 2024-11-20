#' Function for adding repository links in the register table for the creation of the json file.
#' 
#' @param register_table The register table
#' @return Register table with adjusted repository links
add_repository_links_json <- function(register_table) {
  register_table$`Repository Link` <- sapply(
    X = register_table$Repository,
    FUN = function(repository) {
      # ! Needs refactoring
      spec <- parse_repository_spec(repository)
      if (spec[["type"]] == "github") {
        paste0(CONFIG$HYPERLINKS[["github"]], spec[["repo"]])
      } else if (spec[["type"]] == "osf") {
        paste0(CONFIG$HYPERLINKS[["osf"]], spec[["repo"]])
      } else if (spec[["type"]] == "gitlab") {
        paste0(CONFIG$HYPERLINKS[["gitlab"]], spec[["repo"]])
      } else {
        repository
      }
    }
  )
  return(register_table)
}

#' Updates columns in a register table with extracted data without any hyperlinks
#' For "Type" and "Venue" it retrieve the text from the hyperlinks.
#'
#' @param register_table A data frame containing rows with repository details.
#' @return Updated register table with updated "Certificate", "Title", "Paper reference", "Type" and "Vene" columns
set_columns_json <- function(register_table){
  cert_ids <- c()
  titles <- c()
  references <- c()
  venue_types <- c()
  venues <- c()

  # Looping through each row in the register table and retrieving
  # the entry details
  for (i in seq_len(nrow(register_table))){
    register_table_row <- register_table[i, ]
    config_yml <- get_codecheck_yml(register_table_row$Repository)

    cert_id <- config_yml$certificate
    cert_ids <- c(cert_ids, cert_id)

    title <- config_yml$paper$title
    titles <- c(titles, title)

    reference <- config_yml$paper$reference
    references <- c(references, reference)

    # Checking if the table has "Venue" column and if so, retireve venue name
    if ("Venue" %in% colnames(register_table)){
      venue_hyperlink <- register_table_row$Venue
      venue <- sub(CONFIG$REGEX[["hyperlink_text"]], "\\1", venue_hyperlink)
      
      venues <- c(venues, venue)
    }

    # Checking if the table has "Type" column and if so, retireve venue type
    if ("Type" %in% colnames(register_table)){
      venue_type_hyperlink <- register_table_row$Type
      venue_type <- sub(CONFIG$REGEX[["hyperlink_text"]], "\\1", venue_type_hyperlink)
      
      venue_types <- c(venue_types, venue_type)
    }
  }

  # Setting the columns with the adjusted data
  register_table$Certificate <- cert_ids
  register_table$Title <- stringr::str_trim(titles)
  register_table$`Paper reference` <- stringr::str_trim(references)

  if ("Type" %in% names(register_table)){
    register_table$Type <- venue_types
  }

  if ("Venue" %in% names(register_table)){
    register_table$Venue <- venues
  }

  return(register_table)
}

#' Renders register json for a single register_table
#' 
#' @param register_table The register table
#' @param table_details List containing details such as the table name, subcat name.
#' @param filter The filter
render_register_json <- function(register_table, table_details, filter) {
  register_table <- add_repository_links_json(register_table)

  # Set paper titles and references
  register_table <- set_columns_json(register_table)

  output_dir <- table_details[["output_dir"]]

  # Keeping only those columns that are mentioned in the json columns and those that 
  # register table already has
  columns_to_keep <- intersect(CONFIG$JSON_COLUMNS, names(register_table))

  jsonlite::write_json(
    register_table[, columns_to_keep],
    path = paste0(output_dir, "register.json"),
    pretty = TRUE
  )

  jsonlite::write_json(
    utils::tail(register_table, 10)[, columns_to_keep],
    path = paste0(output_dir, "featured.json"),
    pretty = TRUE
  )

  jsonlite::write_json(
    list(
      source = generate_href(filter, table_details, "json"),
      cert_count = nrow(register_table)
      # TODO count conferences, preprints,
      # journals, etc.
    ),
    auto_unbox = TRUE,
    path = paste0(output_dir, "/stats.json"),
    pretty = TRUE
  )
}
