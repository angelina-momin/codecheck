#' Function for adding repository links in the register table for the creation of the html file.
#' 
#' @param register_table The register table
#' @return Register table with adjusted repository links
add_repository_links_html <- function(register_table) {
  register_table$Repository <- sapply(
    X = register_table$Repository,
    FUN = function(repository) {
      spec <- parse_repository_spec(repository)
      if (!any(is.na(spec))) {
        urrl <- "#"
        if (spec[["type"]] == "github") {
          urrl <- paste0("https://github.com/", spec[["repo"]])
          paste0("<i class='fa fa-github'></i>&nbsp;[", spec[["repo"]], "](", urrl, ")")
        } else if (spec[["type"]] == "osf") {
          urrl <- paste0("https://osf.io/", spec[["repo"]])
          paste0("<i class='ai ai-osf'></i>&nbsp;[", spec[["repo"]], "](", urrl, ")")
        } else if (spec[["type"]] == "gitlab") {
          urrl <- paste0("https://gitlab.com/", spec[["repo"]])
          paste0("<i class='fa fa-gitlab'></i>&nbsp;[", spec[["repo"]], "](", urrl, ")")
        } else {
          repository
        }
      } else {
        repository
      }
    }
  )
  return(register_table)
}

#' Dynamically generates a html_document.yml with the full paths to the index header, prefix 
#' and postfix.html files. 
#' 
#' @param filter The filter name
#' @param register_table_name The register table name
generate_html_document_yml <- function(filter, register_table_name) {
  dir <- paste0(getwd(), "/", get_output_dir(filter, register_table_name))

  yaml_content <- sprintf(
    "# DO NOT EDIT THIS FILE MANUALLY
    html_document:
      includes:
        in_header: '%sindex_header.html'
        before_body: '%sindex_prefix.html'
        after_body: '%sindex_postfix.html'
      mathjax: null
      highlight: null
      self_contained: false
      lib_dir: libs",
    dir, dir, dir
  )
  writeLines(yaml_content, paste0(dir, "html_document.yml"))
}

#' Dynamically generates the index_postfix.html from a template file 
#' 
#' @param filter The filter name
#' @param register_table_name The register table name
#' 
#' @importFrom whisker whisker.render
create_index_postfix_html <- function(filter, register_table_name){
  hrefs <- generate_html_postfix_hrefs(filter, register_table_name)

  # Using the index_postfix_template
  postfix_template <- readLines(paste0(getwd(), "/docs/index_postfix_template.html"), warn = FALSE)
  # Render the template with the correct hrefs
  output <- whisker.render(postfix_template, hrefs)
  writeLines(output, paste0(get_output_dir(filter, register_table_name), "index_postfix.html"))
}

#' Dynamically generates the index_prefix.html from a template file 
#' 
#' @param filter The filter name
#' @param register_table_name The register table name
create_index_prefix_html <- function(filter, register_table_name){
  # Using the index_prefix_template
  prefix_template <- readLines(paste0(getwd(), "/docs/index_prefix_template.html"), warn = FALSE)

  writeLines(prefix_template, paste0(get_output_dir(filter, register_table_name), "index_prefix.html"))
}

#' Dynamically generates the index_header.html from a template file 
#' 
#' @param filter The filter name
#' @param register_table_name The register table name
create_index_header_html <- function(filter, register_table_name){
  # Using the index_header_template
  header_template <- readLines(paste0(getwd(), "/docs/index_header_template.html"), warn = FALSE)

  writeLines(header_template, paste0(get_output_dir(filter, register_table_name), "index_header.html"))
}

#' Generates the hrefs to set in the postfix.html file.
#' 
#' @param filter The filter name
#' @param register_table_name The register table name
generate_html_postfix_hrefs <- function(filter, register_table_name) {
  hrefs <- list(
    csv_source_href = generate_href(filter, register_table_name, "csv_source"),
    searchable_csv_href = generate_href(filter, register_table_name, "searchable_csv"),
    json_href = generate_href(filter, register_table_name, "json"),
    md_href = generate_href(filter, register_table_name, "md")
  )
  return(hrefs)
}

#' Generate full href for for different href types.
#'
#' @param filter The filter name 
#' @param register_table_name The register table name
#' @param href_type The href type (e.g., 'csv_source', 'searchable_csv', 'json', 'md')
#' 
#' @return String representing the full URL to access the specified resource
generate_href <- function(filter, register_table_name, href_type) {
  # Determine base path based on the resource type
  href_details <- switch(href_type,
         "csv_source" = list(base_url = "https://raw.githubusercontent.com/codecheckers/register/master/docs/", ext = ".csv"),
         "searchable_csv" = list(base_url ="https://github.com/codecheckers/register/blob/master/docs/", ext = ".csv"),
         "json" = list(base_url = "https://codecheck.org.uk/register/", ext = ".json"),
         "md" = list(base_url = "https://codecheck.org.uk/register/", ext = ".md")
        )
  
  if (filter == "none") {
    return(paste0(href_details$base_url, "register", href_details$ext))
  } else if (filter == "venues") {
    venue_category <- determine_venue_category(register_table_name)
    venue_name <- trimws(gsub("[()]", "", gsub(venue_category, "", register_table_name)))
    venue_name <- gsub(" ", "_", venue_name)
    return(paste0(href_details$base_url, filter, "/", venue_category, "/", venue_name, "/register", href_details$ext))
  } else {
    return(paste0(href_details$base_url, filter, "/", register_table_name, "/register", href_details$ext))
  }
}

#' Creates html files for each index section- the index postfix, prefix and the header 
#'
#' @param filter The filter name 
#' @param register_table_name The register table name
create_index_section_files <- function(filter, register_table_name) {
  create_index_postfix_html(filter, register_table_name)
  create_index_prefix_html(filter, register_table_name)
  create_index_header_html(filter, register_table_name)
}

#' Renders register html for a single register_table
#' 
#' @param filter The filter
#' @param register_table The register table
#' @param register_table_name The register table name
render_register_html <- function(filter, register_table, register_table_name){  
  # Add icons to the Repository column for HTML output, use a copy of the register.md
  register_table <- add_repository_links_html(register_table)

  # Dynamically create the index header, prefix and postfix files
  create_index_section_files(filter, register_table_name)
  generate_html_document_yml(filter, register_table_name)

  output_dir <- get_output_dir(filter, register_table_name)
  # Capture the HTML output from a markdown file
  # Note that the temp md file must be created even if a md exists because the register table
  # now has different icons under "Repository" column
  render_register_md(filter, register_table, register_table_name, for_html_file=TRUE)
  temp_md_file_path <- paste0(output_dir, "temp.md")
  
  yaml_path <- normalizePath(file.path(getwd(), paste0(output_dir, "html_document.yml")))

  # Render HTML from markdown
  rmarkdown::render(
    input = temp_md_file_path,
    output_file = "index.html",
    output_dir = output_dir,
    output_yaml = yaml_path
  )

  file.remove(temp_md_file_path)

  # For all registered tables besides the original we change the html
  # file so that the path to the libs folder refers to the libs folder "docs/libs".
  # This is done to remove duplicates of "libs" folders.
  if (register_table_name != "original"){
    html_file_path <- paste0(output_dir, "index.html")
    edit_html_lib_paths(html_file_path)
    # Deleting the libs folder after changing the html lib path
    unlink(paste0(output_dir, "/libs"), recursive = TRUE)
  }
}

#' Renders register htmls for a list of register tables
#' 
#' @param list_register_table List of register tables
render_register_htmls <- function(list_register_tables) {
  # Loop over each register table
  for (filter in names(list_register_tables)){
    for (register_table_name in names(list_register_tables[[filter]])) {
      register_table <- list_register_tables[[filter]][[register_table_name]]
      render_register_html(filter, register_table, register_table_name)
    }
  }
}

#' Loads a html file and replaces the libs path in the html file to the libs folder in "docs/libs"
#' This is done so all html files can share one libs folder. 
#' 
#' @param html_file_path The path to the html file that needs to be edited.
edit_html_lib_paths <- function(html_file_path) {

  path_components <- strsplit(html_file_path, "/")[[1]]
  # The count of dirs one needs to move up to reach "docs" folder. "-2" is used because both "docs"
  # "index.html" are elements in path_components. 
  count_dir_up <- length(path_components) - 2
  up_dirs_string <- rep("../", count_dir_up)

  # Relative path to the "docs/libs" folder
  path_to_base <- paste0(up_dirs_string, collapse = "")
  relative_libs_dir <- paste0(path_to_base, "libs/")

  # Read the HTML file lines into a vector
  html_lines <- readLines(html_file_path)
  
  # Replace lines containing "=libs/" with the appropriate relative path to "docs/libs"
  edited_lines <- gsub('="libs/', paste0('="', relative_libs_dir), html_lines)
  
  # Write the edited lines back to the file
  writeLines(edited_lines, html_file_path)
}