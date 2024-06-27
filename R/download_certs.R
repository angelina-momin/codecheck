library(httr)
library(jsonlite)

download_cert <- function(report_link, cert_id, cert_dir = "docs/certs/"){
  # Obtaining the pdf download link from the report link
  if (grepl("zenodo", report_link)){
    cert_download_url <- get_zenodo_cert_link(report_link, cert_id) 
  }

  else if (grepl("OSF", report_link, ignore.case = FALSE)) {
    cert_download_url <- get_osf_cert_link(report_link, cert_id)
  }

  # Adding the cert id as a folder name. 
  cert_dir <- paste0(cert_dir, cert_id, "/")
  # Checking if the certs dir exist
  if (!dir.exists(cert_dir)){
    dir.create(cert_dir)
  }

  pdf_path <- paste0(cert_dir, "cert.pdf") 

  # Download the PDF file
  download_response <- GET(cert_download_url, write_disk(pdf_path, overwrite = TRUE))

  if (status_code(download_response) == 200) {
      message(paste("Cert", cert_id, "downloaded successfully"))
  } 

  else{
      warning(paste("Failed to download the file. No link will be added to the cert", cert_id, ". No link will 
      be added to this cert."))
  }
}

get_osf_cert_link <- function(report_link, cert_id){
  # Set the base URL for the OSF API
  base_url <- "https://api.osf.io/v2/"

  # Retrieve the osf_project_id 
  node_id <- basename(report_link)

  # Prepare the API endpoint to access files for a specific node
  files_url <- paste0(base_url, "nodes/", node_id, "/files/osfstorage/")

  # Make the API request
  response <- GET(files_url)

  # Check if the request was successful
  if (status_code(response) == 200) {
      # Manually parse the JSON content
      file_data <- fromJSON(content(response, "text", encoding = "UTF-8"))    
      
      # Accessing the files and finding the correct pdf file based on the extension and the file name
      files_list <- file_data$data

      target_files <- files_list[grepl("\\.pdf$", files_list$attributes$name, ignore.case = TRUE), ]
      
      if (length(nrow(target_files)) > 1){
          warning(paste("Multiple pdf files found in the OSF url. Cannot determine the correct OSF cert file to download. No link will be added to cert", cert_id))
      }

      target_file <- target_files[1, ]
      return (target_file$links$download)
  }

  else {
      warning(paste("Could not access the OSF API. Skipping retrieving cert", cert_id))
  }
}

get_zenodo_cert_link <- function(report_link, cert_id, api_key = "") {
  # Set the base URL for the Zenodo API
  base_url <- "https://zenodo.org/api/records/"
  record_id <- gsub("zenodo.", "", basename(report_link))
  record_url <- paste0(base_url, record_id, "/files")
  
  # Make the API request
  response <- GET(record_url, add_headers(Authorization = paste("Bearer", api_key)))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
   # Parse the response
    record_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    files_list <- record_data$entries

    # Check for files in the record
    if (!is.null(files_list)) {
      target_files <- files_list[grepl("\\.pdf$", files_list$key, ignore.case = TRUE), ]
      
      if (length(nrow(target_files)) > 1){
        warning(paste("Multiple pdf files found in the Zenodo url. Cannot determine the correct Zenodo cert file to download. No link will be added to cert", cert_id))
      }

      target_file <- target_files[1, ]
      return (target_file$links$content)
    } 
  } 
  else {
    warning(paste("Could not access the Zenodo API. Skipping retrieving cert", cert_id))
  }
}
