create_cert_pages <- function(register, cert_dir = "docs/certs/"){

  # Read template
  html_template_path <- "docs/template_cert_page.html"  # Adjust path as necessary
  html_template <- readLines(html_template_path)

  # Loop over each cert in the register table
  for (i in 1:nrow(register)){
    # Retrieving report link and cert id
    report_link <- register[i, ]$Report
    cert_id <- register[i, ]$Certificate

    # Retrieve the abstract
    abstract <- get_abstract(register[i, ]$Repo)

    # Download the file
    cert_dir <- paste0("docs/certs/", cert_id, "/")
    download_cert(report_link, cert_id, cert_dir)

    # Convert the pdf cert to series of jpeg images
    convert_pdf_cert_jpeg(cert_dir)

    img_tag <- sprintf('<img src="%s" alt="Description">', image_path)

    # Add the image tag into the placeholder
    html_file_path <- paste0(cert_dir, "index.html")
    html_file <- gsub("<!--placeholder-for-images-->", img_tag, html_template)

    # Write the updated HTML to a file
    writeLines(html_file, html_file_path)
  }
}

convert_pdf_cert_jpeg <- function(cert_dir){
  # Get the number of pages in the PDF
  cert_pdf_path <- paste0(cert_dir, "cert.pdf")
  pdf_info <- pdf_info(cert_pdf_path)
  num_pages <- pdf_info$pages

  # Create image filenames
  image_filenames <- sapply(1:num_pages, function(page) paste0(cert_dir, "cert_", page, ".png"))
  
  # Read and convert PDF to PNG images
  pdf_convert(cert_pdf_path, format = "png", filenames = image_filenames, dpi = 300)
}
