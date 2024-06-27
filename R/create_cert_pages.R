create_cert_pages <- function(register, cert_dir = "docs/certs/"){

  # Read template
  html_template_path <- "docs/template_cert_page.html"  # Adjust path as necessary
  html_template <- readLines(html_template_path)

  # Loop over each cert in the register table
  for (cert in list_certs){
    # Download the file
    cert_dir <- "docs/certs/"
    download_cert(report_link, cert_id, cert_dir)

    cert_dir <- paste0(cert_dir, cert_id, "/")
    cert_pdf_path <- paste0(cert_dir, "cert.pdf") 
    
    # Read and convert PDF to PNG images
    image_path <- paste0(cert_dir, "cert.png")
    pdf_convert(cert_pdf_path, format = "png", pages = 1, filenames = image_path, dpi = 300)

    # Creating the image tag
    img_tag <- sprintf('<img src="%s" alt="Description">', image_path)

    # Add the image tag into the placeholder
    html_file_path <- paste0(cert_dir, "index.html")
    html_file <- gsub("<!--placeholder-for-images-->", img_tag, html_template)

    # Write the updated HTML to a file
    writeLines(html_file, html_file_path)
  }
}
