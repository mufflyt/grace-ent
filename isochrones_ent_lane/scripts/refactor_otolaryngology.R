library(stringr)
library(here)

# Find all R scripts
r_files <- list.files(here(), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

# Ignore renv, artifacts, data, etc.
r_files <- r_files[!grepl("/renv/|/artifacts/|/data/|/data-raw/|/cache/", r_files)]

files_modified <- 0

for (f in r_files) {
  content <- readLines(f, warn = FALSE)
  original_content <- content
  
  # Replace "Otolaryngology" with "Otolaryngology"
  content <- str_replace_all(content, "Otolaryngology", "Otolaryngology")
  
  # Replace "Otolaryngology" with "Otolaryngology"
  content <- str_replace_all(content, "Otolaryngology", "Otolaryngology")
  
  # Replace "Otolaryngology" with "Otolaryngology" (just in case)
  content <- str_replace_all(content, "Otolaryngology", "Otolaryngology")

  # Replace "otolaryngology" with "otolaryngology"
  content <- str_replace_all(content, "otolaryngology", "otolaryngology")
  
  if (!identical(original_content, content)) {
    writeLines(content, f)
    files_modified <- files_modified + 1
    cat("Modified:", f, "\n")
  }
}

cat("Total files modified:", files_modified, "\n")
