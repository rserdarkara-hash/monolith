# tests/test_docs_existence.R
cat("=== Test: Documentation Files Existence and Content Check ===\n")

files <- c("docs/scientific_guide.md", "docs/user_guide.md", "docs/desc_exploratory_guide.md")

all_exist <- TRUE
for (file in files) {
  if (!file.exists(file)) {
    cat("File not found: ", file, "\n")
    all_exist <- FALSE
  } else {
    file_info <- file.info(file)
    cat("File: ", file, " | Size: ", file_info$size, " bytes\n")
    # Require at least 1000 bytes for all documentation guides to ensure content is present
    if (file_info$size < 1000) {
      cat("Error: File is too small (< 1000 bytes): ", file, "\n")
      all_exist <- FALSE
    }
  }
}

if (all_exist) {
  cat("Documentation files existence check: PASSED\n")
} else {
  cat("Documentation files existence check: FAILED\n")
  quit(status = 1)
}
