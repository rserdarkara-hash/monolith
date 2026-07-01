# tests/test_theme_z_index.R
source("global_0.9.8b.R")

cat("=== Test: Theme z-index Alignment ===\n")

# Check if app_themes is loaded
if (!exists("app_themes") || length(app_themes) == 0) {
  cat("Error: app_themes not loaded or empty.\n")
  quit(status = 1)
}

style_str <- app_themes[["Muted Sage"]]$manual_style
failed <- FALSE

# Helper function to check a CSS class z-index
check_z_index <- function(css_class, expected_val) {
  # Escape dot for regex
  escaped_class <- gsub("\\.", "\\\\.", css_class)
  pattern <- paste0(escaped_class, "\\s*\\{[^}]*\\}")
  class_match <- regmatches(style_str, regexpr(pattern, style_str))
  
  if (length(class_match) == 0) {
    cat(sprintf("FAILED: %s class not found in CSS styles.\n", css_class))
    return(TRUE)
  }
  
  block_content <- class_match[1]
  cat(sprintf("Found %s block:\n%s\n\n", css_class, block_content))
  
  z_index_match <- regmatches(block_content, regexpr("z-index:\\s*([^;! \n]+)", block_content))
  
  if (length(z_index_match) == 0) {
    cat(sprintf("FAILED: z-index property not found in %s style block.\n", css_class))
    return(TRUE)
  }
  
  z_index_val <- gsub("z-index:\\s*", "", z_index_match[1])
  cat(sprintf("Current z-index value of %s: %s\n", css_class, z_index_val))
  
  if (z_index_val == expected_val) {
    cat(sprintf("%s z-index check: PASSED\n", css_class))
    return(FALSE)
  } else {
    cat(sprintf("FAILED: Expected z-index of %s to be %s, but found %s\n", css_class, expected_val, z_index_val))
    return(TRUE)
  }
}

# Assertions
failed <- failed | check_z_index(".docs-drawer", "2500")
failed <- failed | check_z_index(".modal", "2610")
failed <- failed | check_z_index(".modal-backdrop", "2600")

if (failed) {
  cat("=== Result: SOME TESTS FAILED ===\n")
  quit(status = 1)
} else {
  cat("=== Result: ALL TESTS PASSED ===\n")
  quit(status = 0)
}
