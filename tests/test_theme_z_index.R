# tests/test_theme_z_index.R
source("global_0.9.8b.R")

cat("=== Test: Theme z-index Alignment ===\n")

# Check if app_themes is loaded
if (!exists("app_themes") || length(app_themes) == 0) {
  cat("Error: app_themes not loaded or empty.\n")
  quit(status = 1)
}

# Inspect "Muted Sage" theme manual_style
style_str <- app_themes[["Muted Sage"]]$manual_style

# Locate .docs-drawer block using regex
docs_drawer_match <- regmatches(style_str, regexpr("\\.docs-drawer\\s*\\{[^}]*\\}", style_str))

if (length(docs_drawer_match) == 0) {
  cat("FAILED: .docs-drawer class not found in CSS styles.\n")
  quit(status = 1)
}

block_content <- docs_drawer_match[1]
cat("Found .docs-drawer block:\n", block_content, "\n\n")

# Extract z-index property
z_index_match <- regmatches(block_content, regexpr("z-index:\\s*([^;! \n]+)", block_content))

if (length(z_index_match) == 0) {
  cat("FAILED: z-index property not found in .docs-drawer style block.\n")
  quit(status = 1)
}

# Get the value
z_index_val <- gsub("z-index:\\s*", "", z_index_match[1])
cat("Current z-index value of .docs-drawer:", z_index_val, "\n")

if (z_index_val == "2500") {
  cat("Test: PASSED\n")
  quit(status = 0)
} else {
  cat("FAILED: Expected z-index of .docs-drawer to be 2500, but found", z_index_val, "\n")
  quit(status = 1)
}
