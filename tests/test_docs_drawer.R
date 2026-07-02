# tests/test_docs_drawer.R
library(shiny)

# Source the file where render_docs_drawer is defined
source("ui_helpers_0.9.8b.R")

cat("=== Test: Documentation Drawer Tabs Structure ===\n")

# Call the function
drawer_ui <- render_docs_drawer()

# Get the tabbable div (second child of the main div)
tabbable_div <- drawer_ui$children[[2]]

# Get the ul tag (first child of the tabbable div)
ul_tag <- tabbable_div$children[[1]]

# Get the list items
li_tags <- ul_tag$children

# Extract data-value from the 'a' tag in each 'li'
tab_titles <- sapply(li_tags, function(li) {
  a_tag <- li$children[[1]]
  if (!is.null(a_tag$attribs) && !is.null(a_tag$attribs$`data-value`)) {
    return(a_tag$attribs$`data-value`)
  }
  return(NULL)
})

tab_titles <- unlist(tab_titles)
cat("Detected tab titles: ", paste(tab_titles, collapse = ", "), "\n")

expected_tabs <- c("Scientific Guide", "User Guide", "Descriptive & Exploratory Suite")

# Check if the tabs match exactly
match_all <- length(tab_titles) == length(expected_tabs) && all(tab_titles == expected_tabs)

if (match_all) {
  cat("Drawer tabs check: PASSED\n")
} else {
  cat("Drawer tabs check: FAILED (Expected: ", paste(expected_tabs, collapse = ", "), "; Got: ", paste(tab_titles, collapse = ", "), ")\n")
  quit(status = 1)
}
