library(testthat)

# This test checks for Moran's I sparse matrix implementation in spatial_helpers_v1.9.R

test_that("Moran's I: Should use sparse matrix implementation", {
  helpers_file <- "../../improvements/spatial_helpers_v1.9.R"
  
  if(!file.exists(helpers_file)) {
    expect_true(FALSE, info = "The v1.9 helpers file does not exist.")
    return()
  }
  
  helpers_content <- readLines(helpers_file)
  moran_lines <- grep("calc_moran <- function", helpers_content)
  
  # Search for sparse matrix keywords (spdep for Moran's I)
  has_sparse <- any(grep("spdep::", helpers_content, fixed = TRUE))
  
  # Check if fixed
  expect_true(has_sparse, info = "The Moran's I calculation still uses slow dense matrix allocation (not using spdep).")
})
