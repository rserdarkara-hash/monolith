library(testthat)

test_that("Moran's I is exposed in UI metrics table", {
  app_file <- "../../app_v2.2.R"
  if(!file.exists(app_file)) skip("app_v2.2.R not found")
  
  content <- readLines(app_file)
  has_moran_ui <- any(grepl("Moran's I", content) | grepl("Moran_I", content))
  
  expect_true(has_moran_ui, info = "The string 'Moran's I' should be present in app_v2.2.R indicating it is exposed in the metrics table.")
})
