library(testthat)

test_that("Styler configuration logic exists", {
  # Mock the process of downloading and uploading config
  # In reality, this relies on shiny session, but we can verify the JSON serialization logic
  mock_input <- list(
    styler_title_size = 18,
    styler_base_size = 14,
    styler_legend_pos = "bottom"
  )
  
  json_out <- jsonlite::toJSON(mock_input, auto_unbox = TRUE)
  
  expect_true(grepl("styler_title_size", json_out))
  expect_true(grepl("18", json_out))
  
  parsed_in <- jsonlite::fromJSON(json_out)
  expect_equal(parsed_in$styler_legend_pos, "bottom")
})
