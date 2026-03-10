test_that("theme definitions are correctly generated", {
  source("../../theme_helpers.R")

  # Ensure the themes list exists
  expect_true(exists("app_themes"))
  expect_type(app_themes, "list")

  # Expect exactly 10 themes
  expect_equal(length(app_themes), 10)

  # Check a specific theme e.g., "Deep Forest"
  expect_true(!is.null(app_themes[["Deep Forest"]]))
  expect_true(!is.null(app_themes[["Deep Forest"]]$theme))
  expect_true(!is.null(app_themes[["Deep Forest"]]$manual_style))
  expect_true(!is.null(app_themes[["Deep Forest"]]$map_tiles))

  # Ensure map_tiles are among the allowed ones
  allowed_tiles <- c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery")
  
  for (theme_name in names(app_themes)) {
    theme_data <- app_themes[[theme_name]]
    expect_true(theme_data$map_tiles %in% allowed_tiles, 
                info = paste(theme_name, "has invalid map tiles"))
  }
})
