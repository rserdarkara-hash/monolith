test_that("theme map tiles are valid leaflet providers", {
  source("../../theme_helpers.R")
  
  allowed_tiles <- c("CartoDB.DarkMatter", "CartoDB.Positron", "Esri.WorldImagery")
  
  for (theme_name in names(app_themes)) {
    theme_data <- app_themes[[theme_name]]
    expect_true(theme_data$map_tiles %in% allowed_tiles, 
                info = paste(theme_name, "has invalid map tiles"))
  }
})