library(testthat)

test_that("Governing Factors front-end renderers are implemented", {
  # Read the monolith file
  app_code <- readLines("../../monolith_0.8.5.R")
  app_str <- paste(app_code, collapse = "\n")
  
  # Check for renderPlot and renderDataTable functions for Governing Factors
  expect_true(grepl("output\\$gov_plot_importance\\s*<-\\s*renderPlot", app_str), 
              info = "Global Importance Bar Chart renderer should exist.")
              
  expect_true(grepl("output\\$gov_plot_interaction_a\\s*<-\\s*renderPlot", app_str), 
              info = "Interaction Plot A renderer should exist.")
              
  expect_true(grepl("output\\$gov_plot_interaction_b\\s*<-\\s*renderPlot", app_str), 
              info = "Interaction Plot B renderer should exist.")
              
  expect_true(grepl("output\\$gov_plot_effect\\s*<-\\s*renderPlot", app_str), 
              info = "Functional Effect Plot renderer should exist.")
              
  expect_true(grepl("output\\$gov_summary_table\\s*<-\\s*DT::renderDataTable", app_str), 
              info = "Summary Table renderer should exist.")
})