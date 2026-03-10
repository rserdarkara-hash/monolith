library(testthat)
library(terra)

# Mocking the joint scale logic we intend to implement
get_joint_scale_values <- function(r1, r2, match_scales, is_uncertainty) {
  # This is a placeholder for the logic we will add to spatial_helpers
  if(match_scales && !is.null(r1) && !is.null(r2) && !is_uncertainty) {
     v1 <- as.vector(values(r1, na.rm=TRUE))
     v2 <- as.vector(values(r2, na.rm=TRUE))
     return(c(v1, v2))
  }
  return(NULL)
}

test_that("get_joint_scale_values correctly combines values", {
  r1 <- rast(matrix(1:10, 2, 5))
  r2 <- rast(matrix(11:20, 2, 5))
  
  # When match_scales is TRUE
  joint <- get_joint_scale_values(r1, r2, TRUE, FALSE)
  expect_equal(length(joint), 20)
  expect_equal(range(joint), c(1, 20))
  
  # When match_scales is FALSE
  joint_none <- get_joint_scale_values(r1, r2, FALSE, FALSE)
  expect_null(joint_none)
  
  # When it's uncertainty
  joint_uncert <- get_joint_scale_values(r1, r2, TRUE, TRUE)
  expect_null(joint_uncert)
})
