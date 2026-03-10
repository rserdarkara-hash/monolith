library(testthat)
library(sf)

test_that("Bounding box extraction by locality works correctly", {
  # 1. Create dummy multi-locality data
  df <- data.frame(
    locality = c(rep("LocA", 5), rep("LocB", 5)),
    x = c(10, 11, 12, 10, 11, 50, 51, 52, 50, 51),
    y = c(10, 11, 12, 12, 10, 50, 51, 52, 52, 50)
  )
  # Use a standard CRS
  pts_sf <- st_as_sf(df, coords = c("x", "y"), crs = 4326)
  
  # Function simulation: Extract bounds for a specific locality
  get_loc_bounds <- function(sf_data, loc_name) {
    if (loc_name == "global") {
      return(st_bbox(sf_data))
    } else {
      sub_pts <- sf_data[sf_data$locality == loc_name, ]
      return(st_bbox(sub_pts))
    }
  }
  
  # Test LocA bounds
  bbox_a <- get_loc_bounds(pts_sf, "LocA")
  expect_equal(as.numeric(bbox_a$xmin), 10)
  expect_equal(as.numeric(bbox_a$xmax), 12)
  expect_equal(as.numeric(bbox_a$ymin), 10)
  expect_equal(as.numeric(bbox_a$ymax), 12)
  
  # Test LocB bounds
  bbox_b <- get_loc_bounds(pts_sf, "LocB")
  expect_equal(as.numeric(bbox_b$xmin), 50)
  expect_equal(as.numeric(bbox_b$xmax), 52)
  
  # Test Global bounds
  bbox_g <- get_loc_bounds(pts_sf, "global")
  expect_equal(as.numeric(bbox_g$xmin), 10)
  expect_equal(as.numeric(bbox_g$xmax), 52)
})