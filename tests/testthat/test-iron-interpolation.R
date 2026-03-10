library(testthat)
library(sf)
library(terra)
library(gstat)

test_that("Ordinary Kriging handles small values (Fe-like) correctly", {
  # Create sample points with small values and small variance
  # Fe limits are 4-6, so let's use values in that range
  set.seed(123)
  n <- 50
  pts <- data.frame(
    x = runif(n, 0, 1000),
    y = runif(n, 0, 1000),
    Fe = rnorm(n, mean = 5, sd = 0.5)
  )
  pts_sf <- st_as_sf(pts, coords = c("x", "y"), crs = 32635)
  
  # Variogram fitting
  v_emp <- variogram(Fe ~ 1, pts_sf)
  # Basic auto-fit simulation
  initial_sill <- var(pts$Fe)
  initial_range <- 500
  fit <- fit.variogram(v_emp, vgm(psill = initial_sill, "Sph", range = initial_range, nugget = initial_sill/2))
  
  expect_s3_class(fit, "variogramModel")
  
  # Create a grid for interpolation
  bbox <- st_bbox(pts_sf)
  grid_r <- rast(ext(bbox), res = 50, crs = "EPSG:32635")
  grid_p <- as.points(grid_r, values = FALSE) %>% st_as_sf()
  
  # Interpolation
  res_sf <- krige(Fe ~ 1, pts_sf, grid_p, model = fit, debug.level = 0)
  
  expect_true("var1.pred" %in% colnames(res_sf))
  expect_true(all(!is.na(res_sf$var1.pred)))
  
  # Rasterization
  r <- rasterize(res_sf, grid_r, field = "var1.pred")
  
  expect_s4_class(r, "SpatRaster")
  expect_true(all(!is.na(values(r))))
  
  # Check for "invalid surface" (e.g. constant value or crazy extremes)
  v_res <- values(r, na.rm=TRUE)
  expect_true(sd(v_res) > 0)
  expect_true(all(v_res > 0)) # Fe should be positive
})
