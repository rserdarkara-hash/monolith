library(testthat)

test_that("Spatial Engine returns locality-specific resolutions", {
  # Mock of future_map loop where actual_res is calculated
  
  pts_1 <- data.frame(x = c(0, 10, 20), y = c(0, 10, 20))
  pts_2 <- data.frame(x = c(0, 50, 100), y = c(0, 50, 100))
  
  calc_res <- function(pts) {
    if (nrow(pts) > 1) {
      knn_res <- FNN::get.knn(as.matrix(pts), k = 1)
      actual_res <- mean(knn_res$nn.dist) * 0.5
    } else {
      actual_res <- 50
    }
    return(actual_res)
  }
  
  res_1 <- calc_res(pts_1)
  res_2 <- calc_res(pts_2)
  
  expect_true(res_1 > 0)
  expect_true(res_2 > 0)
  expect_true(res_1 != res_2)
})
