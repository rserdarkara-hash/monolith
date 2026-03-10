# Test file for v2.7 export registry (zip export fix)

library(testthat)
library(zip)

test_that("zip library is available and can create an archive", {
  # Create some temp files
  tmp_dir <- tempdir()
  file1 <- file.path(tmp_dir, "test1.txt")
  file2 <- file.path(tmp_dir, "test2.txt")
  writeLines("test 1", file1)
  writeLines("test 2", file2)
  
  zip_file <- file.path(tmp_dir, "test_archive.zip")
  if (file.exists(zip_file)) file.remove(zip_file)
  
  # create zip
  zip::zip(zipfile = zip_file, files = c("test1.txt", "test2.txt"), root = tmp_dir)
  
  expect_true(file.exists(zip_file))
  expect_gt(file.info(zip_file)$size, 0)
  
  # cleanup
  file.remove(file1, file2, zip_file)
})