test_that("datLUTspp returns expected results", {
  
  # Test look up table classification of species common name
  testLUT_species <- datLUTspp(WYtree)

  testLUT_results <- testLUT_species$xLUT
  unique_classes <- unique(testLUT_results$COMMON_NAME)
  expect_snapshot(testLUT_results)
  expect_snapshot(unique_classes)
})
