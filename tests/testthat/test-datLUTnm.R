test_that("datLUTnm works as expected", {

  skip_on_cran()

  # Test Look up table classification with Disturbance Type
  ref_dstrbcd <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "DSTRBCD",]

  testLUTnm <- datLUTnm(x = WYcond,
                        xvar = "DSTRBCD1",
                        LUT = ref_dstrbcd,
                        LUTvar = "VALUE",
                        LUTnewvar = "MEANING",
                        LUTnewvarnm = "DSTRB1NM")

  testLUTnm_results <- testLUTnm$xLUT

  # Gets Data where not N/A
  filtered_result <- testLUTnm_results[testLUTnm_results$DSTRBCD1 > 0, ]
  
  expect_equal(filtered_result[1]$DSTRB1NM, "Insect")
})
