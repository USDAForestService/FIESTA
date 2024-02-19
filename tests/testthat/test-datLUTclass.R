test_that("datLUTclass function works as expected", {

  skip_on_cran()

  # Test Look up Table classification with utils ref table
  testLUT <- datLUTclass(x = WYtree,
                        xvar = "DIA",
                        LUT = FIESTAutils::ref_diacl2in,
                        LUTclassnm = "DIACL2IN")

  testLUT_results <- head(testLUT$xLUT)
  expect_snapshot(testLUT_results)

  # Test Look up table classification with created Dataframe
  diacl25 <- data.frame(MIN = c(5,25), 
                        MAX = c(25, 100), 
                        DIACL25 = c("5.0-24.9", "25.0+"))

  testLUT2 <- datLUTclass(x = WYtree,
                          xvar = "DIA",
                          LUT = diacl25,
                          LUTclassnm = "DIACL25")

  testLUT2_results <- head(testLUT2$xLUT)
  expect_snapshot(testLUT2_results)

  # Test Look up table classification with provided cutbreaks

  cutbreaks <- c(0,25,50,100)
  testLUT3 <- datLUTclass(x = WYcond, 
                          xvar = "LIVE_CANOPY_CVR_PCT",
                          cutbreaks = cutbreaks)

  testLUT3_results <- head(testLUT3$xLUT)
  expect_snapshot(testLUT3_results)

})
