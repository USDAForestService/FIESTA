
test_that("datSumCond Testing", {
  
  skip_on_cran()

  # Test 1
  condsumdat <- datSumCond(cond = WYcond, csumvar = "LIVE_CANOPY_CVR_PCT")
  condsum <- condsumdat$condsum

  summed_num <- condsum[condsum$PLT_CN == 40405596010690,]$LIVE_CANOPY_CVR_PCT_PLT
  expected_sum <- sum(WYcond[WYcond$PLT_CN == 40405596010690,]$LIVE_CANOPY_CVR_PCT)
  expect_equal(summed_num, expected_sum)

  # Test 2
  condsumdat2 <- datSumCond(cond = WYcond,
                            plt = WYplt,
                            csumvar = "LIVE_CANOPY_CVR_PCT",
                            cfilter = "STDSZCD == 1")

  condsum2 <- condsumdat2$condsum

  summed_num2 <- condsum2[condsum2$CN == 40405596010690,]$LIVE_CANOPY_CVR_PCT_PLT
  nonfiltered_exp <- WYcond[WYcond$PLT_CN == 40405596010690,]
  expected_sum2 <- sum(nonfiltered_exp[nonfiltered_exp$STDSZCD == 1,]$LIVE_CANOPY_CVR_PCT)
  expect_equal(summed_num2, expected_sum2)

  # Test 3
  condsumdat3 <- datSumCond(cond = WYcond,
                            plt = WYplt,
                            csumvar = "CONDPROP_UNADJ",
                            csumvarnm = "cond_nf",
                            cfilter = "COND_STATUS_CD %in% c(2,3)")

  condsum3 <- condsumdat3$condsum

  summed_num3 <- condsum3[condsum3$CN == 40404737010690,]$cond_nf
  nonfiltered_exp3 <- WYcond[WYcond$PLT_CN == 40404737010690,]
  expected_sum3 <- sum(nonfiltered_exp3[nonfiltered_exp3$COND_STATUS_CD %in% c(2,3),]$CONDPROP_UNADJ)
  expect_equal(summed_num3, expected_sum3)

  # Test 4
  condsumdat4 <- datSumCond(cond = WYcond,
                            plt = WYplt,
                            csumvar = "CONDPROP_UNADJ",
                            csumvarnm = "cond_reserved",
                            cfilter = "RESERVCD == 1")

  condsum4 <- condsumdat4$condsum

  summed_num4 <- condsum4[condsum4$CN == 46792188020004,]$cond_reserved
  nonfiltered_exp4 <- WYcond[WYcond$PLT_CN == 46792188020004,]
  expected_sum4 <- sum(nonfiltered_exp4[nonfiltered_exp4$RESERVCD == 1,]$CONDPROP_UNADJ)
  expect_equal(summed_num4, expected_sum4)  
})