
# Pivot table summarizes and analyzes large quantities of data
test_that("datPivot Testing", {
  # Using datPivot - from vignette
  test1 <- datPivot(x = WYtree,
                    pvar = "HT", ##Height
                    xvar = "SPGRPCD", ##Species Group Code
                    yvar = "TREECLCD", ##Tree Class Code
                    pfun = mean) ##Name of function to use for pivot values

  # Using datPivot - from Tracy's Example
  test2 <- datPivot(x = WYtree,
                   pvar = "VOLCFNET",
                   xvar = "PLT_CN",
                   yvar = "SPCD",
                   xfilter = "STATUSCD == 1")

  CN <- 40404730010690
  test2_subset <- round(test2[test2$PLT_CN == CN, "113",][[1]], 2)

  input1 <- WYtree[WYtree$PLT_CN == CN, ]
  input2 <- round(sum(input1[input1$SPCD == 113 & input1$STATUSCD == 1, "VOLCFNET"]), 2)

  expect_equal(test2_subset, input2) ##expecting Tracys example code aligns with WYtree data
  expect_equal(dim(test1), c(10,4)) ##testing dimensions of output
  expect_snapshot(test1) ##Capturing snapshot
  expect_snapshot(test2)
})
