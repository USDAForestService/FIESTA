
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
  test2_subset <- test2[test2$PLT_CN == CN, "113",][[1]]
  test2_subset_char <- as.character(test2_subset)

  input1 <- WYtree[WYtree$PLT_CN == CN, ]
  input2 <- sum(input1[input1$SPCD == 113 & input1$STATUSCD == 1, "VOLCFNET"])
  input3_final <- formatC(input2)

  expect_equal(test2_subset_char, input3_final)
  expect_equal(dim(test1), c(10,4))
  expect_snapshot(test1)
})