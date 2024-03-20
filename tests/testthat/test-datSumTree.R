

test_that("datSumTree Aboveground Biomass w/out Extrapolation", {
  
  skip_on_cran()

  # Using datSumTree - nonTPA  
  test1 <- datSumTree(tree = FIESTA::WYtree, ##
                      tsumvarlst = "DRYBIO_AG", ##tsumvarlst == tree-level variables to aggregate: in this case it is aboveground biomass
                      lbs2tons = FALSE, ##if TRUE converts biomass or carbon variables from lbs to tons
                      TPA = FALSE, ##Trees per acre - 
                      tfilter = "STATUSCD == 1") ## Set to alive trees

  treedat_nonTPA <- test1$treedat
  # Using datSumTree for one plot
  CN <- 40404730010690 ## Randomly selecting one plot ? How to best go about selecting this?
  treedat_nonTPA[treedat_nonTPA$PLT_CN == CN,]
  treedat_nonTPA[treedat_nonTPA$PLT_CN == CN,][[2]]#Selecting a single plot within the treedat df, selecting second variable which is the 

  # Using WYplot data
  input1 <- WYtree[WYtree$PLT_CN == CN, ] 
  sum(input1[input1$STATUSCD == 1, "DRYBIO_AG"], na.rm = TRUE)

  # Expect datSumTree & WYplot data match  
  expect_equal(sum(input1[input1$STATUSCD == 1, "DRYBIO_AG"], na.rm=TRUE), treedat_nonTPA[treedat_nonTPA$PLT_CN == CN, ][[2]])
  #expect_snapshot(treedat_nonTPA)

  # Using datSumTree - TPA
  test2 <- datSumTree(WYtree, 
                      tsumvarlst = "DRYBIO_AG", 
                      lbs2tons = FALSE,
                      TPA = TRUE, 
                      tfilter = "STATUSCD == 1")

  treedat_TPA <- test2$treedat

  # Test output for 1 plot
  CN <- 40404730010690
  treedat_TPA[treedat_TPA$PLT_CN == CN, ] [[2]]
  input1 <- WYtree[WYtree$PLT_CN == CN & WYtree$STATUSCD == 1, ]

  output1 <- with(input1, sum(DRYBIO_AG * TPA_UNADJ, na.rm=TRUE))

  # Expecting same output when summed for TPA
  expect_equal(output1, treedat_TPA[treedat_TPA$PLT_CN == CN, ][[2]])
  #expect_snapshot(treedat_TPA)
})
