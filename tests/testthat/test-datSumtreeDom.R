
##The `datSumTreeDom` function aggregates tree-level data to plot or condition, including options for filtering tree data or extrapolating to plot acre by multiplying by `TPA`.

test_that("datSumTreeDom Testing", { 
    
  skip_on_cran()

  treedomBA <- datSumTreeDom( ##For Basal Area
    tree = WYtree, 
    cond = WYcond, 
    plt = WYplt, 
    puniqueid = "CN", 
    bycond = FALSE, 
    tsumvar = "BA", 
    TPA = TRUE, 
    tdomtot = TRUE, 
    tdomtotnm = "BA_LIVE", 
    tdomprefix = "BA_LIVE", 
    tround = 2, 
    tfilter = "STATUSCD==1"
  )
  
  treedomVOL <- datSumTreeDom( ##For Volume
    tree = WYtree, 
    cond = WYcond, 
    plt = WYplt, 
    puniqueid = "CN", 
    bycond = FALSE, 
    tsumvar = "VOLCFNET", 
    TPA = TRUE, 
    tdomtot = TRUE, 
    tdomtotnm = "VOLCFNET_LIVE", 
    tdomprefix = "VOLCFNET_LIVE", 
    tround = 2, 
    tfilter = "STATUSCD==1"
  )
  
  datBA_varlut <- treedomBA$tdomvarlut
  
  datVOL_varlut <- treedomVOL$tdomvarlut

  expect_snapshot(datBA_varlut)
  expect_snapshot(datVOL_varlut)
})

