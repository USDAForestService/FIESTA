wwwGetAOI <- function(AOI_table_name,
                      AOI_domain_units,
                      prednames,
                      pgconn,
                      byeach = TRUE,
                      rastfolder = "rasterlayers",
                      minplots = 50) {
  
  ## tasks:
  ## 1. check plot count to determine whether to use sae
  ##    - get largebnd province names
  ##    - get aoiwhere and aoiselect queries
  ## 2. build pltassgn_proj
  ##    - sae
  ##      - use CASE statements to build out domain_unit, AOI columns
  ##    - non-sae
  ##      - build out domain_unit, AOI columns
  ## 3. build unitarea 
  ## 4. build unitzonal
  ##    - sae 
  ##      - compile raster filepaths
  ##      - loop through largebnd provinces and
  ##        - get list of all AOI_domain_units in province
  ##        - find all subsections that intersect with those AOI_domain_units
  ##        - recalculate zonals for those subsections
  ##      - compile all zonals
  ##    - non-sae
  ##      - filter domain_unit_zonal down to domain_units in aoi
  
  
  ## returns:
  ##  - auxdat: list containing unitzonal, unitarea, pltassgn_proj objects
  ##  - prednames: prednames
  ##  - predfac: predfac
  ##  - aoiqueries: list containing aoiselect, aoiwhere aoifrom queries
  ##  - pltidsqry: pltids query
  ##  - SAE: boolean, whether doing SAE or not
  ##  - AOI_table_name: AOI_table_name
  ##  - strvar: default to "lf2022_evt_tree_nontree"
  ##  - pltassgnid: default to "plt_cn"
  ##  - pltidsid: default to "cn"
  
  ## prep ----------------------------------------------------------------------
  
  domain_unit <- DBI::dbGetQuery(pgconn,
                                 paste0("SELECT domain_unit
                                         FROM ref_boundary
                                         WHERE table_name = '", AOI_table_name, "'"))[[1]]
  
  table_name <- AOI_table_name
  
  
  domain_unit.filter <- getfilter("domain_unit", AOI_domain_units, syntax = "sql")
  table_name.filter <- getfilter("table_name", table_name, syntax = "sql")
  
  strvar <- "lf2022_evt_tree_nontree"

  
  ## 1. check number of plots to determine whether to use SAE ------------------
  
  
  pltchk <- wwwCheckPlots(popType = "VOL",
                          popFilter = popFilters(evalCur = TRUE),
                          byeach = byeach,
                          AOI_table_name = AOI_table_name,
                          AOI_domain_unit_values = AOI_domain_units,
                          minplots = minplots,
                          dbconn = pgconn,
                          pltassgnnm = "plot_assign_plot",
                          plotnm = NULL)
  
  
  
  plotcnt <- pltchk$plotcnt
  pwhereqry <- pltchk$pwhereqry
  pltafromqry <- pltchk$pltafromqry
  plotcntqry <- pltchk$plotcntqry
  pfilter <- pltchk$pfilter
  pltadata <- pltchk$pltadata
  states <- pltchk$states
  invyrs <- pltchk$invyrs
  SAE <- pltchk$SAE
  province_max <- pltchk$province_max
  largebnd.provinces <- unique(province_max$ecomap_province)
  aoiselect.qry <- pltchk$aoiselectqry
  aoiwhere.qry <- pltchk$aoiwhereqry
  prov_dom_lut <- province_max
  
  ## 2. build pltassgn_proj ------------------------------------------------------

  # add popwhereqry to pltassgn

  pltidsid <- "CN"
  pltidvars <- c(pltidsid)
  pltidvars <- paste0("plta.", pltidvars)
  prednm.filter <- getfilter("display_name", prednames, syntax = "sql")
  pred_shortnames <- DBI::dbGetQuery(pgconn,
                                     paste0("SELECT short_name
                                             FROM ref_auxiliary
                                             WHERE ", prednm.filter))[[1]]

  if (SAE) {
    
    pltassgn.qry <- paste0("SELECT ", toString(c(pltidvars, "ecomap_province")), ", ",
                           aoiselect.qry, ", ",
                           "\n     CASE WHEN ", pfilter, 
                           "\n          THEN 1 ELSE 0 END AS aoi, ",
                           "\n     ", toString(pred_shortnames),
                           pltafromqry,
                           aoiwhere.qry)
    
    pltids.qry <- paste0("SELECT ", toString(pltidvars), ", ",
                         aoiselect.qry,
                         pltafromqry,
                         aoiwhere.qry)
    
  } else {
    
    pltassgn.qry <- paste0("SELECT ", toString(pltidvars), ", ",
                           aoiselect.qry, ", ",
                           "\n     1 AS aoi, ",
                           "\n     ", toString(pred_shortnames),
                           pltafromqry,
                           pwhereqry)
    
    pltids.qry <- paste0("SELECT ", toString(pltidvars), ", ", 
                         aoiselect.qry, ", ",
                         "\n     ", strvar,
                         pltafromqry,
                         pwhereqry)
                        
  }
  
  pltassgn_proj <- DBI::dbGetQuery(pgconn, pltassgn.qry)
  
  
  ## 3. get unitarea -----------------------------------------------------------
  
  unitarea_cols <- c("table_name", "domain_unit", "total_acres")
  
  if (SAE) {
    
    # just need for aoi or also for helpers?
    zonal_helper.filter <- getfilter("domain_unit", largebnd.provinces, syntax = "sql", like = TRUE)

    unitarea_aoi.qry <- paste0("SELECT ", toString(unitarea_cols),
                               "\nFROM domain_unit_zonal",
                               "\nWHERE ", table_name.filter,
                               "\nAND ", domain_unit.filter,
                               " OR (table_name = 'ecomap_subsection' AND (", zonal_helper.filter, "))")
                              
  } else {
    
    unitarea_aoi.qry <- paste0("SELECT ", toString(unitarea_cols),
                               "\n FROM domain_unit_zonal",
                               "\n WHERE ", table_name.filter,
                               "\n AND ", domain_unit.filter)
    
  }
  
  unitarea <- DBI::dbGetQuery(pgconn, unitarea_aoi.qry)
  
  # if not byeach collapse down
  if (!byeach) {
    if (SAE) {
      # collapse values for AOI?
      cmb_area_aoi <- colSums(unitarea[unitarea$domain_unit %in% AOI_domain_units, "total_acres", drop = FALSE])
      names(cmb_area_aoi) <- NULL
      tmp <- data.frame(table_name = table_name,
                        domain_unit = 1,
                        total_acres = cmb_area_aoi)
      unitarea <- unitarea[!(unitarea$domain_unit %in% AOI_domain_units), , drop = FALSE ] |>
        rbind(tmp)
    }
    else {
      cmb_area <- colSums(unitarea[ , "total_acres", drop = FALSE])
      names(cmb_area) <- NULL
      tmp <- data.frame(table_name = table_name,
                        oneunit = 1,
                        total_acres = cmb_area)
      unitarea <- tmp
    } 
  }
  

  ## 4. get unitzonal ----------------------------------------------------------
  
  # need to likely adjust SA module in FIESTA to filter unitlut down to the appropriate domains
  # SAest.large line 1089
  
  if (SAE) {

    rastlst.cont <- rastlst.cont.name <- NULL
    rastlst.cat <- rastlst.cat.name <- NULL

    aux_layer_filter <- getfilter("short_name", pred_shortnames, syntax = "sql", like = FALSE)
    
    aux_cont_qry <- paste0("SELECT short_name, layer_name, type",
                           "\nFROM ref_auxiliary",
                           "\nWHERE (", aux_layer_filter, ") AND type = 'Continuous'")
    
    aux_cat_qry <- paste0("SELECT short_name, layer_name, type",
                           "\nFROM ref_auxiliary",
                           "\nWHERE (", aux_layer_filter, ") AND type = 'Categorical'")
    
    aux_cont <- DBI::dbGetQuery(pgconn, aux_cont_qry)
    aux_cat <- DBI::dbGetQuery(pgconn, aux_cat_qry)
    
    if (nrow(aux_cont) != 0) {
      rastlst.cont <- paste0(rastfolder, "/", aux_cont$layer_name)
      rastlst.cont.name <- aux_cont$short_name
    } else {
      rastlst.cont <- NULL
      rastlst.cont.name <- NULL
    }
    
    if (nrow(aux_cat) != 0) {
      rastlst.cat <- paste0(rastfolder, "/", aux_cat$layer_name)
      rastlst.cat.name <- aux_cat$short_name
      dunitzonal_nms <- DBI::dbGetQuery(pgconn,
                                        "SELECT column_name
                                         FROM information_schema.columns
                                         WHERE table_name = 'domain_unit_zonal'")
      # cat_nms.filter <- getfilter('dunitzonal_nms', rastlst.cat.name, syntax = 'R', like = TRUE)
      
      # dunitzonal_nms <- DBI::dbGetQuery(pgconn,
      #                                   "SELECT name
      #                                    FROM pragma_table_info('domain_unit_zonal')")

      cat_nms.filter <- getfilter('column_name', rastlst.cat.name, syntax = 'R', like = TRUE)
      zonal_cat_cols <- subset(dunitzonal_nms, eval(parse(text = cat_nms.filter)))[["column_name"]]
      zonal_cat_cols_sqlfmt <-  paste0("\"",zonal_cat_cols, "\"")

    } else {
      rastlst.cat <- NULL
      rastlst.cat.name <- NULL
      zonal_cat_cols <- NULL
      zonal_cat_cols_sqlfmt <- NULL
    }
    
  }  
  
  unitzonal_cols <- c("table_name", "domain_unit", "pixel_count",
                      c(rastlst.cont.name,  zonal_cat_cols_sqlfmt))
  unitzonal_cols <- toString(unitzonal_cols)
  zonal_prednames <- c("pixel_count", rastlst.cont.name, zonal_cat_cols)
  
  
  if (SAE) {
    
    unitzonal_aoi.qry <- paste0("SELECT ", unitzonal_cols,
                                "\nFROM domain_unit_zonal",
                                "\nWHERE ", table_name.filter,
                                "\nAND ", domain_unit.filter,
                                " OR (table_name = 'ecomap_subsection' AND (", zonal_helper.filter, "))")
    
  } else {
    
    unitzonal_aoi.qry <- paste0("SELECT ", unitzonal_cols, 
                                "\nFROM domain_unit_zonal",
                                "\nWHERE ", table_name.filter,
                                "\nAND ", domain_unit.filter)
    
  }
  
  unitzonal_temp <- DBI::dbGetQuery(pgconn, unitzonal_aoi.qry)
  
  unitzonal_temp <- merge(unitzonal_temp, province_max[ ,c("domain_unit", "ecomap_province")],
                          on = "domain_unit",
                          all.x = TRUE)
  
  unitzonal_temp[is.na(unitzonal_temp$ecomap_province), "ecomap_province"] <- 
    sub("(?<=\\d)([a-zA-Z]+)", "", 
        unitzonal_temp[is.na(unitzonal_temp$ecomap_province), "domain_unit"],
        perl = TRUE)

  
  if (SAE) {
    
    # loop through **provinces** recalculating zonals within each
    for (i in seq_along(largebnd.provinces)) {
      
      prov.filter <- getfilter("domain_unit", largebnd.provinces[i], syntax = "sql", like = TRUE)
      
      helpers_lst.qry <- paste0("SELECT DISTINCT domain_unit",
                                "\nFROM domain_unit_zonal",
                                "\nWHERE table_name = 'ecomap_subsection' AND ",
                                prov.filter)
      
      helpers_list <- DBI::dbGetQuery(pgconn, helpers_lst.qry)[[1]]
      helpers.filter <- getfilter("map_unit_symbol", helpers_list, syntax = "sql")
      
      prov_domain_units <- prov_dom_lut[prov_dom_lut$ecomap_province == largebnd.provinces[i],
                                        "domain_unit", drop = TRUE]
      aoi.filter <- getfilter(domain_unit, prov_domain_units, syntax = "sql")
      
      # could this be optimized?
      intersecting_subsec.qry <- paste0("WITH", 
                                        "\n filtered_subsec AS", 
                                        "\n (SELECT *",
                                        "\n  FROM ecomap_subsections",
                                        "\n  WHERE ", helpers.filter, "), ",
                                        "\n layer_filt AS", 
                                        "\n (SELECT *",
                                        "\n  FROM ", table_name,
                                        "\n  WHERE ", aoi.filter, ") ",
                                        "\n SELECT map_unit_symbol",
                                        "\n FROM filtered_subsec AS s",
                                        "\n JOIN layer_filt ON ST_Intersects(s.geom, layer_filt.geom)")
      
      intersecting_subsecs <- DBI::dbGetQuery(pgconn, intersecting_subsec.qry)[[1]]
      intersec_subsecs.filter <- getfilter("map_unit_symbol", intersecting_subsecs, syntax = 'sql')
      
      # load intersecting ecosubsections
      helpersx <- sf::st_read(pgconn,
                              query = paste0("SELECT * ",
                                             "FROM ecomap_subsections ",
                                             "WHERE ", intersec_subsecs.filter))
      
      # load layer aoi
      lyrx <- sf::st_read(pgconn,
                          query = paste0("SELECT * ",
                                         "FROM ", table_name, " ",
                                         "WHERE ", aoi.filter))
      
      union_tmp <- FIESTA::spUnionPoly(helpersx, polyv2 = lyrx)
      
      union_tmp$domain_unit <- union_tmp$map_unit_symbol
      union_tmp[!is.na(union_tmp[[domain_unit]]), "domain_unit"] <- 
        sf::st_drop_geometry(union_tmp[!is.na(union_tmp[[domain_unit]]), domain_unit])
      
      union_tmp <- union_tmp[union_tmp$domain_unit %in% intersecting_subsecs,
                             c(domain_unit, "map_unit_symbol", "domain_unit")]
      
      zonaldat <- 
        FIESTA::spGetAuxiliary(unit_layer = union_tmp,
                               unitvar = "domain_unit",
                               rastlst.cont = rastlst.cont,
                               rastlst.cont.name = rastlst.cont.name,
                               rastlst.cat = rastlst.cat,
                               rastlst.cat.name = rastlst.cat.name,
                               extract = FALSE,
                               ncores = 1)
      
      
      
      colnames(zonaldat$unitzonal)[colnames(zonaldat$unitzonal) == "npixels"] <- "pixel_count"
      unitzonal_new <- zonaldat$unitzonal
      unitzonal_new$ecomap_province <- 
        sub("(?<=\\d)([a-zA-Z]+)", "", 
            unitzonal_new[, "domain_unit"],
            perl = TRUE)
      
      
      unitzonal_old <- unitzonal_temp[!unitzonal_temp$domain_unit %in% intersecting_subsecs, ]
      unitzonal_temp <- rbind(unitzonal_old, data.frame(table_name = "ecomap_subsections", unitzonal_new))
      
    }
    
    unitzonal <- unitzonal_temp
  
     
  } else {
    
    unitzonal <- unitzonal_temp
    
  }

  
  tmp <- data.frame(table_name = table_name,
                    domain_unit = 1)
  .prs <- zonal_prednames[zonal_prednames != "pixel_count"]
  
  if (!byeach) {
    
    if (SAE) {
      
      # domain means -> domain totals -> total of all domains combined -> mean of all domains combined
      # in SAE case, just do this for AOIs. not helper ecosubsections
      zonals_aoi <- unitzonal[unitzonal$domain_unit %in% AOI_domain_units, zonal_prednames, drop = FALSE]
      for (pr in .prs) {
        zonals_aoi[[pr]] <- zonals_aoi[[pr]] * zonals_aoi$pixel_count
      }
      
      cmb_zonals_aoi <- colSums(zonals_aoi)
      cmb_zonals_aoi <- data.frame(t(cmb_zonals_aoi))
      
      for (pr in .prs) {
        cmb_zonals_aoi[[pr]] <- cmb_zonals_aoi[[pr]] / cmb_zonals_aoi$pixel_count
      }
      
      unitzonal <- unitzonal[!(unitzonal$domain_unit %in% AOI_domain_units), , drop = FALSE] |>
        rbind(cbind(tmp, cmb_zonals_aoi))
      
    } else {
      
      for (pr in .prs) {
        unitzonal[[pr]] <- unitzonal[[pr]] * unitzonal$pixel_count
      }
      
      cmb_zonals <- colSums(unitzonal[ , zonal_prednames, drop = FALSE])
      cmb_zonals <- data.frame(t(cmb_zonals))
      
      for (pr in .prs) {
        cmb_zonals[[pr]] <- cmb_zonals[[pr]] / cmb_zonals$pixel_count
      }
      
      unitzonal <- cbind(tmp, cmb_zonals)
      
    }
    
  }
  
  auxdat <- list("pltassgn" = pltassgn_proj,
                 "unitarea" = unitarea,
                 "unitzonal" = unitzonal)
  
  aoiqueries <- list(aoiselect = aoiselect.qry,
                     aoifromqry = pltafromqry,
                     aoiwhereqry = aoiwhere.qry)

  out <- list(auxdata = auxdat,
              prednames = pred_shortnames,
              predfac = rastlst.cat.name,
              aoiqueries = aoiqueries,
              pltidsqry = pltids.qry,
              SAE = SAE,
              AOI_table_name = AOI_table_name,
              strvar = "lf2022_evt_tree_nontree",
              pltassgnid = "CN",
              pltidsid = pltidsid)
  
  return(out)
  
}














