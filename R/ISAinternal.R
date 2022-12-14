## helper.select
## FIESTA_SAmod_demo_plots - Gretchen function

helper.select <- function(smallbndx, smallbnd.unique, smallbnd.domain=NULL,
		helperbndx, helperbnd.unique, largebndx, largebnd.unique,
		maxbndx=NULL, maxbnd.unique=NULL, nbrdom.min=NULL, maxislarge=FALSE,
		largeishelper=FALSE, polyunion=TRUE, showsteps=TRUE, savesteps=FALSE,
		stepfolder=NULL, out_fmt="shp", step_dsn=NULL, step.cex=0.8,
		maxbnd.threshold=0, largebnd.threshold=30, multiSAdoms=TRUE,
		byeach=FALSE, maxbnd.addtext=TRUE, largebnd.addtext=FALSE, 
		overwrite=TRUE) {
  ## DESCRIPTION:: Automate selection of helperbnd polygons for modeling.
  ## maxbnd - maximum constraint for modeling extent (e.g., Province)
  ## if maxbnd, maxbnd is intersected with smallbnd and dissolved by maxbnd.unique (*maxbnd_select)
  ## if (smallbnd intersects more than 1 maxbnd by greater than the set threshold...
  ## 		if multiSAdoms=TRUE, more than 1 SAdoms is returned as a list.

  ## global parameters
  stepcnt <- 1
  maxbndxlst <-{}
  int.pct=helperbndx.tmp <- NULL

  mbndlst <- list()
  #sbndlst <- list(smallbndx)
  SAdomslst <- list()

  if (showsteps) {
    ## Set par
    mar <-  graphics::par("mar")
    on.exit(graphics::par(mar=mar))
    par(mar=c(1,1,1,1))
  }

  ############################################################################
  ## maxbnd
  ############################################################################
  if (!is.null(maxbndx)) {
    message("dissolving maxbnd polygons...")
    maxbndxd <- sf_dissolve(maxbndx, maxbnd.unique, areacalc=FALSE)

    ## get intersection of maxbndx and smallbndx to select intersecting maxbnds
    ## Note: use tabulateIntersections with fewer smallbnd polygons - faster
#    if (nrow(smallbndx) < 200) {
      maxbndx_intersect <-
		suppressWarnings(tabulateIntersections(layer1=smallbndx,
		        layer1fld=smallbnd.unique, layer2=maxbndxd, layer2fld=maxbnd.unique))
      maxbndx_intersect <- maxbndx_intersect[!is.na(maxbndx_intersect$int.pct),
            c(maxbnd.unique, smallbnd.unique, "int.pct")]
      maxbndxlst <- unique(maxbndx_intersect[[maxbnd.unique]])
      maxbndxd.int <- maxbndxd[maxbndxd[[maxbnd.unique]] %in% maxbndxlst, ]
#    } else {
#      maxbndxd.int <- maxbndxd[unique(unlist(sf::st_intersects(smallbndx, maxbndxd))), ]
#      maxbndxlst <- maxbndxd.int[[maxbnd.unique]]
#    }
    message("smallbnd intersected ", length(maxbndxlst), " ", maxbnd.unique, " polygons...)")
 
    if (length(maxbndxlst) < length(unique(maxbndxd[[maxbnd.unique]]))) {
      if (length(unique(smallbndx[[smallbnd.unique]])) > nrow(smallbndx)) {
        smallbndx$DISSOLVE <- 1
        smallbndxd <- sf_dissolve(smallbndx, "DISSOLVE", areacalc=FALSE)
        smallbndx$DISSOLVE <- NULL
      } else {
        smallbndxd <- smallbndx
        smallbndxd$DISSOLVE <- 1
      } 
         
      ## get intersection of maxbndx and smallbndx (dissolved as 1 polygon)
      maxbndx_intersect <-
	 	suppressWarnings(tabulateIntersections(layer1=smallbndxd,
	 	        layer1fld="DISSOLVE", layer2=maxbndxd.int, layer2fld=maxbnd.unique))

      ## Get the maximum overlap by province for smallbndx (dissolved as 1 polygon)
      maxbndx.pct <- maxbndx_intersect[order(maxbndx_intersect$int.pct, decreasing=TRUE), ]
 
      maxbnd.gtthres <- unique(maxbndx.pct[[maxbnd.unique]][maxbndx.pct$int.pct >= maxbnd.threshold])
      maxbnd.ltthres <- unique(maxbndx.pct[[maxbnd.unique]][maxbndx.pct$int.pct < maxbnd.threshold])
      maxbndxlst <- unique(maxbndx.pct[[maxbnd.unique]])

      ## Remove water from list if it exists
      if (any(maxbndxlst == "Water")) {
        maxbndxlst <- maxbndxlst[maxbndxlst != "Water"]
        maxbndx_intersect <- maxbndx_intersect[maxbndx_intersect[[maxbnd.unique]] != "Water",]
      }

      ## Subset maxbndxd to only maxbnd.unique that intersects with smallbnd
      maxbndx.intd <- maxbndxd[maxbndxd[[maxbnd.unique]] %in% maxbndxlst,]

      ## Now, get intersection of maxbndx and smallbndx, by smallbnd.unique
      maxbndx_intersect <-
		suppressWarnings(tabulateIntersections(layer1=smallbndx,
		        layer1fld=smallbnd.unique, layer2=maxbndx.intd, layer2fld=maxbnd.unique))
      maxbndx_intersect <- maxbndx_intersect[!is.na(maxbndx_intersect$int.pct),
            c(maxbnd.unique, smallbnd.unique, "int.pct")]
 
    } else {

      if (!exists("maxbndx_intersect")) {
        maxbndx_intersect <-
		suppressWarnings(tabulateIntersections(layer1=smallbndx,
		        layer1fld=smallbnd.unique, layer2=maxbndxd, layer2fld=maxbnd.unique))
        maxbndx_intersect <- maxbndx_intersect[!is.na(maxbndx_intersect$int.pct),
            c(maxbnd.unique, smallbnd.unique, "int.pct")]
      }

      ## Remove water from list if it exists
      if (any(maxbndxlst == "Water")) {
        maxbndxlst <- maxbndxlst[maxbndxlst != "Water"]
      }

      ## Subset maxbndxd to only maxbnd.unique that intersects with smallbnd
      maxbndx.intd <- maxbndxd[maxbndxd[[maxbnd.unique]] %in% maxbndxlst,]

      maxbnd.gtthres <- maxbndxlst
      maxbnd.ltthres <- {}
    }

    ############################################################
    ## Display and save image for maxbnd_intersect
    ############################################################
    if (showsteps) {
      plot(sf::st_geometry(maxbndx.intd[maxbnd.unique]), main=NULL, border="dark grey",
		col=sf::sf.colors(nrow(maxbndx.intd), categorical=TRUE, alpha=.2))
      if (nrow(smallbndx) < 1000) {
        plot(sf::st_geometry(smallbndx), add=TRUE, border="red")
      }
      coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(maxbndx.intd)))
      if (maxbnd.addtext) {
        text(coords[,"X"], coords[,"Y"], maxbndx.intd[[maxbnd.unique]], cex=step.cex)
      }
    }

    if (savesteps) {
      out_layer <- paste0("step", stepcnt, "_maxbnd_intersect")
      spExportSpatial(maxbndx.intd,
              savedata_opts=list(outfolder=stepfolder, out_dsn=step_dsn,
			        out_fmt=out_fmt, out_layer=out_layer, append_layer=TRUE,
			        overwrite_dsn=FALSE, overwrite_layer=overwrite))

      jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
      jpeg(jpgfn, res=300, units="in", width=8, height=10)
        plot(sf::st_geometry(maxbndx.intd[maxbnd.unique]), main=NULL, border="dark grey",
             col=sf::sf.colors(nrow(maxbndx.intd), categorical=TRUE, alpha=.2))
        plot(sf::st_geometry(smallbndx), add=TRUE, border="red")

        coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(maxbndx.intd)))
        if (maxbnd.addtext) {
          text(coords[,"X"], coords[,"Y"], maxbndx.intd[[maxbnd.unique]])
        }
      dev.off()
      message("Writing jpg to ", jpgfn, "\n")

      stepcnt <- stepcnt+1
    }

    ## Get the maximum overlap by maxbnd.unique for each smallbnd.unique
    maxbnd_max <- aggregate(maxbndx_intersect$int.pct,
			list(maxbndx_intersect[[smallbnd.unique]]), max)
    names(maxbnd_max) <- c(smallbnd.unique, "int.pct")
    maxbnd_max <- merge(maxbndx_intersect, maxbnd_max)
    maxbndxlst <- maxbndxlst[maxbndxlst %in% unique(maxbnd_max[[maxbnd.unique]])]
 
    if (length(maxbnd.gtthres) > 1 || (nrow(maxbnd_max) > 1 && byeach)) {
      message("smallbnd intersects more than 1 maxbnd")

      if (multiSAdoms) {
        if (byeach) {
          mbndlst <- maxbnd_max[[maxbnd.unique]]
          sbndlst <- lapply(maxbnd_max[[smallbnd.unique]], 
					function(x) smallbndx[smallbndx[[smallbnd.unique]] == x,])
          names(sbndlst) <- maxbnd_max[[smallbnd.unique]]

        } else {
          mbndlst <- maxbnd.gtthres
          mbndlst <- as.list(mbndlst[mbndlst %in% unique(maxbnd_max[[maxbnd.unique]])])

          ## Create list of new smallbnd(s)
          sbndlst <- lapply(mbndlst, function(mbnd, maxbnd_max, smallbndx, smallbnd.unique) {
              sbnd.att <- maxbnd_max[maxbnd_max[[maxbnd.unique]] %in% mbnd, smallbnd.unique]
              if (length(sbnd.att) == 0)
                stop("cannot have more than one model...  check smallbnd")
              smallbndx[smallbndx[[smallbnd.unique]] %in% sbnd.att,]
          }, maxbnd_max, smallbndx, smallbnd.unique)
          names(sbndlst) <- mbndlst
        
 
          ## Appends small areas from maxbnds less than threshold to maxbnds greater 
          ## than threshold based on closest centroids
          if (length(maxbnd.ltthres) > 0) {

            for (j in 1:length(maxbnd.ltthres)) {
              maxbndltd <- maxbndx.intd[maxbndx.intd[[maxbnd.unique]] == maxbnd.ltthres[j],]
              if (nrow(maxbndltd) > 0) {
                maxbndlt.centroid <- suppressWarnings(sf::st_centroid(maxbndltd))

                maxbndx.dist <- closest_poly(maxbndlt.centroid,
			  ypoly=maxbndx[maxbndx[[maxbnd.unique]] %in% maxbndxlst,],
			  ypoly.att=maxbnd.unique, returnsf=FALSE)
                maxbndltnm <- names(maxbndx.dist)[names(maxbndx.dist) %in% maxbnd.gtthres][1]

                sbndlt.att <- maxbnd_max[maxbnd_max[[maxbnd.unique]] %in% maxbnd.ltthres[j], 
				smallbnd.unique]
                sbndlst[[maxbndltnm]] <- rbind(sbndlst[[maxbndltnm]], 
							smallbndx[smallbndx[[smallbnd.unique]] %in% sbndlt.att,])
              }
            }
          }
        }       
      } else {
        message(paste0("smallbnd overlaps greater than ", maxbnd.threshold,
			"% threshold in more than 1 ", maxbnd.unique, 
			"... consider creating more than 1 model"))

        if (length(maxbnd.ltthres) > 1) {
          ## Merge maxbnds less than maxbnd.threshold to closest maxbnd
          mltbndlst <- lapply(maxbnd.ltthres,
			function(lt, maxbndx.intd, maxbnd.unique) {
            	lt.centroid <- suppressWarnings(
				sf::st_centroid(maxbndx.intd[maxbndx.intd[[maxbnd.unique]] == lt, ]))
            		closest.maxbnd <- names(closest_poly(lt.centroid,
				ypoly=maxbndx.intd[maxbndx.intd[[maxbnd.unique]] != lt, ],
				ypoly.att=maxbnd.unique, nbr=1, returnsf=FALSE))
            	return(c(closest.maxbnd, lt)) }, maxbndx.intd, maxbnd.unique)
          mbndlst <- unique(c(maxbnd.gtthres, unlist(mltbndlst)))

          ## Create list of new smallbnd(s)
 #         sbndlst <- lapply(mbndlst, function(mbnd, maxbnd_max, smallbndx, smallbnd.unique) {
 #           sbnd.att <- maxbnd_max[maxbnd_max[[maxbnd.unique]] %in% mbnd, smallbnd.unique]
 #           if (length(sbnd.att) == 0)
 #             stop("cannot have more than one model...  check smallbnd")
 #           smallbndx[smallbndx[[smallbnd.unique]] %in% sbnd.att,]
 #         }, maxbnd_max, smallbndx, smallbnd.unique)

          sbndlst <- list(smallbndx)

        } else {
          mbndlst <- maxbndxlst[1]
          sbndlst <- list(smallbndx)
        }
      }
    } else {
      if (length(maxbnd.gtthres) == 1) {
        mbndlst <- list(maxbnd.gtthres)
      } else {
        mbndlst <- list(maxbnd.ltthres)
      }
      sbndlst <- list(smallbndx)
    }

    ############################################################
    ## Display and save image for maxbnd_select
    ############################################################
    if (showsteps) {
      plot(sf::st_geometry(maxbndx.intd[maxbnd.unique]), main=NULL, border="dark grey",
		col=sf::sf.colors(nrow(maxbndx.intd), categorical=TRUE, alpha=.2), reset=TRUE)
      if (nrow(smallbndx) < 1000) {
        plot(sf::st_geometry(smallbndx), add=TRUE, border="red")
      }
      if (multiSAdoms) {
        plot(sf::st_geometry(maxbndx.intd[maxbndx.intd[[maxbnd.unique]] %in% maxbndxlst,]),
		add=TRUE, border="cyan2", lwd=2)
      } else {
        plot(sf::st_geometry(maxbndx.intd[maxbndx.intd[[maxbnd.unique]] %in% maxbnd.gtthres,]),
		add=TRUE, border="cyan2", lwd=2)
      }
      coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(maxbndx.intd)))
      if (maxbnd.addtext) {
        text(coords[,"X"], coords[,"Y"], maxbndx.intd[[maxbnd.unique]], cex=step.cex)
      }
    }
 
    if (savesteps) {
      out_layer <- paste0("step", stepcnt, "_maxbnd_select")
      maxbndx.selectd <- maxbndx.intd[maxbndx.intd[[maxbnd.unique]] %in% maxbnd.gtthres,]
      spExportSpatial(maxbndx.selectd,
              savedata_opts=list(outfolder=stepfolder, out_dsn=step_dsn,
              out_fmt=out_fmt, out_layer=out_layer, append_layer=TRUE,
              overwrite_layer=overwrite))

      jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
      jpeg(jpgfn, res=300, units="in", width=8, height=10)
        plot(sf::st_geometry(maxbndx.intd[maxbnd.unique]), main=NULL, border="dark grey",
             col=sf::sf.colors(nrow(maxbndx.intd), categorical=TRUE, alpha=.2))

        if (nrow(smallbndx) < 1000) {
          plot(sf::st_geometry(smallbndx), add=TRUE, border="red")
        }
        if (multiSAdoms) {
          plot(sf::st_geometry(maxbndx.intd[maxbndx.intd[[maxbnd.unique]] %in% maxbndxlst,]),
		add=TRUE, border="cyan2", lwd=2)
        } else {
          plot(sf::st_geometry(maxbndx.intd[maxbndx.intd[[maxbnd.unique]] %in% maxbnd.gtthres,]),
		add=TRUE, border="cyan2", lwd=2)
        }
        coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(maxbndx.intd)))
        if (maxbnd.addtext) {
          text(coords[,"X"], coords[,"Y"], maxbndx.intd[[maxbnd.unique]], cex=step.cex)
        }
      dev.off()
      message("Writing jpg to ", jpgfn, "\n")

      stepcnt <- stepcnt+1
    }

    ## Check largebndx
    if (maxislarge) {
      largebndx <- maxbndx.intd
    }
  }
  ## Check helperbndx
  if (largeishelper) {
    helperbndx <- largebndx
  }

  #################################################################
  ## Get helper domains
  #################################################################
  helperbndxlst <- list()
  smallbndxlst <- list()
  SAbndlst <- list()
  SAdomsnmlst <- vector("character", length(sbndlst))

  ## Loop thru sbndlst
  for (i in 1:length(sbndlst)) {
    sbnd <- sbndlst[[i]]
    if (smallbnd.domain != smallbnd.unique) {
      sbnd <- sf_dissolve(sbnd, smallbnd.domain)
    }
    sbnd$AOI <- 1
    sbndnm <- names(sbndlst[i])
    if (is.null(sbndnm) || is.na(sbndnm) || sbndnm == "") {
      sbndnm <- "SAbnd"
      if (length(sbndlst) > 1) {
        sbndnm <- paste0(sbndnm, i)
      }
    }
    SAdomsnmlst[i] <- sbndnm
    nbrdom <- 0
    mbnd <- {}

    message("\ngetting model domains for ", sbndnm, "...")

    ## Get number of boundaries - if maxbnd=NULL, use number of largebnd
    ## If nbrdom.min=NULL, only use 1 maxbnd
    if (length(mbndlst) > 0) {
      if (is.null(nbrdom.min)) {
        mbnd <- mbndlst[[i]][1]
        nbrdom.minx <- 1
      } else {
        mbnd <- c(unique(mbndlst[[i]], maxbndxlst))
        nbrdom.minx <- nbrdom.min
      }
    } else {
      nbrdom.minx <- ifelse(is.null(nbrdom.min), 1, nbrdom.min)
    }
    sbndd <- sf_dissolve(sbnd, areacalc=FALSE)
    sbnd.centroid <- suppressWarnings(sf::st_centroid(sbndd))
    largebnd_select <- NULL		## for selected largebnds to include more helperbnds


    largebndx <- largebndx[, largebnd.unique]
    helperbndx <- helperbndx[, helperbnd.unique]

    j <- 1
    ## Loop thru maxbndlst
    while (nbrdom < nbrdom.minx && j <= ifelse(length(maxbndxlst) > 0, length(mbnd), 1)) {
      if (length(mbnd) > 0) {
        message("\nadding ", maxbnd.unique, ": ", mbnd[j])

        ## Subset maxbndx.intd
        maxbndx.tmpd <- maxbndx.intd[maxbndx.intd[[maxbnd.unique]] %in% mbnd[j],]

       ## Remove columns in maxbndx.tmpd with same names as in largebndx.int
        maxbndx.tmpd <- maxbndx.tmpd[,
		names(maxbndx.tmpd)[!names(maxbndx.tmpd) %in% names(largebndx)]]

        ## Get largebnd polygons within maxbndx and dissolve
        largebndx.int <- suppressWarnings(selectByIntersects(largebndx, maxbndx.tmpd))
        if (is.null(largebndx.int))
          stop("no largebnds for maxbnd: ", mbnd[j])
        largebndx.intd <- sf_dissolve(largebndx.int, largebnd.unique, areacalc=FALSE)

      } else {
        ## get intersection of largebndx and smallbndx
        largebndx.intd <- getIntersect(largebndx, sbnd, largebnd.unique, layer2fld="AOI")
      }

      ###############################################################
      ## Display and save image for largebnd (largebnd within maxbnd)
      ###############################################################
      if (showsteps) {
        plot(merge_extents(largebndx.intd, sbnd), border="transparent")
        plot(sf::st_geometry(largebndx.intd), add=TRUE, main=NULL, border="dark grey")
        plot(sf::st_geometry(sbnd), add=TRUE, border="red")
        coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebndx.intd)))
        if (largebnd.addtext) {
          text(coords[,"X"], coords[,"Y"], largebndx.intd[[largebnd.unique]], cex=step.cex)
        }
      }
      if (savesteps) {
        out_layer <- paste0("step", stepcnt, "_", sbndnm, "_largebnds")
        spExportSpatial(largebndx.intd,
                savedata_opts=list(outfolder=stepfolder,
                                   out_dsn=step_dsn,
                                   out_fmt=out_fmt,
                                   out_layer=out_layer,
                                   append_layer=TRUE,
                                   overwrite_layer=overwrite))
        jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
        jpeg(jpgfn, res=300, units="in", width=8, height=10)
          plot(merge_extents(largebndx.intd, sbnd), border="transparent")
          plot(sf::st_geometry(largebndx.intd), add=TRUE, main=NULL, border="dark grey")
          plot(sf::st_geometry(sbndlst[[i]]), add=TRUE, border="red")
          coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebndx.intd)))
          if (largebnd.addtext) {
            text(coords[,"X"], coords[,"Y"], largebndx.intd[[largebnd.unique]])
          }
        dev.off()
        message("Writing jpg to ", jpgfn, "\n")

        stepcnt <- stepcnt+1
      }

      ## get percent overlap of x (largebnd) and y (smallbnd)
      ############################################################
      largebndx.pct <- suppressWarnings(tabulateIntersections(layer1=sbnd,
 			layer1fld="AOI", layer2=largebndx.intd, layer2fld=largebnd.unique))
      largebndx.pct <- largebndx.pct[order(largebndx.pct$int.pct, decreasing=TRUE),]
      largebndx.pct[is.na(largebndx.pct$int.pct), "int.pct"] <- 0
      largebndxlst <- unique(largebndx[[largebnd.unique]])
      largebnd.gtthres <- largebndx.pct[largebndx.pct$int.pct >= largebnd.threshold, largebnd.unique]
      largebnd.ltthres <- largebndx.pct[largebndx.pct$int.pct < largebnd.threshold &
					largebndx.pct$int.pct != 0, largebnd.unique]
      largebnd.lt0 <- largebndx.pct[largebndx.pct$int.pct == 0, largebnd.unique]
      message(paste0(utils::capture.output(largebndx.pct), collapse = "\n"))

      if (length(largebnd.gtthres) == 0) {
        largebnd.gtthres <- largebnd.ltthres
      }

      ## Select largebnd(s) that intersect more than threshold
      largebnd_select <- largebndx.intd[largebndx.intd[[largebnd.unique]] %in% largebnd.gtthres,]

      ############################################################
      ## Display and save image for largebnd_intersect
      ############################################################
      if (showsteps) {
        plot(merge_extents(largebndx.intd, sbnd), border="transparent")
        plot(sf::st_geometry(largebndx.intd), add=TRUE, main=NULL, border="dark grey")
        plot(sf::st_geometry(sbnd), add=TRUE, border="red")
        plot(sf::st_geometry(largebnd_select), add=TRUE)
        coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebndx.intd)))
        if (largebnd.addtext) {
          text(coords[,"X"], coords[,"Y"], largebndx.intd[[largebnd.unique]], cex=step.cex)
        }
      }

      if (savesteps) {
        out_layer <- paste0("step", stepcnt, "_", sbndnm, "_largebnd_intersect")
        spExportSpatial(largebndx.intd,
                savedata_opts=list(outfolder=stepfolder,
                                   out_dsn=step_dsn,
                                   out_fmt=out_fmt,
                                   out_layer=out_layer,
                                   append_layer=TRUE,
                                   overwrite_layer=overwrite))
        jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
        jpeg(jpgfn, res=300, units="in", width=8, height=10)
          plot(merge_extents(largebndx.intd, sbnd), border="transparent")
          plot(sf::st_geometry(largebndx.intd), add=TRUE, main=NULL, border="dark grey")
          plot(sf::st_geometry(sbnd), add=TRUE, border="red")
          plot(sf::st_geometry(largebnd_select), add=TRUE)
          coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebndx.intd)))
          if (largebnd.addtext) {
            text(coords[,"X"], coords[,"Y"], largebndx.intd[[largebnd.unique]])
          }
        dev.off()
        message("Writing jpg to ", jpgfn, "\n")

        stepcnt <- stepcnt+1
      }

      ############################################################################
      ## largebnd
      ############################################################################
      #largebndxlst <- unique(c(largebnd.ltthres, largebndx[[largebnd.unique]]))
      largebndxlst <- largebndxlst[!largebndxlst %in% largebnd.gtthres]

      k <- 1
      end <- FALSE

      ## For largebnds with no intersecting smallbnd, get largebndx polygons closest
      ## to smallbnd centroid within maxbnd
      if (length(largebndxlst) > 0) {
        largebndx.dist <- closest_poly(sbnd.centroid,
			ypoly=largebndx[largebndx[[largebnd.unique]] %in% largebndxlst,],
			ypoly.att=largebnd.unique, returnsf=FALSE)
        largebndxlst <- unique(c(largebnd.ltthres, largebnd.lt0, names(largebndx.dist)))
      }
 
      while (!end) {
        ## Get intersecting helper polygons
        helperbndx.tmp <- suppressWarnings(sf::st_join(helperbndx,
						sf_dissolve(largebnd_select, largebnd.unique),
						join=sf::st_intersects, left=FALSE))

        # get percent overlap of helperbndx.int and y largebndx.int
        ############################################################
#        helperbndx.tmp$FID <- seq(1:nrow(helperbndx.tmp))
#        helperbndx.tmppct <- suppressWarnings(tabulateIntersections(layer1=helperbndx.tmp,
#			layer1fld="FID", layer2=largebnd_select))
#        FIDpct <- helperbndx.tmppct$FID[helperbndx.tmppct$int.pct > largebnd.threshold]
#        helperbndx.tmp <- helperbndx.tmp[helperbndx.tmp$FID %in% FIDpct,]

        helperbndx.tmppct <- tryCatch(suppressWarnings(tabulateIntersections(layer1=helperbndx.tmp,
			layer1fld=helperbnd.unique, layer2=largebnd_select)),
     	 			error=function(e) {
					message("helperbnd intersection...")
					stop(e, "\n")})

        helperbnd.pct <- helperbndx.tmppct[[helperbnd.unique]][
			helperbndx.tmppct$int.pct > largebnd.threshold]
        helperbndx.tmp <- helperbndx.tmp[helperbndx.tmp[[helperbnd.unique]] %in% helperbnd.pct,]

        ## Get number of polygons (i.e., domains)
        nbrdom <- nrow(helperbndx.tmp)
        nbrdom.needed <- (nbrdom.minx - nbrdom)
        if (nbrdom.needed > 0) {
          message("still need ", (nbrdom.minx - nbrdom), " more helperbnds")
        } else {
          message("reached nbrdom.min... total of ", nbrdom, " domains")
        }
        ## Show intermediate step
##        if (showsteps) {
##          plot(append(sf::st_as_sfc(sf::st_bbox(sbnd)),
##			sf::st_as_sfc(sf::st_bbox(helperbndx.tmp))), border="transparent")
##          plot(sf::st_geometry(helperbndx.tmp), add=TRUE, border="grey")
##          plot(sf::st_geometry(sbnd), add=TRUE, border="red")
##          plot(sf::st_geometry(largebnd_select), add=TRUE)
##          coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebnd_select)))
##          if (largebnd.addtext) {
##            text(coords[,"X"], coords[,"Y"], largebnd_select[[largebnd.unique]], cex=step.cex)
##          }
##        }
#        if (nbrdom > nbrdom.minx || k > length(largebndxlst)) {

        if (nbrdom.needed > 0 && k < length(largebndxlst)) {
          message("adding ", largebnd.unique, ": ", paste(largebndxlst[k], collapse=", "))

          ## Select largebnd(s) that intersect more than threshold
          largebnd_select <- rbind(largebnd_select,
			sf_dissolve(largebndx[largebndx[[largebnd.unique]] %in% largebndxlst[k],],
			largebnd.unique, areacalc=FALSE))
          end <- FALSE
        } else {
          end <- TRUE
        }

        k <- k + 1   ## loop through maxbnd
      } ## End while k - largebnd

#################
#      if (nbrdom.needed > 0) {
#        maxbndx.intd <- sf_dissolve(maxbndx, maxbnd.unique)
#        if (nrow(maxbndx.intd) == length(maxbnd.gtthres))
#          stop("no maxbnds for selecting additional domains...")
#
#        maxbndx.dist <- closest_poly(sbnd.centroid,
#			ypoly=maxbndx.intd, ypoly.att=maxbnd.unique, returnsf=FALSE)
#        mbnd <- unique(c(maxbnd.gtthres, names(maxbndx.dist)))
#      }

      ############################################################
      ## Display and save image for largebnd_select
      ############################################################
      if (showsteps) {
        plot(merge_extents(largebndx.intd, sbndlst[[i]]), border="transparent")
        plot(sf::st_geometry(largebndx.intd), add=TRUE, main=NULL, border="dark grey")
        plot(sf::st_geometry(sbndlst[[i]]), add=TRUE, border="red")
        plot(sf::st_geometry(largebnd_select), add=TRUE, border="cyan2", lwd=2)
        coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebndx.intd)))
        if (largebnd.addtext) {
          text(coords[,"X"], coords[,"Y"], largebndx.intd[[largebnd.unique]])
        }
      }

      if (savesteps && !is.null(helperbndx.tmp)) {
        out_layer <- paste0("step", stepcnt, "_", sbndnm, "_largebnd_select")
        spExportSpatial(largebnd_select,
              savedata_opts = list(outfolder=stepfolder, out_dsn=step_dsn,
			        out_fmt=out_fmt, out_layer=out_layer, append_layer=TRUE,
			        overwrite_layer=overwrite))

        jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
        jpeg(jpgfn, res=300, units="in", width=8, height=10)
          plot(merge_extents(largebndx.intd, sbndlst[[i]]), border="transparent")
          plot(sf::st_geometry(largebndx.intd), add=TRUE, main=NULL, border="dark grey")
          plot(sf::st_geometry(sbndlst[[i]]), add=TRUE, border="red")
          plot(sf::st_geometry(largebnd_select), add=TRUE, border="cyan2", lwd=2)
          coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebndx.intd)))
          if (largebnd.addtext) {
            text(coords[,"X"], coords[,"Y"], largebndx.intd[[largebnd.unique]], cex=step.cex)
          }
        dev.off()
        message("Writing jpg to ", jpgfn, "\n")

        stepcnt <- stepcnt+1
      }

      ############################################################
      ## Display and save image for helperbnd_intersect
      ############################################################
      if (showsteps) {
        plot(merge_extents(largebnd_select, sbnd), border="transparent")
        plot(sf::st_geometry(helperbndx.tmp), add=TRUE, border="grey")
        plot(sf::st_geometry(sbnd), add=TRUE, border="red")
        plot(sf::st_geometry(largebnd_select), add=TRUE)
        coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebnd_select)))
        if (largebnd.addtext) {
          text(coords[,"X"], coords[,"Y"], largebnd_select[[largebnd.unique]], 0.8)
        }
      }

      if (savesteps && !is.null(helperbndx.tmp)) {
        out_layer <- paste0("step", stepcnt, "_", sbndnm, "_helperbnd_intersect")
        spExportSpatial(largebnd_select,
                savedata_opts=list(outfolder=stepfolder, out_dsn=step_dsn,
			          out_fmt=out_fmt, out_layer=out_layer, append_layer=TRUE,
			          overwrite_layer=overwrite))

        jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
        jpeg(jpgfn, res=300, units="in", width=8, height=10)
          plot(merge_extents(largebnd_select, sbnd), border="transparent")
          plot(sf::st_geometry(helperbndx.tmp), add=TRUE, border="grey")
          plot(sf::st_geometry(sbnd), add=TRUE, border="red", lwd=1.5)
          plot(sf::st_geometry(largebnd_select), add=TRUE)
          coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(largebnd_select)))
          if (largebnd.addtext) {
            text(coords[,"X"], coords[,"Y"], largebnd_select[[largebnd.unique]])
          }
        dev.off()
        message("Writing jpg to ", jpgfn, "\n")

        stepcnt <- stepcnt+1
      }

      j <- j + 1
    } ## End while j - maxbnd

    if (polyunion) {
      ## Remove columns in helperbndx.tmp with same names as in smallbnd attributes
      helperbndx.tmp <- helperbndx.tmp[, names(helperbndx.tmp)[!names(helperbndx.tmp) %in%
			c(smallbnd.unique, "AOI", smallbnd.domain)]]

      ## Change name of helperbnd.unique values if the same as smallbnd.unique values
      if (any(helperbndx.tmp[[helperbnd.unique]] %in% smallbndx[[smallbnd.unique]])) {
        helperbndx.tmp[[helperbnd.unique]] <- suppressWarnings(sapply(helperbndx.tmp[[helperbnd.unique]],
			checknm, sbndlst[[i]][[smallbnd.unique]]))
      }
      ## Union helperbnd and smallbnd polygons
      SAdoms <- suppressWarnings(spUnionPoly(sf::st_make_valid(helperbndx.tmp[, helperbnd.unique]),
				polyv2=sf::st_make_valid(sbnd)))
    } else {
      SAdoms <- sbnd
    }

    ## Add 0 to non-AOI
    SAdoms[is.na(SAdoms$AOI), "AOI"] <- 0


    ## Add a new column (DOMAIN) with helperbnd.unique where AOI = 0
    SAdoms$DOMAIN <- SAdoms[[smallbnd.domain]]
    SAdoms$DOMAIN[SAdoms$AOI == 0] <- SAdoms[[helperbnd.unique]][SAdoms$AOI == 0]
    SAdoms$DOMAIN[SAdoms$AOI == 0] <- SAdoms[[helperbnd.unique]][SAdoms$AOI == 0]

#    plot(sf::st_geometry(SAdoms["DOMAIN"]), main=NULL, border="grey",
#		col=sf::sf.colors(nbrdom, categorical=TRUE, alpha=.2))
#          coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(SAdoms)))
#          if (largebnd.addtext) {
#            text(coords[,"X"], coords[,"Y"], SAdoms[["DOMAIN"]], cex=.4)
#          }

    SAdomslst[[i]] <- SAdoms
    helperbndxlst[[i]] <- helperbndx.tmp
    sbnd$AOI <- NULL
    smallbndxlst[[i]] <- sbnd

  } ## End while i - sbnd

  names(SAdomslst) <- SAdomsnmlst
  names(smallbndxlst) <- SAdomsnmlst

  returnlst <- list(SAdomslst=SAdomslst, helperbndxlst=helperbndxlst,
				smallbndxlst=smallbndxlst)

  return(returnlst)
}




## FIESTA_SAmod_demo_plot.R: function to create plots for SA demo

FIESTA_SAmod_demo_plots <- function (estvar, prednames=NULL, est.com, domain.att=NULL,
		title.ref, cty=FALSE, showimg=TRUE, saveimg=FALSE, outfolder=NULL,
		out_layer=NULL){


## Define results files
	est <- est.com
	est.AOI <- est[est$AOI ==1 ,]

	maxy.AOI <- max(c(est.AOI$DIR,est.AOI$JU.GREG,
				est.AOI$JU.EBLUP,est.AOI$JFH,
				est.AOI$JA.synth,est.AOI$JU.Synth),na.rm = TRUE)*2
# cosmetics
	mycolors <- c("lightblue", "mistyrose", "lightcyan","lavender", "cornsilk",
					"white")
#	mycolors <- c("darkgreen", "green","blue","purple","orange","red")

 if (is.null(domain.att)) {
   domain.att <- "DOMAIN"
 }

 if (showimg) {
   dev.new(record=TRUE)
   if(nrow(est.AOI)==1){
       ## Plot results for AOI
	   tmp <- est.AOI[,c("DIR", "JU.GREG",
				"JU.EBLUP", "JFH",
				"JA.synth","JU.Synth")]
	   #actual plot
		tmp1 <- barplot(t(as.matrix(tmp)), beside=T,
				names.arg=est.AOI[[domain.att]][my.range],
				xlab="", ylab=estvar, ylim = c(0,maxy.AOI),
		col = mycolors)
	   #error bars
		tmp2<- est.AOI[,c("DIR.se", "JU.GREG.se",
			"JU.EBLUP.se.1","JFH.se","JA.synth.se", "DIR.se")]
	   #plot error bars
	     #DIR
		arrows(x0 = tmp1[1,], y0 = tmp$DIR + 2*tmp2$DIR.se,
			 x1 = tmp1[1,], y1 = tmp$DIR - 2*tmp2$DIR.se,
			 length = 0.01, angle = 90, code = 3)
	     #GREG
		  arrows(x0 = tmp1[2,], y0 = tmp$JU.GREG + 2*tmp2$JU.GREG.se,
			 x1 = tmp1[2,], y1 = tmp$JU.GREG - 2*tmp2$JU.GREG.se,
			 length = 0.01, angle = 90, code = 3)
	     #U-EBLUP
		  arrows(x0 = tmp1[3,], y0 = tmp$JU.EBLUP + 2*tmp2$JU.EBLUP.se,
			 x1 = tmp1[3,], y1 = tmp$JU.EBLUP - 2*tmp2$JU.EBLUP.se,
			 length = 0.01, angle = 90, code = 3)
	     #JFH
		  arrows(x0 = tmp1[4,], y0 = tmp$JFH + 2*tmp2$JFH.se,
			 x1 = tmp1[4,], y1 = tmp$JFH - 2*tmp2$JFH.se,
			 length = 0.01, angle = 90, code = 3)
	     #A-Synth
		  arrows(x0 = tmp1[5,], y0 = tmp$JA.synth + 2*tmp2$JA.synth.se,
			 x1 = tmp1[5,], y1 = tmp$JA.synth - 2*tmp2$JA.synth.se,
			 length = 0.01, angle = 90, code = 3)

	     #legend and sub
	 	  title(main = est.AOI[[domain.att]], sub = paste(
				paste("Number of plots = ",est.AOI$NBRPLT.x,
					",  Preds = "),
					prednames[1],
					prednames[2],
					prednames[3],
					",  Run = ",title.ref))
	 	  legend(min(tmp1)-.5,maxy.AOI,
			fill=mycolors,
			horiz = TRUE,
			legend=c("HT", "GREG", "U-EBL","FH","A-Syn","U-Syn"),
			bty="n", cex = 1, text.width = .5)
   }

   if(nrow(est.AOI)>1){
   ## Plot results for multiple AOIs

	  est.AOI[[domain.att]] <- substr(est.AOI[[domain.att]],1,5)

	  for (i in 0:(ceiling(nrow(est.AOI)/5)-1)){
		my.range <- i*5 + (1:5)
		tmp <- est.AOI[my.range,c("DIR", "JU.GREG", "JU.EBLUP",
				"JFH", "JA.synth","JU.Synth")]

	   #actual plot
		tmp1 <- barplot(t(as.matrix(tmp)), beside=T,
				names.arg=est.AOI[[domain.att]][my.range],
				xlab="", ylab=estvar, ylim = c(0,maxy.AOI),
			col = mycolors)
	   #print n.sample plots
		text(tmp1[2,]+.5, y = maxy.AOI*.8,
			labels = est.AOI$NBRPLT.y[my.range], cex=1)
	   #error bars
		tmp2<- est.AOI[my.range,c("DIR.se", "JU.GREG.se",
			"JU.EBLUP.se.1","JFH.se","JA.synth.se", "DIR.se")]

	   #plot error bars
	   #DIR
		arrows(x0 = tmp1[1,], y0 = tmp$DIR + 2*tmp2$DIR.se,
			x1 = tmp1[1,],y1 = tmp$DIR - 2*tmp2$DIR.se,
			length = 0.01, angle = 90, code = 3)
	   #GREG
		arrows(x0 = tmp1[2,], y0 = tmp$JU.GREG + 2*tmp2$JU.GREG.se,
		       x1 = tmp1[2,], y1 = tmp$JU.GREG - 2*tmp2$JU.GREG.se,
			 length = 0.01, angle = 90, code = 3)
	   #EBLUP
		arrows(x0 = tmp1[3,], y0 = tmp$JU.EBLUP + 2*tmp2$JU.EBLUP.se.1,
			 x1 = tmp1[3,], y1 = tmp$JU.EBLUP - 2*tmp2$JU.EBLUP.se.1,
			 length = 0.01, angle = 90, code = 3)
	   #JFH
		arrows(x0 = tmp1[4,], y0 = tmp$JFH + 2*tmp2$JFH.se,
			 x1 = tmp1[4,], y1 = tmp$JFH - 2*tmp2$JFH.se,
			 length = 0.01, angle = 90, code = 3)
	   #A-Synth
		arrows(x0 = tmp1[5,], y0 = tmp$JA.synth + 2*tmp2$JA.synth.se,
			 x1 = tmp1[5,], y1 = tmp$JA.synth - 2*tmp2$JA.synth.se,
			 length = 0.01, angle = 90, code = 3)
	   #legend
	 	legend(min(tmp1),maxy.AOI,
			fill = mycolors,
			horiz = TRUE,
			legend=c("HT", "GREG", "U-EBL","FH","A-Syn","U-Syn"),
			bty="n",cex = 1,text.width = 2.75)
	 	title(main = title.ref, sub = paste("Preds = ",
					prednames[1],
					prednames[2],
					prednames[3]))
	  }
   }
   #dev.off()
 }  ## showimg


if (saveimg) {
   if (is.null(out_layer))
     out_layer <- paste0("SAest_compare")
   jpgfn <- paste0(outfolder, "/", out_layer, ".jpg")
   jpeg(jpgfn, res=400, units="in", width=8, height=10)
}

if(nrow(est.AOI)==1){
## Plot results for AOI
	 tmp <- est.AOI[,c("DIR", "JU.GREG",
				"JU.EBLUP", "JFH",
				"JA.synth","JU.Synth")]

	 #actual plot
		tmp1 <- barplot(t(as.matrix(tmp)), beside=T,
				names.arg=est.AOI[[domain.att]][my.range],
				xlab="", ylab=estvar, ylim = c(0,maxy.AOI),
		col = mycolors)
	 #error bars
		tmp2<- est.AOI[,c("DIR.se", "JU.GREG.se",
			"JU.EBLUP.se.1","JFH.se","JA.synth.se", "DIR.se")]
	 #plot error bars
	   #DIR
		arrows(x0 = tmp1[1,], y0 = tmp$DIR + 2*tmp2$DIR.se,
			 x1 = tmp1[1,], y1 = tmp$DIR - 2*tmp2$DIR.se,
			 length = 0.01, angle = 90, code = 3)
	   #GREG
		arrows(x0 = tmp1[2,], y0 = tmp$JU.GREG + 2*tmp2$JU.GREG.se,
			 x1 = tmp1[2,], y1 = tmp$JU.GREG - 2*tmp2$JU.GREG.se,
			 length = 0.01, angle = 90, code = 3)
	   #U-EBLUP
		arrows(x0 = tmp1[3,], y0 = tmp$JU.EBLUP + 2*tmp2$JU.EBLUP.se,
			 x1 = tmp1[3,], y1 = tmp$JU.EBLUP - 2*tmp2$JU.EBLUP.se,
			 length = 0.01, angle = 90, code = 3)
	   #JFH
		arrows(x0 = tmp1[4,], y0 = tmp$JFH + 2*tmp2$JFH.se,
			 x1 = tmp1[4,], y1 = tmp$JFH - 2*tmp2$JFH.se,
			 length = 0.01, angle = 90, code = 3)
	   #A-Synth
		arrows(x0 = tmp1[5,], y0 = tmp$JA.synth + 2*tmp2$JA.synth.se,
			 x1 = tmp1[5,], y1 = tmp$JA.synth - 2*tmp2$JA.synth.se,
			 length = 0.01, angle = 90, code = 3)

	 #legend and sub
	 	title(main = est.AOI$DOMAIN, sub = paste(
				paste("Number of plots = ",est.AOI$NBRPLT.x,
					",  Preds = "),
					prednames[1],
					prednames[2],
					prednames[3],
					",  Run = ",title.ref))
	 	legend(min(tmp1)-.5,maxy.AOI,
			fill=mycolors,
			horiz = TRUE,
			legend=c("HT", "GREG", "U-EBL","FH","A-Syn","U-Syn"),
			bty="n", cex = 1, text.width = .5)

}

if(nrow(est.AOI)>1){
## Plot results for multiple AOIs

	est.AOI$DOMAIN <- substr(est.AOI$DOMAIN,1,5)

	for (i in 0:(ceiling(nrow(est.AOI)/5)-1)){
		my.range <- i*5 + (1:5)
		tmp <- est.AOI[my.range,c("DIR", "JU.GREG", "JU.EBLUP",
				"JFH", "JA.synth","JU.Synth")]

	 #actual plot
		tmp1 <- barplot(t(as.matrix(tmp)), beside=T,
				names.arg=est.AOI$DOMAIN[my.range], xlab="",
				ylab=estvar, ylim = c(0,maxy.AOI),
			col = mycolors)
	 #print n.sample plots
		text(tmp1[2,]+.5, y = maxy.AOI*.8,
			labels = est.AOI$NBRPLT.y[my.range],cex=1)
	 #error bars
		tmp2<- est.AOI[my.range,c("DIR.se", "JU.GREG.se",
			"JU.EBLUP.se.1","JFH.se","JA.synth.se", "DIR.se")]

	 #plot error bars
	 #DIR
		arrows(x0 = tmp1[1,], y0 = tmp$DIR + 2*tmp2$DIR.se,
			x1 = tmp1[1,],y1 = tmp$DIR - 2*tmp2$DIR.se,
			length = 0.01, angle = 90, code = 3)
	 #GREG
		arrows(x0 = tmp1[2,], y0 = tmp$JU.GREG + 2*tmp2$JU.GREG.se,
		       x1 = tmp1[2,], y1 = tmp$JU.GREG - 2*tmp2$JU.GREG.se,
			 length = 0.01, angle = 90, code = 3)
	 #EBLUP
		arrows(x0 = tmp1[3,], y0 = tmp$JU.EBLUP + 2*tmp2$JU.EBLUP.se.1,
			 x1 = tmp1[3,], y1 = tmp$JU.EBLUP - 2*tmp2$JU.EBLUP.se.1,
			 length = 0.01, angle = 90, code = 3)
	 #JFH
		arrows(x0 = tmp1[4,], y0 = tmp$JFH + 2*tmp2$JFH.se,
			 x1 = tmp1[4,], y1 = tmp$JFH - 2*tmp2$JFH.se,
			 length = 0.01, angle = 90, code = 3)
	 #A-Synth
		arrows(x0 = tmp1[5,], y0 = tmp$JA.synth + 2*tmp2$JA.synth.se,
			 x1 = tmp1[5,], y1 = tmp$JA.synth - 2*tmp2$JA.synth.se,
			 length = 0.01, angle = 90, code = 3)
	 #legend
	 	legend(min(tmp1),maxy.AOI,
			fill = mycolors,
			horiz = TRUE,
			legend=c("HT", "GREG", "U-EBL","FH","A-Syn","U-Syn"),
			bty="n",cex = 1,text.width = 2.75)
	 	title(main = title.ref, sub = paste("Preds = ",
					prednames[1],
					prednames[2],
					prednames[3]))
	}
}

if (saveimg) {
      dev.off()
      message("Writing jpg to ", jpgfn, "\n")
}

}


