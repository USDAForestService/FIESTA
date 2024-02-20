# Union Poly Works and creates expected SF object

    Code
      polyUnion
    Output
      Simple feature collection with 3 features and 16 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -945312.3 ymin: 2386884 xmax: -860375.4 ymax: 2505111
      Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs
        REGION FORESTNUMB DISTRICTNU                     DISTRICTNA OBJECTID
      1     02         02         03 Medicine Wheel Ranger District        6
      2     02         02         03 Medicine Wheel Ranger District       NA
      3     02         02       <NA>                           <NA>        6
            ADMINFORES   REV_DATE DATA_SOURC ACCURACY REGION.1 FORESTNUMB.1
      1 99020200010343 2012/01/20         09        0       02           02
      2           <NA>       <NA>       <NA>       NA     <NA>         <NA>
      3 99020200010343 2012/01/20         09        0     <NA>         <NA>
                     FORESTNAME GIS_ACRES Shape_Leng Shape_Area
      1 Bighorn National Forest   1112790   4.942746  0.5097936
      2                    <NA>        NA         NA         NA
      3 Bighorn National Forest   1112790   4.942746  0.5097936
                              geometry  ACRES_GIS
      1 MULTIPOLYGON (((-915228.1 2... 364357.613
      2 MULTIPOLYGON (((-906037.1 2...    168.745
      3 MULTIPOLYGON (((-919246.1 2... 748054.740

