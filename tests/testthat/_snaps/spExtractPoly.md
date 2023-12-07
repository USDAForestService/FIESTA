# Extract Polygon Attributes works

    Code
      ext_plts
    Output
      Simple feature collection with 56 features and 22 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -943048.1 ymin: 2394115 xmax: -863987.6 ymax: 2501966
      Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs
      First 10 features:
                      CN INVYR STATECD CYCLE UNITCD COUNTYCD  PLOT MEASYEAR RDDISTCD
      142 40404876010690  2012      56     3      2        3 83143     2012        6
      145 40404879010690  2011      56     3      2        3 80153     2011        6
      152 40404886010690  2012      56     3      2        3 86397     2012       NA
      159 40404893010690  2011      56     3      2        3 85901     2011        4
      160 40404894010690  2013      56     3      2        3 90221     2013        6
      165 40404899010690  2013      56     3      2        3 85144     2013       NA
      169 40404903010690  2012      56     3      2        3 80353     2012       NA
      181 40404916010690  2011      56     3      2        3 85420     2011       NA
      186 40404921010690  2013      56     3      2        3 85403     2013       NA
      190 40404925010690  2012      56     3      2        3 87744     2012        3
          NF_SAMPLING_STATUS_CD PLOT_STATUS_CD NF_PLOT_STATUS_CD NBRCND NBRCNDSAMP
      142                     0              1                NA      2          2
      145                     0              1                NA      1          1
      152                     0              2                NA      1          1
      159                     0              1                NA      1          1
      160                     0              1                NA      1          1
      165                     0              2                NA      1          1
      169                     0              2                NA      1          1
      181                     0              2                NA      1          1
      186                     0              2                NA      1          1
      190                     0              1                NA      1          1
          NBRCNDFOR CCLIVEPLT        FORNONSAMP        PLOT_ID REGION FORESTNUMB
      142         2      67.5    Sampled-Forest ID560200383143     02         02
      145         1      66.0    Sampled-Forest ID560200380153     02         02
      152         0       0.0 Sampled-Nonforest ID560200386397     02         02
      159         1      48.0    Sampled-Forest ID560200385901     02         02
      160         1       2.0    Sampled-Forest ID560200390221     02         02
      165         0       8.0 Sampled-Nonforest ID560200385144     02         02
      169         0       1.0 Sampled-Nonforest ID560200380353     02         02
      181         0      10.0 Sampled-Nonforest ID560200385420     02         02
      186         0       0.0 Sampled-Nonforest ID560200385403     02         02
      190         1      36.0    Sampled-Forest ID560200387744     02         02
          DISTRICTNU                     DISTRICTNA                  geometry
      142         03 Medicine Wheel Ranger District POINT (-937172.9 2492354)
      145         03 Medicine Wheel Ranger District POINT (-943048.1 2487710)
      152         03 Medicine Wheel Ranger District POINT (-928130.7 2476936)
      159         03 Medicine Wheel Ranger District POINT (-919787.2 2464822)
      160         03 Medicine Wheel Ranger District POINT (-914104.9 2465931)
      165         03 Medicine Wheel Ranger District POINT (-930324.7 2461728)
      169         03 Medicine Wheel Ranger District POINT (-905102.8 2454596)
      181         03 Medicine Wheel Ranger District POINT (-896931.1 2444320)
      186         03 Medicine Wheel Ranger District POINT (-906430.5 2439460)
      190         03 Medicine Wheel Ranger District POINT (-906870.1 2435067)

---

    Code
      ext_plts2
    Output
      Simple feature collection with 56 features and 19 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -943048.1 ymin: 2394115 xmax: -863987.6 ymax: 2501966
      Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs
      First 10 features:
                      CN INVYR STATECD CYCLE UNITCD COUNTYCD  PLOT MEASYEAR RDDISTCD
      142 40404876010690  2012      56     3      2        3 83143     2012        6
      145 40404879010690  2011      56     3      2        3 80153     2011        6
      152 40404886010690  2012      56     3      2        3 86397     2012       NA
      159 40404893010690  2011      56     3      2        3 85901     2011        4
      160 40404894010690  2013      56     3      2        3 90221     2013        6
      165 40404899010690  2013      56     3      2        3 85144     2013       NA
      169 40404903010690  2012      56     3      2        3 80353     2012       NA
      181 40404916010690  2011      56     3      2        3 85420     2011       NA
      186 40404921010690  2013      56     3      2        3 85403     2013       NA
      190 40404925010690  2012      56     3      2        3 87744     2012        3
          NF_SAMPLING_STATUS_CD PLOT_STATUS_CD NF_PLOT_STATUS_CD NBRCND NBRCNDSAMP
      142                     0              1                NA      2          2
      145                     0              1                NA      1          1
      152                     0              2                NA      1          1
      159                     0              1                NA      1          1
      160                     0              1                NA      1          1
      165                     0              2                NA      1          1
      169                     0              2                NA      1          1
      181                     0              2                NA      1          1
      186                     0              2                NA      1          1
      190                     0              1                NA      1          1
          NBRCNDFOR CCLIVEPLT        FORNONSAMP        PLOT_ID FORESTNUMB
      142         2      67.5    Sampled-Forest ID560200383143         02
      145         1      66.0    Sampled-Forest ID560200380153         02
      152         0       0.0 Sampled-Nonforest ID560200386397         02
      159         1      48.0    Sampled-Forest ID560200385901         02
      160         1       2.0    Sampled-Forest ID560200390221         02
      165         0       8.0 Sampled-Nonforest ID560200385144         02
      169         0       1.0 Sampled-Nonforest ID560200380353         02
      181         0      10.0 Sampled-Nonforest ID560200385420         02
      186         0       0.0 Sampled-Nonforest ID560200385403         02
      190         1      36.0    Sampled-Forest ID560200387744         02
                           geometry
      142 POINT (-937172.9 2492354)
      145 POINT (-943048.1 2487710)
      152 POINT (-928130.7 2476936)
      159 POINT (-919787.2 2464822)
      160 POINT (-914104.9 2465931)
      165 POINT (-930324.7 2461728)
      169 POINT (-905102.8 2454596)
      181 POINT (-896931.1 2444320)
      186 POINT (-906430.5 2439460)
      190 POINT (-906870.1 2435067)

