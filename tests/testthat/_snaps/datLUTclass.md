# datLUTclass function works as expected

    Code
      testLUT_results
    Output
                PLT_CN CONDID SUBP TREE STATUSCD SPCD SPGRPCD  DIA HT TREECLCD
      1 40404729010690      1    1    1        2  113      24  7.7 18        3
      2 40404729010690      1    1    2        1   66      23 10.8 14        3
      3 40404729010690      1    1    3        2  113      24  5.2 23        3
      4 40404729010690      1    1    4        1  113      24  5.2 18        2
      5 40404729010690      1    3    1        1  113      24  8.8 21        3
      6 40404729010690      1    4    1        1  113      24  8.9 28        3
        AGENTCD STANDING_DEAD_CD VOLCFNET VOLCFGRS VOLBFNET TPA_UNADJ DRYBIO_AG
      1      10                1 1.001201 1.820365       NA  6.018046  68.32740
      2      NA               NA       NA       NA       NA  6.018046 128.28703
      3      10                1 0.466414 0.848025       NA  6.018046  40.24585
      4      NA               NA 0.630180 0.630180       NA  6.018046  40.61207
      5      NA               NA 2.491559 2.931246       NA  6.018046 144.25115
      6      NA               NA 3.824139 4.202350       NA  6.018046 182.53588
        CARBON_AG        BA DIACL2IN
      1  34.43701 0.3233677  7.0-8.9
      2  61.19291 0.6361546 9.0-10.9
      3  20.28391 0.1474762  5.0-6.9
      4  19.49379 0.1474762  5.0-6.9
      5  69.24055 0.4223578  7.0-8.9
      6  87.61722 0.4320113  7.0-8.9

---

    Code
      testLUT2_results
    Output
                PLT_CN CONDID SUBP TREE STATUSCD SPCD SPGRPCD  DIA HT TREECLCD
      1 40404729010690      1    1    1        2  113      24  7.7 18        3
      2 40404729010690      1    1    2        1   66      23 10.8 14        3
      3 40404729010690      1    1    3        2  113      24  5.2 23        3
      4 40404729010690      1    1    4        1  113      24  5.2 18        2
      5 40404729010690      1    3    1        1  113      24  8.8 21        3
      6 40404729010690      1    4    1        1  113      24  8.9 28        3
        AGENTCD STANDING_DEAD_CD VOLCFNET VOLCFGRS VOLBFNET TPA_UNADJ DRYBIO_AG
      1      10                1 1.001201 1.820365       NA  6.018046  68.32740
      2      NA               NA       NA       NA       NA  6.018046 128.28703
      3      10                1 0.466414 0.848025       NA  6.018046  40.24585
      4      NA               NA 0.630180 0.630180       NA  6.018046  40.61207
      5      NA               NA 2.491559 2.931246       NA  6.018046 144.25115
      6      NA               NA 3.824139 4.202350       NA  6.018046 182.53588
        CARBON_AG        BA  DIACL25
      1  34.43701 0.3233677 5.0-24.9
      2  61.19291 0.6361546 5.0-24.9
      3  20.28391 0.1474762 5.0-24.9
      4  19.49379 0.1474762 5.0-24.9
      5  69.24055 0.4223578 5.0-24.9
      6  87.61722 0.4320113 5.0-24.9

---

    Code
      testLUT3_results
    Output
                PLT_CN CONDID COND_NONSAMPLE_REASN_CD CONDPROP_UNADJ SUBPPROP_UNADJ
      1 40404728010690      1                      NA              1              1
      2 40404729010690      1                      NA              1              1
      3 40404730010690      1                      NA              1              1
      4 40404731010690      1                      NA              1              1
      5 40404733010690      1                      NA              1              1
      6 40404734010690      1                      NA              1              1
        MICRPROP_UNADJ MACRPROP_UNADJ OWNCD OWNGRPCD RESERVCD SITECLCD STDORGCD
      1              1             NA    46       40        0       NA       NA
      2              1             NA    46       40        0        7        0
      3              1             NA    11       10        0        6        0
      4              1             NA    22       20        0       NA       NA
      5              1             NA    46       40        0       NA       NA
      6              1             NA    22       20        0       NA       NA
        ADFORCD LIVE_CANOPY_CVR_PCT COND_STATUS_CD NF_COND_STATUS_CD FORTYPCD STDSZCD
      1      NA                   0              2                NA       NA      NA
      2      NA                  17              1                NA      366       1
      3     206                  41              1                NA      201       1
      4      NA                   0              2                NA       NA      NA
      5      NA                   0              2                NA       NA      NA
      6      NA                   0              2                NA       NA      NA
        STDAGE GSSTKCD DSTRBCD1 DSTRBYR1 DSTRBCD2 DSTRBYR2 FORTYPGRPCD TIMBERCD
      1     NA      NA       NA       NA       NA       NA          NA       NA
      2     46       5        0       NA        0       NA         360        2
      3    220       3        0       NA        0       NA         200        1
      4     NA      NA       NA       NA       NA       NA          NA       NA
      5     NA      NA       NA       NA       NA       NA          NA       NA
      6     NA      NA       NA       NA       NA       NA          NA       NA
        LIVE_CANOPY_CVR_PCTCL
      1                0-24.9
      2                0-24.9
      3               25-49.9
      4                0-24.9
      5                0-24.9
      6                0-24.9

