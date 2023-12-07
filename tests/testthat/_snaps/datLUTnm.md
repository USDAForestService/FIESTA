# datLUTnm works as expected

    Code
      filtered_result
    Output
                   PLT_CN CONDID COND_NONSAMPLE_REASN_CD CONDPROP_UNADJ
        1: 40404738010690      1                      NA            1.0
        2: 40404821010690      1                      NA            1.0
        3: 40404838010690      1                      NA            1.0
        4: 40404844010690      1                      NA            1.0
        5: 40404845010690      1                      NA            1.0
       ---                                                             
      143: 40406259010690      1                      NA            0.5
      144: 40406709010690      1                      NA            1.0
      145: 40407599010690      1                      NA            1.0
      146: 40407655010690      1                      NA            1.0
      147: 40406668010690      2                      NA            0.5
           SUBPPROP_UNADJ MICRPROP_UNADJ MACRPROP_UNADJ OWNCD OWNGRPCD RESERVCD
        1:            1.0            1.0             NA    31       30        0
        2:            1.0            1.0             NA    11       10        0
        3:            1.0            1.0             NA    11       10        0
        4:            1.0            1.0             NA    11       10        0
        5:            1.0            1.0             NA    11       10        0
       ---                                                                     
      143:            0.5            0.5             NA    11       10        0
      144:            1.0            1.0             NA    21       20        1
      145:            1.0            1.0             NA    21       20        1
      146:            1.0            1.0             NA    21       20        1
      147:            0.5            0.5             NA    11       10        1
           SITECLCD STDORGCD ADFORCD LIVE_CANOPY_CVR_PCT COND_STATUS_CD
        1:        7        0      NA                  17              1
        2:        5        0     206                  24              1
        3:        6        0     206                  30              1
        4:        6        0     206                  49              1
        5:        6        0     206                  22              1
       ---                                                             
      143:       NA       NA     415                   3              2
      144:        6        0      NA                   5              1
      145:        6        0      NA                  35              1
      146:        5        0      NA                  41              1
      147:        6        0     214                  25              1
           NF_COND_STATUS_CD FORTYPCD STDSZCD STDAGE GSSTKCD DSTRBCD1 DSTRBYR1
        1:                NA      366       3      5       4       10     9999
        2:                NA      266       1    121       3       10     9999
        3:                NA      901       3     20       3       10     2009
        4:                NA      266       2     82       3       10     2006
        5:                NA      281       2    129       4       10     2011
       ---                                                                    
      143:                 2       NA      NA     NA      NA       91     2011
      144:                NA      266       3      5       4       91     2010
      145:                NA      268       3     22       3       92     9999
      146:                NA      266       1    118       3       92     9999
      147:                NA      367       3     17       4       95     9999
           DSTRBCD2 DSTRBYR2 FORTYPGRPCD TIMBERCD                    DSTRB1NM
        1:        0       NA         360        2                      Insect
        2:        0       NA         260        1                      Insect
        3:        0       NA         900        1                      Insect
        4:        0       NA         260        1                      Insect
        5:        0       NA         280        1                      Insect
       ---                                                                   
      143:       NA       NA          NA       NA                   Landslide
      144:        0       NA         260        1                   Landslide
      145:        0       NA         260        1             Avalanche track
      146:        0       NA         260        1             Avalanche track
      147:        0       NA         360        1 Earth movement / avalanches

