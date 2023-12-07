# datLUTspp returns expected results

    Code
      testLUT_results
    Output
             SPCD         PLT_CN CONDID SUBP TREE STATUSCD SPGRPCD  DIA HT TREECLCD
          1:   19 40404821010690      1    1    7        1      12  5.9 48        2
          2:   19 40404821010690      1    1    8        1      12  8.1 45        2
          3:   19 40404821010690      1    1   11        2      12   NA NA       NA
          4:   19 40404821010690      1    1   14        1      12 15.6 83        2
          5:   19 40404821010690      1    1   15        1      12 13.7 77        2
         ---                                                                       
      18376:  823 40405569010690      1    4    4        2      47   NA NA       NA
      18377:  823 40405569010690      1    4    5        2      47   NA NA       NA
      18378:  823 40405569010690      1    4    6        2      47   NA NA       NA
      18379:  823 40405569010690      1    4    7        1      47  1.1 10        2
      18380:  823 40405569010690      1    4    8        1      47  1.2 10        2
             AGENTCD STANDING_DEAD_CD  VOLCFNET  VOLCFGRS VOLBFNET TPA_UNADJ
          1:      NA               NA  3.162918  3.162918       NA  6.018046
          2:      NA               NA  5.921927  5.921927       NA  6.018046
          3:      70                0        NA        NA       NA        NA
          4:      NA               NA 40.301980 40.301980 245.1420  6.018046
          5:      NA               NA 29.384580 29.384580 164.8938  6.018046
         ---                                                                
      18376:      80                0        NA        NA       NA        NA
      18377:      80                0        NA        NA       NA        NA
      18378:      80                0        NA        NA       NA        NA
      18379:      NA               NA        NA        NA       NA 74.965282
      18380:      NA               NA        NA        NA       NA 74.965282
               DRYBIO_AG  CARBON_AG         BA   COMMON_NAME
          1:  124.006714  59.771236 0.18985374 subalpine fir
          2:  228.804060 110.283557 0.35783694 subalpine fir
          3:          NA         NA         NA subalpine fir
          4: 1393.211394 671.527892 1.32728544 subalpine fir
          5:  991.993188 478.140717 1.02366126 subalpine fir
         ---                                                
      18376:          NA         NA         NA       bur oak
      18377:          NA         NA         NA       bur oak
      18378:          NA         NA         NA       bur oak
      18379:    2.244426   1.061614 0.00659934       bur oak
      18380:    2.676594   1.266029 0.00785376       bur oak

---

    Code
      unique_classes
    Output
       [1] "subalpine fir"               "Utah juniper"               
       [3] "Rocky Mountain juniper"      "Engelmann spruce"           
       [5] "blue spruce"                 "whitebark pine"             
       [7] "common or two-needle pinyon" "lodgepole pine"             
       [9] "limber pine"                 "ponderosa pine"             
      [11] "Douglas-fir"                 "boxelder"                   
      [13] "paper birch"                 "curlleaf mountain-mahogany" 
      [15] "green ash"                   "plains cottonwood"          
      [17] "quaking aspen"               "narrowleaf cottonwood"      
      [19] "bur oak"                    

