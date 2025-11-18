# difNLR - examples at help page

    Code
      (fit1 <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  82.0689      0.0000 ***
      Item2  28.3232      0.0000 ***
      Item3   0.6845      0.7102    
      Item4   3.3055      0.1915    
      Item5   1.1984      0.5492    
      Item6   0.1573      0.9244    
      Item7   8.3032      0.0157 *  
      Item8   2.8660      0.2386    
      Item9   0.4549      0.7966    
      Item10  1.3507      0.5090    
      Item11  1.2431      0.5371    
      Item12  1.0537      0.5905    
      Item13  4.4139      0.1100    
      Item14  1.4940      0.4738    
      Item15  1.3079      0.5200    
      Item16  0.1424      0.9313    
      Item17  3.1673      0.2052    
      Item18  2.0206      0.3641    
      Item19  6.2546      0.0438 *  
      Item20  3.4871      0.1749    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7
       Item19

---

    Code
      AIC(fit1)
    Output
       [1] 2515.030 2549.043 2342.008 2013.198 1670.650 2455.279 2518.547 2598.912
       [9] 2693.296 2695.274 2371.996 2694.215 2221.522 2624.144 2737.446 2527.643
      [17] 2738.945 2484.224 2607.867 2613.890

---

    Code
      BIC(fit1)
    Output
       [1] 2543.035 2577.048 2358.811 2030.000 1687.453 2472.082 2546.552 2615.714
       [9] 2710.099 2712.076 2388.799 2711.018 2238.325 2640.947 2754.248 2544.446
      [17] 2755.748 2501.027 2635.871 2630.693

---

    Code
      logLik(fit1)
    Output
       [1] -1252.515 -1269.522 -1168.004 -1003.599  -832.325 -1224.639 -1254.274
       [8] -1296.456 -1343.648 -1344.637 -1182.998 -1344.108 -1107.761 -1309.072
      [15] -1365.723 -1260.821 -1366.473 -1239.112 -1298.933 -1303.945

---

    Code
      AIC(fit1, item = 1)
    Output
      [1] 2515.03

---

    Code
      BIC(fit1, item = 1)
    Output
      [1] 2543.035

---

    Code
      logLik(fit1, item = 1)
    Output
      'log Lik.' -1252.515 (df=5)

---

    Code
      (fit2 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "W"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression Wald test statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  41.9145      0.0000 ***
      Item2  14.7397      0.0006 ***
      Item3   0.6903      0.7081    
      Item4   2.8622      0.2390    
      Item5   1.0890      0.5801    
      Item6   0.1547      0.9256    
      Item7   5.6898      0.0581 .  
      Item8   2.4127      0.2993    
      Item9   0.4219      0.8098    
      Item10  1.1699      0.5571    
      Item11  1.2022      0.5482    
      Item12  1.0143      0.6022    
      Item13  3.8549      0.1455    
      Item14  1.4149      0.4929    
      Item15  1.1084      0.5745    
      Item16  0.1394      0.9327    
      Item17  2.5649      0.2774    
      Item18  1.9413      0.3788    
      Item19  4.8067      0.0904 .  
      Item20  3.1155      0.2106    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2

---

    Code
      (fit3 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "F"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression F-test statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             F-value P-value    
      Item1  41.7833  0.0000 ***
      Item2  14.2267  0.0000 ***
      Item3   0.3414  0.7108    
      Item4   1.6500  0.1923    
      Item5   0.5979  0.5501    
      Item6   0.0784  0.9246    
      Item7   4.1498  0.0159 *  
      Item8   1.4304  0.2395    
      Item9   0.2269  0.7970    
      Item10  0.6739  0.5098    
      Item11  0.6202  0.5379    
      Item12  0.5257  0.5912    
      Item13  2.2039  0.1106    
      Item14  0.7454  0.4747    
      Item15  0.6525  0.5208    
      Item16  0.0710  0.9314    
      Item17  1.5809  0.2060    
      Item18  1.0083  0.3650    
      Item19  3.1244  0.0442 *  
      Item20  1.7407  0.1757    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 3.0002 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7
       Item19

---

    Code
      (fit4 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", sandwich = TRUE))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  82.0689      0.0000 ***
      Item2  28.3232      0.0000 ***
      Item3   0.6845      0.7102    
      Item4   3.3055      0.1915    
      Item5   1.1984      0.5492    
      Item6   0.1573      0.9244    
      Item7   8.3032      0.0157 *  
      Item8   2.8660      0.2386    
      Item9   0.4549      0.7966    
      Item10  1.3507      0.5090    
      Item11  1.2431      0.5371    
      Item12  1.0537      0.5905    
      Item13  4.4139      0.1100    
      Item14  1.4940      0.4738    
      Item15  1.3079      0.5200    
      Item16  0.1424      0.9313    
      Item17  3.1673      0.2052    
      Item18  2.0206      0.3641    
      Item19  6.2546      0.0438 *  
      Item20  3.4871      0.1749    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7
       Item19

---

    Code
      (fit5 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", p.adjust.method = "BH")
      )
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      Multiple comparisons made with Benjamini-Hochberg adjustment of p-values
      
             Chisq-value P-value Adj. P-value    
      Item1  82.0689      0.0000  0.0000      ***
      Item2  28.3232      0.0000  0.0000      ***
      Item3   0.6845      0.7102  0.8355         
      Item4   3.3055      0.1915  0.5131         
      Item5   1.1984      0.5492  0.7323         
      Item6   0.1573      0.9244  0.9313         
      Item7   8.3032      0.0157  0.1049         
      Item8   2.8660      0.2386  0.5302         
      Item9   0.4549      0.7966  0.8851         
      Item10  1.3507      0.5090  0.7323         
      Item11  1.2431      0.5371  0.7323         
      Item12  1.0537      0.5905  0.7381         
      Item13  4.4139      0.1100  0.4401         
      Item14  1.4940      0.4738  0.7323         
      Item15  1.3079      0.5200  0.7323         
      Item16  0.1424      0.9313  0.9313         
      Item17  3.1673      0.2052  0.5131         
      Item18  2.0206      0.3641  0.7282         
      Item19  6.2546      0.0438  0.2192         
      Item20  3.4871      0.1749  0.5131         
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2

---

    Code
      (fit6 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", purify = TRUE))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was applied with 2 iterations.
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  78.1279      0.0000 ***
      Item2  27.7354      0.0000 ***
      Item3   0.0027      0.9986    
      Item4   2.0028      0.3674    
      Item5   1.8701      0.3926    
      Item6   1.1317      0.5679    
      Item7   4.2269      0.1208    
      Item8   3.8019      0.1494    
      Item9   0.2162      0.8975    
      Item10  0.3917      0.8221    
      Item11  3.1631      0.2057    
      Item12  0.2259      0.8932    
      Item13  2.4394      0.2953    
      Item14  0.4483      0.7992    
      Item15  0.4197      0.8107    
      Item16  1.0382      0.5950    
      Item17  1.4373      0.4874    
      Item18  0.1553      0.9253    
      Item19  3.1764      0.2043    
      Item20  5.3659      0.0684 .  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2

---

    Code
      (fit7a <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "score"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  82.0689      0.0000 ***
      Item2  28.3232      0.0000 ***
      Item3   0.6845      0.7102    
      Item4   3.3055      0.1915    
      Item5   1.1984      0.5492    
      Item6   0.1573      0.9244    
      Item7   8.3032      0.0157 *  
      Item8   2.8660      0.2386    
      Item9   0.4549      0.7966    
      Item10  1.3507      0.5090    
      Item11  1.2431      0.5371    
      Item12  1.0537      0.5905    
      Item13  4.4139      0.1100    
      Item14  1.4940      0.4738    
      Item15  1.3079      0.5200    
      Item16  0.1424      0.9313    
      Item17  3.1673      0.2052    
      Item18  2.0206      0.3641    
      Item19  6.2546      0.0438 *  
      Item20  3.4871      0.1749    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7
       Item19

---

    Code
      (fit7b <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "restscore")
      )
    Message
      Trying to recalculate starting values based on bootstrapped samples... 
      The recalculation of starting values was successful. 
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  75.6719      0.0000 ***
      Item2  27.0087      0.0000 ***
      Item3   0.6859      0.7097    
      Item4   3.1385      0.2082    
      Item5   0.2006      0.9046    
      Item6   0.1170      0.9432    
      Item7   7.5484      0.0230 *  
      Item8   4.3590      0.1131    
      Item9   0.3903      0.8227    
      Item10  1.1064      0.5751    
      Item11  0.9851      0.6111    
      Item12  0.6209      0.7331    
      Item13  2.5064      0.2856    
      Item14  1.4418      0.4863    
      Item15  1.2315      0.5402    
      Item16  0.2056      0.9023    
      Item17  2.9997      0.2232    
      Item18  1.6633      0.4353    
      Item19  5.9430      0.0512 .  
      Item20  1.5849      0.4527    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7

---

    Code
      (fit7c <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "zrestscore")
      )
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  75.6719      0.0000 ***
      Item2  27.0087      0.0000 ***
      Item3   0.6859      0.7097    
      Item4   3.1385      0.2082    
      Item5   0.2006      0.9046    
      Item6   0.1170      0.9432    
      Item7   7.5484      0.0230 *  
      Item8   4.3590      0.1131    
      Item9   0.3903      0.8227    
      Item10  1.1064      0.5751    
      Item11  0.9851      0.6111    
      Item12  0.6209      0.7331    
      Item13  2.5064      0.2856    
      Item14  1.4418      0.4863    
      Item15  1.2315      0.5402    
      Item16  0.2056      0.9023    
      Item17  2.9997      0.2232    
      Item18  1.6633      0.4353    
      Item19  5.9430      0.0512 .  
      Item20  1.5849      0.4527    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7

---

    Code
      (fit7d <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  82.0689      0.0000 ***
      Item2  28.3232      0.0000 ***
      Item3   0.6845      0.7102    
      Item4   3.3055      0.1915    
      Item5   1.1984      0.5492    
      Item6   0.1573      0.9244    
      Item7   8.3032      0.0157 *  
      Item8   2.8660      0.2386    
      Item9   0.4549      0.7966    
      Item10  1.3507      0.5090    
      Item11  1.2431      0.5371    
      Item12  1.0537      0.5905    
      Item13  4.4139      0.1100    
      Item14  1.4940      0.4738    
      Item15  1.3079      0.5200    
      Item16  0.1424      0.9313    
      Item17  3.1673      0.2052    
      Item18  2.0206      0.3641    
      Item19  6.2546      0.0438 *  
      Item20  3.4871      0.1749    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7
       Item19

---

    Code
      (fit7e <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  82.0689      0.0000 ***
      Item2  28.3232      0.0000 ***
      Item3   0.6845      0.7102    
      Item4   3.3055      0.1915    
      Item5   1.1984      0.5492    
      Item6   0.1573      0.9244    
      Item7   8.3032      0.0157 *  
      Item8   2.8660      0.2386    
      Item9   0.4549      0.7966    
      Item10  1.3507      0.5090    
      Item11  1.2431      0.5371    
      Item12  1.0537      0.5905    
      Item13  4.4139      0.1100    
      Item14  1.4940      0.4738    
      Item15  1.3079      0.5200    
      Item16  0.1424      0.9313    
      Item17  3.1673      0.2052    
      Item18  2.0206      0.3641    
      Item19  6.2546      0.0438 *  
      Item20  3.4871      0.1749    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7
       Item19

---

    Code
      (fit7f <- difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  82.0689      0.0000 ***
      Item2  28.3232      0.0000 ***
      Item3   0.6845      0.7102    
      Item4   3.3055      0.1915    
      Item5   1.1984      0.5492    
      Item6   0.1573      0.9244    
      Item7   8.3032      0.0157 *  
      Item8   2.8660      0.2386    
      Item9   0.4549      0.7966    
      Item10  1.3507      0.5090    
      Item11  1.2431      0.5371    
      Item12  1.0537      0.5905    
      Item13  4.4139      0.1100    
      Item14  1.4940      0.4738    
      Item15  1.3079      0.5200    
      Item16  0.1424      0.9313    
      Item17  3.1673      0.2052    
      Item18  2.0206      0.3641    
      Item19  6.2546      0.0438 *  
      Item20  3.4871      0.1749    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7
       Item19

---

    Code
      (fit8 <- difNLR(Data, group, focal.name = 1, model = "4PLcgdg", type = "udif"))
    Output
      Detection of uniform differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 4PL model with fixed guessing and inattention parameter for groups 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  81.9549      0.0000 ***
      Item2  14.5542      0.0001 ***
      Item3   0.3153      0.5745    
      Item4   2.4851      0.1149    
      Item5   0.0007      0.9793    
      Item6   0.0716      0.7890    
      Item7   7.9741      0.0047 ** 
      Item8   0.1516      0.6970    
      Item9   0.0948      0.7582    
      Item10  0.9293      0.3350    
      Item11  0.3832      0.5359    
      Item12  0.5839      0.4448    
      Item13  2.0124      0.1560    
      Item14  1.1316      0.2874    
      Item15  0.8492      0.3568    
      Item16  0.0204      0.8864    
      Item17  2.8934      0.0889 .  
      Item18  1.7608      0.1845    
      Item19  2.1632      0.1414    
      Item20  1.6239      0.2025    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7

---

    Code
      (fit9 <- difNLR(Data, group, focal.name = 1, model = "2PL", type = "nudif"))
    Output
      Detection of non-uniform differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 2PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1   1.4694      0.2254    
      Item2  12.8761      0.0003 ***
      Item3   0.2241      0.6359    
      Item4   0.8464      0.3576    
      Item5   1.7056      0.1916    
      Item6   0.0902      0.7639    
      Item7   0.3291      0.5662    
      Item8   2.7878      0.0950 .  
      Item9   0.3482      0.5551    
      Item10  0.4159      0.5190    
      Item11  0.8829      0.3474    
      Item12  1.1279      0.2882    
      Item13  4.2949      0.0382 *  
      Item14  0.4103      0.5218    
      Item15  0.4587      0.4982    
      Item16  0.2215      0.6379    
      Item17  0.1550      0.6938    
      Item18  0.1909      0.6622    
      Item19  3.6780      0.0551 .  
      Item20  1.0065      0.3157    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items:
       Item2
       Item13

---

    Code
      (fit10 <- difNLR(Data, group, focal.name = 1, model = "4PL", constraints = "ac",
        type = "b"))
    Output
      Detection of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 4PL model  with constraints on parameters a, c 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  17.3612      0.0000 ***
      Item2  17.5307      0.0000 ***
      Item3   0.0337      0.8543    
      Item4   0.0239      0.8771    
      Item5   0.0007      0.9793    
      Item6   0.0507      0.8218    
      Item7   3.3095      0.0689 .  
      Item8   1.3556      0.2443    
      Item9   0.1936      0.6599    
      Item10  0.4760      0.4902    
      Item11  0.0073      0.9319    
      Item12  0.2910      0.5896    
      Item13  0.1490      0.6995    
      Item14  0.4523      0.5012    
      Item15  0.5203      0.4707    
      Item16  0.0204      0.8864    
      Item17  1.0202      0.3125    
      Item18  1.3394      0.2471    
      Item19  4.8954      0.0269 *  
      Item20  1.0550      0.3044    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item19

---

    Code
      (fit11 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "mle"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using maximum likelihood method 
      with the L-BFGS-B algorithm
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  74.3235      0.0000 ***
      Item2  27.1120      0.0000 ***
      Item3   0.7401      0.6907    
      Item4   3.4097      0.1818    
      Item5   0.1188      0.9423    
      Item6   0.0989      0.9517    
      Item7   7.2097      0.0272 *  
      Item8   5.5471      0.0624 .  
      Item9   0.4107      0.8144    
      Item10  1.2497      0.5353    
      Item11  0.9490      0.6222    
      Item12  0.6327      0.7288    
      Item13  2.5885      0.2741    
      Item14  1.8876      0.3891    
      Item15  1.4200      0.4916    
      Item16  0.5303      0.7671    
      Item17  3.1490      0.2071    
      Item18  1.3752      0.5028    
      Item19  5.6279      0.0600 .  
      Item20  3.1076      0.2114    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7

---

    Code
      (fit13 <- difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "plf"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model with fixed guessing for groups 
      
      Parameters were estimated using maximum likelihood method 
      with the PLF algorithm
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  73.5354      0.0000 ***
      Item2  27.6368      0.0000 ***
      Item3   0.7401      0.6907    
      Item4   3.4110      0.1817    
      Item5   0.6804      0.7116    
      Item6   0.1073      0.9478    
      Item7   7.2092      0.0272 *  
      Item8   4.9733      0.0832 .  
      Item9   0.4276      0.8075    
      Item10  1.3559      0.5076    
      Item11  1.0097      0.6036    
      Item12  1.0773      0.5835    
      Item13  3.2177      0.2001    
      Item14  2.3594      0.3074    
      Item15  1.4200      0.4916    
      Item16  0.8646      0.6490    
      Item17  3.1339      0.2087    
      Item18  1.3752      0.5028    
      Item19  5.0306      0.0808 .  
      Item20  3.7682      0.1520    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7

---

    Code
      (fit14 <- difNLR(Data, group, focal.name = 1, model = "2PL", method = "irls"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 2PL model 
      
      Parameters were estimated using maximum likelihood method 
      with the iteratively reweighted least squares algorithm
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  73.5146      0.0000 ***
      Item2  27.5609      0.0000 ***
      Item3   0.7401      0.6907    
      Item4   3.4110      0.1817    
      Item5   0.6860      0.7096    
      Item6   0.1072      0.9478    
      Item7   7.2092      0.0272 *  
      Item8   4.9704      0.0833 .  
      Item9   0.4278      0.8074    
      Item10  1.3567      0.5075    
      Item11  1.0111      0.6032    
      Item12  1.0835      0.5817    
      Item13  3.2220      0.1997    
      Item14  2.3760      0.3048    
      Item15  1.4200      0.4916    
      Item16  0.8831      0.6430    
      Item17  3.1335      0.2087    
      Item18  1.3752      0.5028    
      Item19  5.0045      0.0819 .  
      Item20  3.7580      0.1527    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item7

# testing paper code - R Journal 2020 - generated data

    Code
      head(df[, c(1:5, 16)])
    Output
        Item1 Item2 Item3 Item4 Item5 group
      1     0     1     1     1     1     0
      2     0     1     1     0     1     0
      3     0     1     0     0     1     0
      4     1     1     1     0     1     0
      5     1     1     0     1     1     0
      6     0     1     0     0     1     0

---

    Code
      (fit1 <- difNLR(DataDIF, groupDIF, focal.name = 1, model = "4PL", type = "all"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 4PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1   6.2044      0.1844    
      Item2   0.2802      0.9911    
      Item3   2.7038      0.6086    
      Item4   5.8271      0.2124    
      Item5  48.0052      0.0000 ***
      Item6   7.2060      0.1254    
      Item7   3.2390      0.5187    
      Item8  16.8991      0.0020 ** 
      Item9   2.1595      0.7064    
      Item10  4.6866      0.3210    
      Item11 69.5328      0.0000 ***
      Item12  8.1931      0.0848 .  
      Item13  2.5850      0.6295    
      Item14  2.9478      0.5666    
      Item15 20.6589      0.0004 ***
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 9.4877 (significance level: 0.05)
      
      Items detected as DIF items:
       Item5
       Item8
       Item11
       Item15

---

    Code
      round(coef(fit1, simplify = TRUE), 3)
    Output
                           a      b      c     d   aDif   bDif   cDif   dDif
      Item1 estimate   1.484  1.294  0.049 1.000  0.000  0.000  0.000  0.000
      Item1 CI2.5      0.730  0.726 -0.022 0.633  0.000  0.000  0.000  0.000
      Item1 CI97.5     2.237  1.861  0.119 1.367  0.000  0.000  0.000  0.000
      Item2 estimate   1.176  0.153  0.000 1.000  0.000  0.000  0.000  0.000
      Item2 CI2.5      0.417 -0.279 -0.242 0.751  0.000  0.000  0.000  0.000
      Item2 CI97.5     1.936  0.584  0.242 1.249  0.000  0.000  0.000  0.000
      Item3 estimate   1.281  1.766  0.001 1.000  0.000  0.000  0.000  0.000
      Item3 CI2.5      0.548  0.773 -0.063 0.381  0.000  0.000  0.000  0.000
      Item3 CI97.5     2.014  2.758  0.065 1.619  0.000  0.000  0.000  0.000
      Item4 estimate   1.450  0.421  0.000 1.000  0.000  0.000  0.000  0.000
      Item4 CI2.5      0.785  0.122 -0.128 0.802  0.000  0.000  0.000  0.000
      Item4 CI97.5     2.115  0.719  0.128 1.198  0.000  0.000  0.000  0.000
      Item5 estimate   1.965 -1.147  0.000 0.868 -0.408  0.769  0.023 -0.006
      Item5 CI2.5      0.310 -1.939 -0.602 0.780 -2.455 -0.177 -0.654 -0.188
      Item5 CI97.5     3.619 -0.356  0.602 0.955  1.640  1.715  0.699  0.177
      Item6 estimate   1.458 -0.527  0.000 0.954  0.000  0.000  0.000  0.000
      Item6 CI2.5      0.670 -0.953 -0.288 0.837  0.000  0.000  0.000  0.000
      Item6 CI97.5     2.246 -0.101  0.288 1.071  0.000  0.000  0.000  0.000
      Item7 estimate   0.888  1.392  0.000 1.000  0.000  0.000  0.000  0.000
      Item7 CI2.5     -0.076 -0.526 -0.209 0.059  0.000  0.000  0.000  0.000
      Item7 CI97.5     1.852  3.311  0.209 1.941  0.000  0.000  0.000  0.000
      Item8 estimate   1.162  1.407  0.000 0.866 -0.117  0.974  0.007  0.134
      Item8 CI2.5     -0.004 -0.078 -0.139 0.109 -1.975 -3.428 -0.173 -2.270
      Item8 CI97.5     2.329  2.892  0.139 1.622  1.741  5.375  0.186  2.539
      Item9 estimate   1.482 -1.337  0.000 0.928  0.000  0.000  0.000  0.000
      Item9 CI2.5      0.426 -2.385 -0.705 0.850  0.000  0.000  0.000  0.000
      Item9 CI97.5     2.538 -0.290  0.705 1.005  0.000  0.000  0.000  0.000
      Item10 estimate  1.375 -0.570  0.007 0.967  0.000  0.000  0.000  0.000
      Item10 CI2.5     0.572 -1.070 -0.323 0.841  0.000  0.000  0.000  0.000
      Item10 CI97.5    2.178 -0.069  0.338 1.093  0.000  0.000  0.000  0.000
      Item11 estimate  1.071 -1.027  0.000 0.969  1.173 -0.499  0.000  0.011
      Item11 CI2.5    -0.199 -2.862 -1.022 0.763 -0.948 -2.500 -1.293 -0.204
      Item11 CI97.5    2.341  0.808  1.022 1.175  3.294  1.502  1.293  0.225
      Item12 estimate  1.051  1.560  0.080 1.000  0.000  0.000  0.000  0.000
      Item12 CI2.5     0.035 -0.162 -0.056 0.141  0.000  0.000  0.000  0.000
      Item12 CI97.5    2.066  3.283  0.215 1.859  0.000  0.000  0.000  0.000
      Item13 estimate  1.009  1.348  0.084 1.000  0.000  0.000  0.000  0.000
      Item13 CI2.5    -0.013 -0.253 -0.084 0.217  0.000  0.000  0.000  0.000
      Item13 CI97.5    2.030  2.949  0.253 1.783  0.000  0.000  0.000  0.000
      Item14 estimate  1.093  1.659  0.141 1.000  0.000  0.000  0.000  0.000
      Item14 CI2.5    -0.065 -0.300  0.016 0.064  0.000  0.000  0.000  0.000
      Item14 CI97.5    2.252  3.618  0.266 1.936  0.000  0.000  0.000  0.000
      Item15 estimate  0.875 -0.565  0.000 0.945  0.205  0.348  0.000 -0.142
      Item15 CI2.5    -0.789 -2.871 -1.192 0.460 -2.042 -2.203 -1.312 -0.739
      Item15 CI97.5    2.539  1.740  1.192 1.429  2.452  2.900  1.312  0.454

---

    Code
      round(coef(fit1, SE = TRUE)[[5]], 3)
    Output
                   a      b   aDif   bDif      c   cDif     d   dDif
      estimate 1.965 -1.147 -0.408  0.769  0.000  0.023 0.868 -0.006
      SE       0.844  0.404  1.045  0.483  0.307  0.345 0.044  0.093
      CI2.5    0.310 -1.939 -2.455 -0.177 -0.602 -0.654 0.780 -0.188
      CI97.5   3.619 -0.356  1.640  1.715  0.602  0.699 0.955  0.177

---

    Code
      (fit2 <- difNLR(DataDIF, groupDIF, focal.name = 1, model = model, type = "all"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1   3.7724      0.0521 .  
      Item2   0.2487      0.8831    
      Item3   2.0373      0.3611    
      Item4   5.8151      0.1210    
      Item5  46.2121      0.0000 ***
      Item6   6.9990      0.0719 .  
      Item7   3.2390      0.3562    
      Item8  16.8991      0.0020 ** 
      Item9   2.1595      0.7064    
      Item10  4.6866      0.3210    
      Item11 69.5328      0.0000 ***
      Item12  8.1931      0.0848 .  
      Item13  2.5850      0.6295    
      Item14  2.9478      0.5666    
      Item15 20.6589      0.0004 ***
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      
      Items detected as DIF items:
       Item5
       Item8
       Item11
       Item15

---

    Code
      (fit3 <- difNLR(DataDIF, groupDIF, focal.name = 1, model = model, type = type))
    Output
      Detection of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value   
      Item1   3.7724      0.0521 . 
      Item2   0.2487      0.8831   
      Item3   2.0373      0.3611   
      Item4   5.8151      0.1210   
      Item5  10.0923      0.0015 **
      Item6   6.9990      0.0719 . 
      Item7   3.2390      0.3562   
      Item8   0.0681      0.7941   
      Item9   2.1595      0.7064   
      Item10  4.6866      0.3210   
      Item11  0.0000      1.0000   
      Item12  8.1931      0.0848 . 
      Item13  2.5850      0.6295   
      Item14  2.9478      0.5666   
      Item15  0.3552      0.5512   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      
      Items detected as DIF items:
       Item5

---

    Code
      (fit4 <- difNLR(DataDIF, groupDIF, focal.name = 1, model = model, constraints = constraints,
        type = type))
    Output
      Detection of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1   3.7724      0.0521 .  
      Item2   0.2487      0.8831    
      Item3   2.0373      0.3611    
      Item4   5.8151      0.1210    
      Item5  46.1905      0.0000 ***
      Item6   6.9990      0.0719 .  
      Item7   3.2390      0.3562    
      Item8  11.6006      0.0007 ***
      Item9   2.1595      0.7064    
      Item10  4.6866      0.3210    
      Item11 35.2213      0.0000 ***
      Item12  8.1931      0.0848 .  
      Item13  2.5850      0.6295    
      Item14  2.9478      0.5666    
      Item15 17.7716      0.0000 ***
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      
      Items detected as DIF items:
       Item5
       Item8
       Item11
       Item15

---

    Code
      (df <- data.frame(AIC = c(AIC(fit2), AIC(fit3), AIC(fit4)), BIC = c(BIC(fit2),
      BIC(fit3), BIC(fit4)), Fit = paste("fit", rep(2:4, each = 15), sep = ""), Item = as.factor(
        rep(1:15, 3))))
    Output
               AIC       BIC  Fit Item
      1   903.5972  913.4127 fit2    1
      2  1183.4245 1193.2400 fit2    2
      3   557.5339  567.3495 fit2    3
      4  1054.5443 1069.2676 fit2    4
      5  1169.9188 1199.3653 fit2    5
      6  1138.6881 1153.4114 fit2    6
      7  1035.9053 1050.6286 fit2    7
      8   641.3773  680.6393 fit2    8
      9  1004.7217 1024.3527 fit2    9
      10 1143.9437 1163.5747 fit2   10
      11  860.0053  899.2674 fit2   11
      12 1096.9640 1116.5950 fit2   12
      13 1172.5120 1192.1430 fit2   13
      14 1190.0824 1209.7134 fit2   14
      15 1311.7767 1351.0387 fit2   15
      16  903.5972  913.4127 fit3    1
      17 1183.4245 1193.2400 fit3    2
      18  557.5339  567.3495 fit3    3
      19 1054.5443 1069.2676 fit3    4
      20 1169.9188 1199.3653 fit3    5
      21 1138.6881 1153.4114 fit3    6
      22 1035.9053 1050.6286 fit3    7
      23  639.4454  673.7997 fit3    8
      24 1004.7217 1024.3527 fit3    9
      25 1143.9437 1163.5747 fit3   10
      26  858.0053  892.3596 fit3   11
      27 1096.9640 1116.5950 fit3   12
      28 1172.5120 1192.1430 fit3   13
      29 1190.0824 1209.7134 fit3   14
      30 1310.1319 1344.4861 fit3   15
      31  903.5972  913.4127 fit4    1
      32 1183.4245 1193.2400 fit4    2
      33  557.5339  567.3495 fit4    3
      34 1054.5443 1069.2676 fit4    4
      35 1165.9404 1185.5715 fit4    5
      36 1138.6881 1153.4114 fit4    6
      37 1035.9053 1050.6286 fit4    7
      38  640.6757  665.2145 fit4    8
      39 1004.7217 1024.3527 fit4    9
      40 1143.9437 1163.5747 fit4   10
      41  888.3168  912.8556 fit4   11
      42 1096.9640 1116.5950 fit4   12
      43 1172.5120 1192.1430 fit4   13
      44 1190.0824 1209.7134 fit4   14
      45 1308.6640 1333.2028 fit4   15

---

    Code
      logLik(fit3, item = 8)
    Output
      'log Lik.' -312.7227 (df=7)

---

    Code
      logLik(fit4, item = 8)
    Output
      'log Lik.' -315.3379 (df=5)

---

    Code
      predict(fit1, item = 5, group = c(0, 1), match = 0)
    Output
         item match group      prob
      1 Item5     0     0 0.7851726
      2 Item5     0     1 0.5624891

---

    Code
      fit9$difPur
    Output
            Item1 Item2 Item3 Item4 Item5 Item6
      Step0     0     0     0     0     1     1
      Step1     0     0     0     0     1     0
      Step2     0     0     0     0     1     0

---

    Code
      fit14$difPur
    Output
             Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 Item10 Item11
      Step0      0     0     0     0     1     0     0     1     0      0      1
      Step1      1     0     0     0     1     0     0     1     0      0      1
      Step2      0     0     0     0     1     0     0     1     0      0      1
      Step3      1     0     0     0     1     0     0     1     0      0      1
      Step4      0     0     0     0     1     0     0     1     0      0      1
      Step5      1     0     0     0     1     0     0     1     0      0      1
      Step6      0     0     0     0     1     0     0     1     0      0      1
      Step7      1     0     0     0     1     0     0     1     0      0      1
      Step8      0     0     0     0     1     0     0     1     0      0      1
      Step9      1     0     0     0     1     0     0     1     0      0      1
      Step10     0     0     0     0     1     0     0     1     0      0      1
             Item12
      Step0       0
      Step1       0
      Step2       0
      Step3       0
      Step4       0
      Step5       0
      Step6       0
      Step7       0
      Step8       0
      Step9       0
      Step10      0

# testing paper code - R Journal 2020 - LearningToLearn

    Code
      predict(fitex3, match = rep(c(-1, 0, 1), 2), group = rep(c(0, 1), each = 3),
      item = 1, interval = "confidence")
    Output
            item match group      prob  lwr.conf  upr.conf
      1 Item6A_9    -1     0 0.6785773 0.6188773 0.7382773
      2 Item6A_9     0     0 0.7773050 0.7269066 0.8277034
      3 Item6A_9     1     0 0.8781427 0.8114574 0.9448281
      4 Item6A_9    -1     1 0.7802954 0.7186997 0.8418912
      5 Item6A_9     0     1 0.8431037 0.7869870 0.8992204
      6 Item6A_9     1     1 0.9290799 0.8549497 1.0032100

# testing paper code - R Journal 2020 - special cases (not included)

    Code
      head(df[, c(1:5, 16)])
    Output
        Item1 Item2 Item3 Item4 Item5 group
      1     0     1     1     1     1     0
      2     0     1     1     0     1     0
      3     0     1     0     0     1     0
      4     1     1     1     0     1     0
      5     1     1     0     1     1     0
      6     0     1     0     0     1     0

---

    Code
      coef(fit12b, item = 14)
    Output
      $Item14
                       a        b          c  d
      estimate 0.9294854 1.329411 0.06799893  1
      CI2.5           NA       NA         NA NA
      CI97.5          NA       NA         NA NA
      

---

    Code
      fit12c
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression Wald test statistics
      based on 4PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value     
      Item1   43.3878      0.0000 ***
      Item2  155.8403      0.0000 ***
      Item3  153.4009      0.0000 ***
      Item4  111.2974      0.0000 ***
      Item5  533.3646      0.0000 ***
      Item6  107.9470      0.0000 ***
      Item7   11.5948      0.0206 *  
      Item8   53.3736      0.0000 ***
      Item9  453.2297      0.0000 ***
      Item10 145.8386      0.0000 ***
      Item11 910.1895      0.0000 ***
      Item12  68.7672      0.0000 ***
      Item13  28.3833      0.0000 ***
      Item14       NA          NA    
      Item15  96.6385      0.0000 ***
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 9.4877 (significance level: 0.05)
      
      Items detected as DIF items:
       Item1
       Item2
       Item3
       Item4
       Item5
       Item6
       Item7
       Item8
       Item9
       Item10
       Item11
       Item12
       Item13
       Item15

---

    Code
      fit12d
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 4PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1   2.0935      0.7186    
      Item2   0.7896      0.9398    
      Item3  17.6515      0.0014 ** 
      Item4   4.8403      0.3041    
      Item5  13.1573      0.0105 *  
      Item6   3.8714      0.4237    
      Item7   1.5778      0.8128    
      Item8  10.9093      0.0276 *  
      Item9   0.6097      0.9620    
      Item10  2.4853      0.6473    
      Item11 26.6071      0.0000 ***
      Item12  5.2838      0.2594    
      Item13  2.8016      0.5916    
      Item14      NA          NA    
      Item15 12.8104      0.0122 *  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 9.4877 (significance level: 0.05)
      
      Items detected as DIF items:
       Item3
       Item5
       Item8
       Item11
       Item15

---

    Code
      (fit15a <- difNLR(DataDIF[, -c(5, 8, 11, 15)], groupDIF, focal.name = 1, model = "4PL",
      type = "all"))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 4PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value  
      Item1  9.1344      0.0578  .
      Item2  1.1944      0.8790   
      Item3  4.5018      0.3423   
      Item4  2.9407      0.5678   
      Item6  6.2007      0.1847   
      Item7  0.8918      0.9257   
      Item9  1.2215      0.8746   
      Item10 1.6226      0.8047   
      Item12 7.2585      0.1228   
      Item13 5.6248      0.2290   
      Item14 1.5104      0.8248   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 9.4877 (significance level: 0.05)
      None of items is detected as DIF 

---

    Code
      (fit15b <- difNLR(DataDIF[, -c(5, 8, 11, 15)], groupDIF, focal.name = 1, model = "4PL",
      type = "all", purify = TRUE))
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 4PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was applied with 0 iteration.
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value  
      Item1  9.1344      0.0578  .
      Item2  1.1944      0.8790   
      Item3  4.5018      0.3423   
      Item4  2.9407      0.5678   
      Item6  6.2007      0.1847   
      Item7  0.8918      0.9257   
      Item9  1.2215      0.8746   
      Item10 1.6226      0.8047   
      Item12 7.2585      0.1228   
      Item13 5.6248      0.2290   
      Item14 1.5104      0.8248   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 9.4877 (significance level: 0.05)
      None of items is detected as DIF 

