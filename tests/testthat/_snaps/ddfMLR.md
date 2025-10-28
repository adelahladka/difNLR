# ddfMLR - examples at help page

    Code
      (fit1 <- ddfMLR(Data, group, focal.name = 1, key))
    Output
      Detection of both types of Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  76.8561      0.0000 ***
      Item2  28.1954      0.0001 ***
      Item3   6.1443      0.4072    
      Item4   6.8119      0.3386    
      Item5   4.2511      0.6427    
      Item6   3.6133      0.7288    
      Item7   8.6239      0.1959    
      Item8  10.0779      0.1214    
      Item9  14.5444      0.0241 *  
      Item10  3.9987      0.6768    
      Item11  6.2514      0.3956    
      Item12  6.9577      0.3248    
      Item13  3.6414      0.7251    
      Item14  5.9761      0.4259    
      Item15  2.2051      0.8999    
      Item16  2.2730      0.8930    
      Item17  3.9642      0.6815    
      Item18  8.0746      0.2327    
      Item19  7.4243      0.2834    
      Item20  5.0249      0.5406    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DDF items:
       Item1
       Item2
       Item9

---

    Code
      AIC(fit1)
    Output
       [1] 4351.384 4328.702 3459.972 2861.326 2515.821 3593.464 3688.784 3845.248
       [9] 4228.551 4126.332 3140.202 4006.963 3234.644 4434.031 3684.150 4421.824
      [17] 3474.962 4456.081 4264.595 4967.780

---

    Code
      BIC(fit1)
    Output
       [1] 4418.594 4395.913 3493.577 2894.932 2549.426 3627.069 3722.389 3878.853
       [9] 4295.762 4159.937 3173.808 4040.568 3268.249 4467.636 3717.755 4455.429
      [17] 3508.568 4489.686 4298.201 5001.385

---

    Code
      logLik(fit1)
    Output
       [1] -2163.692 -2152.351 -1723.986 -1424.663 -1251.910 -1790.732 -1838.392
       [8] -1916.624 -2102.276 -2057.166 -1564.101 -1997.482 -1611.322 -2211.015
      [15] -1836.075 -2204.912 -1731.481 -2222.040 -2126.298 -2477.890

---

    Code
      AIC(fit1, item = 1)
    Output
      [1] 4351.384

---

    Code
      BIC(fit1, item = 1)
    Output
      [1] 4418.594

---

    Code
      logLik(fit1, item = 1)
    Output
      'log Lik.' -2163.692 (df=12)

---

    Code
      (fit2 <- ddfMLR(Data, group, focal.name = 1, key, p.adjust.method = "BH"))
    Output
      Detection of both types of Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was not applied
      Multiple comparisons made with Benjamini-Hochberg adjustment of p-values
      
             Chisq-value P-value Adj. P-value    
      Item1  76.8561      0.0000  0.0000      ***
      Item2  28.1954      0.0001  0.0009      ***
      Item3   6.1443      0.4072  0.7098         
      Item4   6.8119      0.3386  0.7098         
      Item5   4.2511      0.6427  0.8098         
      Item6   3.6133      0.7288  0.8098         
      Item7   8.6239      0.1959  0.7098         
      Item8  10.0779      0.1214  0.6070         
      Item9  14.5444      0.0241  0.1607         
      Item10  3.9987      0.6768  0.8098         
      Item11  6.2514      0.3956  0.7098         
      Item12  6.9577      0.3248  0.7098         
      Item13  3.6414      0.7251  0.8098         
      Item14  5.9761      0.4259  0.7098         
      Item15  2.2051      0.8999  0.8999         
      Item16  2.2730      0.8930  0.8999         
      Item17  3.9642      0.6815  0.8098         
      Item18  8.0746      0.2327  0.7098         
      Item19  7.4243      0.2834  0.7098         
      Item20  5.0249      0.5406  0.8098         
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DDF items:
       Item1
       Item2

---

    Code
      (fit3 <- ddfMLR(Data, group, focal.name = 1, key, purify = TRUE))
    Output
      Detection of both types of Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was applied with 1 iteration
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  75.9559      0.0000 ***
      Item2  27.1791      0.0001 ***
      Item3   6.7634      0.3433    
      Item4   6.7449      0.3451    
      Item5   4.8937      0.5575    
      Item6   5.4903      0.4826    
      Item7   5.1100      0.5298    
      Item8   9.1139      0.1673    
      Item9  13.8481      0.0314 *  
      Item10  2.5269      0.8654    
      Item11  6.6844      0.3510    
      Item12  8.0727      0.2328    
      Item13  2.6738      0.8485    
      Item14  5.7490      0.4519    
      Item15  1.2392      0.9749    
      Item16  2.4622      0.8727    
      Item17  2.2130      0.8991    
      Item18  6.8336      0.3365    
      Item19  5.9072      0.4337    
      Item20  7.6451      0.2653    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DDF items:
       Item1
       Item2
       Item9

---

    Code
      (fit4 <- ddfMLR(Data, group, key, focal.name = 1, type = "udif"))
    Output
      Detection of uniform Differential Distractor Functioning
      using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  74.7785      0.0000 ***
      Item2  15.0722      0.0018 ** 
      Item3   0.6117      0.8937    
      Item4   3.1795      0.3648    
      Item5   1.3389      0.7199    
      Item6   0.2546      0.9683    
      Item7   7.4301      0.0594 .  
      Item8   2.7892      0.4253    
      Item9   8.6917      0.0337 *  
      Item10  1.6848      0.6403    
      Item11  1.6566      0.6466    
      Item12  5.8329      0.1200    
      Item13  1.5170      0.6783    
      Item14  2.6428      0.4500    
      Item15  1.2535      0.7402    
      Item16  0.5613      0.9052    
      Item17  3.8170      0.2819    
      Item18  7.9086      0.0479 *  
      Item19  1.7105      0.6346    
      Item20  3.6070      0.3072    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as uniform DDF items:
       Item1
       Item2
       Item9
       Item18

---

    Code
      (fit5 <- ddfMLR(Data, group, key, focal.name = 1, type = "nudif"))
    Output
      Detection of non-uniform Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value   
      Item1   2.0776      0.5565   
      Item2  13.1232      0.0044 **
      Item3   5.5326      0.1367   
      Item4   3.6324      0.3040   
      Item5   2.9123      0.4054   
      Item6   3.3587      0.3396   
      Item7   1.1938      0.7545   
      Item8   7.2887      0.0632 . 
      Item9   5.8527      0.1190   
      Item10  2.3139      0.5099   
      Item11  4.5949      0.2040   
      Item12  1.1248      0.7711   
      Item13  2.1244      0.5470   
      Item14  3.3333      0.3430   
      Item15  0.9516      0.8130   
      Item16  1.7117      0.6343   
      Item17  0.1473      0.9856   
      Item18  0.1660      0.9829   
      Item19  5.7138      0.1264   
      Item20  1.4180      0.7013   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as non-uniform DDF items:
       Item2

---

    Code
      (fit6 <- ddfMLR(Data, group, key, focal.name = 1, match = "score"))
    Output
      Detection of both types of Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  76.8561      0.0000 ***
      Item2  28.1954      0.0001 ***
      Item3   6.1443      0.4072    
      Item4   6.8119      0.3386    
      Item5   4.2511      0.6427    
      Item6   3.6133      0.7288    
      Item7   8.6239      0.1959    
      Item8  10.0779      0.1214    
      Item9  14.5445      0.0241 *  
      Item10  3.9987      0.6768    
      Item11  6.2523      0.3955    
      Item12  6.9576      0.3248    
      Item13  3.6414      0.7251    
      Item14  5.9761      0.4259    
      Item15  2.2053      0.8999    
      Item16  2.2730      0.8930    
      Item17  3.9642      0.6815    
      Item18  8.0746      0.2327    
      Item19  7.4241      0.2834    
      Item20  5.0249      0.5406    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DDF items:
       Item1
       Item2
       Item9

# ddfMLR - other examples

    Code
      (fit7 <- ddfMLR(Data[, -c(1, 2, 9)], group, key[-c(1, 2, 9)], focal.name = 1))
    Output
      Detection of both types of Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value 
      Item3  6.7634      0.3433  
      Item4  6.7449      0.3451  
      Item5  4.8937      0.5575  
      Item6  5.4903      0.4826  
      Item7  5.1100      0.5298  
      Item8  9.1139      0.1673  
      Item10 2.5269      0.8654  
      Item11 6.6844      0.3510  
      Item12 8.0727      0.2328  
      Item13 2.6738      0.8485  
      Item14 5.7490      0.4519  
      Item15 1.2392      0.9749  
      Item16 2.4622      0.8727  
      Item17 2.2130      0.8991  
      Item18 6.8336      0.3365  
      Item19 5.9072      0.4337  
      Item20 7.6451      0.2653  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      None of items is detected as DDF

---

    Code
      ddfMLR(Data[, -c(1, 2, 9)], group, key[-c(1, 2, 9)], focal.name = 1, purify = TRUE)
    Output
      Detection of both types of Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was applied with 0 iteration
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value 
      Item3  6.7634      0.3433  
      Item4  6.7449      0.3451  
      Item5  4.8937      0.5575  
      Item6  5.4903      0.4826  
      Item7  5.1100      0.5298  
      Item8  9.1139      0.1673  
      Item10 2.5269      0.8654  
      Item11 6.6844      0.3510  
      Item12 8.0727      0.2328  
      Item13 2.6738      0.8485  
      Item14 5.7490      0.4519  
      Item15 1.2392      0.9749  
      Item16 2.4622      0.8727  
      Item17 2.2130      0.8991  
      Item18 6.8336      0.3365  
      Item19 5.9072      0.4337  
      Item20 7.6451      0.2653  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      None of items is detected as DDF

# testing paper code - R Journal 2020 - generated data

    Code
      head(DataDDF)
    Output
        Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 Item10 group
      1     B     B     C     A     C     B     B     D     B      B     0
      2     C     A     B     A     C     C     B     B     C      C     0
      3     B     C     C     B     C     C     B     C     B      D     0
      4     B     A     C     A     C     B     A     B     B      B     0
      5     B     B     C     B     C     B     A     C     A      B     0
      6     B     A     A     A     A     B     B     A     A      A     0

---

    Code
      (fit1 <- ddfMLR(DataDDF, group = "group", focal.name = 1, key = rep("A", 10)))
    Output
      Detection of both types of Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1  29.5508      0.0000 ***
      Item2   1.1136      0.8921    
      Item3   1.0362      0.9043    
      Item4   4.1345      0.3881    
      Item5   7.4608      0.1134    
      Item6  47.0701      0.0000 ***
      Item7   1.3285      0.9701    
      Item8   2.3629      0.8835    
      Item9  10.4472      0.1070    
      Item10  3.5602      0.7359    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DDF items:
       Item1
       Item6

# testing paper code - R Journal 2020 - LearningToLearn

    Code
      summary(LtL6_change[, 1:4])
    Output
       track    Item6A_changes Item6B_changes Item6C_changes
       BS:391   00:113         00:186         00:465        
       AS:391   10: 33         10: 33         10: 36        
                01:431         01:414         01:252        
                11:205         11:149         11: 29        

---

    Code
      (fitex4 <- ddfMLR(Data = LtL6_change, group = "track", focal.name = "AS", key = rep(
        "11", 8), match = zscore6))
    Output
      Detection of both types of Differential Distractor
      Functioning using multinomial log-linear regression model
      
      Likelihood-ratio chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
                     Chisq-value P-value  
      Item6A_changes  7.7521      0.2568  
      Item6B_changes 15.1084      0.0194 *
      Item6C_changes  2.2369      0.8967  
      Item6D_changes 10.7788      0.0955 .
      Item6E_changes 13.1187      0.0412 *
      Item6F_changes  7.9567      0.2413  
      Item6G_changes  9.5489      0.1450  
      Item6H_changes  6.6121      0.3582  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DDF items:
       Item6B_changes
       Item6E_changes

