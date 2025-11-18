# difORD - examples at help page

    Code
      (fit1 <- difORD(Data, group, focal.name = 1, model = "adjacent"))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.2551      0.5339   
      R2   5.9526      0.0510 . 
      R3   0.0852      0.9583   
      R4   0.1258      0.9390   
      R5   1.0432      0.5936   
      R6   9.8619      0.0072 **
      R7   9.9535      0.0069 **
      R8   1.0119      0.6029   
      R9   2.8220      0.2439   
      R10  5.2412      0.0728 . 
      R11  2.5074      0.2855   
      R12  4.0344      0.1330   
      R13  1.6216      0.4445   
      R14  0.5069      0.7761   
      R15  1.6559      0.4370   
      R16  3.9444      0.1391   
      R17  1.7717      0.4124   
      R18  0.1236      0.9401   
      R19  9.1928      0.0101 * 
      R20 11.1244      0.0038 **
      R21  3.0459      0.2181   
      R22  3.7980      0.1497   
      R23  2.7844      0.2485   
      R24  0.5137      0.7735   
      R25  1.0364      0.5956   
      R26  0.9524      0.6211   
      R27  0.2938      0.8634   
      R28  4.3879      0.1115   
      R29  3.4921      0.1745   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R19
       R20

---

    Code
      AIC(fit1)
    Output
       [1]  868.7506  828.2355  762.8070 1172.1709  834.1942 1066.3088 1327.1609
       [8] 1222.1158 1355.7572  698.8149 1342.9936 1431.2480 1320.1761 1413.2859
      [15]  966.0284 1322.9039  511.0202 1571.5938  705.2756  912.1476 1287.1975
      [22] 1051.6305 1422.2094 1246.3794 1972.2709 1450.7519 1090.9833 1380.3714
      [29]  900.9954

---

    Code
      BIC(fit1)
    Output
       [1]  891.9565  851.4414  786.0129 1195.3768  857.4002 1098.7970 1359.6492
       [8] 1245.3217 1378.9631  722.0208 1366.1995 1454.4539 1343.3820 1436.4918
      [15]  989.2343 1346.1098  534.2261 1594.7997  737.7639  944.6358 1310.4034
      [22] 1074.8364 1445.4153 1269.5853 1995.4768 1473.9578 1114.1892 1403.5774
      [29]  924.2013

---

    Code
      logLik(fit1)
    Output
       [1] -429.3753 -409.1178 -376.4035 -581.0855 -412.0971 -526.1544 -656.5804
       [8] -606.0579 -672.8786 -344.4075 -666.4968 -710.6240 -655.0880 -701.6429
      [15] -478.0142 -656.4519 -250.5101 -780.7969 -345.6378 -449.0738 -638.5987
      [22] -520.8152 -706.1047 -618.1897 -981.1354 -720.3759 -540.4916 -685.1857
      [29] -445.4977

---

    Code
      AIC(fit1, item = 1)
    Output
      [1] 868.7506

---

    Code
      BIC(fit1, item = 1)
    Output
      [1] 891.9565

---

    Code
      logLik(fit1, item = 1)
    Output
      'log Lik.' -429.3753 (df=5)

---

    Code
      (fit2 <- difORD(Data, group, focal.name = 1, model = "adjacent",
        p.adjust.method = "BH"))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      Multiple comparisons made with Benjamini-Hochberg adjustment of p-values
      
          Chisq-value P-value Adj. P-value  
      R1   1.2551      0.5339  0.7832       
      R2   5.9526      0.0510  0.2957       
      R3   0.0852      0.9583  0.9583       
      R4   0.1258      0.9390  0.9583       
      R5   1.0432      0.5936  0.7832       
      R6   9.8619      0.0072  0.0698      .
      R7   9.9535      0.0069  0.0698      .
      R8   1.0119      0.6029  0.7832       
      R9   2.8220      0.2439  0.5148       
      R10  5.2412      0.0728  0.3517       
      R11  2.5074      0.2855  0.5519       
      R12  4.0344      0.1330  0.4342       
      R13  1.6216      0.4445  0.7161       
      R14  0.5069      0.7761  0.9003       
      R15  1.6559      0.4370  0.7161       
      R16  3.9444      0.1391  0.4342       
      R17  1.7717      0.4124  0.7161       
      R18  0.1236      0.9401  0.9583       
      R19  9.1928      0.0101  0.0731      .
      R20 11.1244      0.0038  0.0698      .
      R21  3.0459      0.2181  0.5148       
      R22  3.7980      0.1497  0.4342       
      R23  2.7844      0.2485  0.5148       
      R24  0.5137      0.7735  0.9003       
      R25  1.0364      0.5956  0.7832       
      R26  0.9524      0.6211  0.7832       
      R27  0.2938      0.8634  0.9583       
      R28  4.3879      0.1115  0.4342       
      R29  3.4921      0.1745  0.4600       
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      None of items is detected as DIF

---

    Code
      (fit3 <- difORD(Data, group, focal.name = 1, model = "adjacent", purify = TRUE))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was applied with 2 iterations
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.6404      0.4403   
      R2   6.7384      0.0344 * 
      R3   0.0320      0.9841   
      R4   0.0008      0.9996   
      R5   0.8556      0.6520   
      R6   7.8654      0.0196 * 
      R7   9.9131      0.0070 **
      R8   0.7217      0.6971   
      R9   2.7545      0.2523   
      R10  4.2581      0.1190   
      R11  2.8458      0.2410   
      R12  3.7298      0.1549   
      R13  1.2787      0.5276   
      R14  0.4712      0.7901   
      R15  1.9018      0.3864   
      R16  4.1030      0.1285   
      R17  2.2937      0.3176   
      R18  0.1617      0.9223   
      R19  7.4481      0.0241 * 
      R20 11.2324      0.0036 **
      R21  2.7952      0.2472   
      R22  4.2428      0.1199   
      R23  2.1986      0.3331   
      R24  0.7738      0.6792   
      R25  0.5600      0.7558   
      R26  0.6024      0.7399   
      R27  0.7387      0.6912   
      R28  4.2304      0.1206   
      R29  3.1896      0.2030   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R2
       R6
       R7
       R19
       R20

---

    Code
      (fit4 <- difORD(Data, group, focal.name = 1, model = "adjacent", type = "udif"))
    Output
      Detection of uniform Differential Item Functioning for
      ordinal data using adjacent category logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1  0.5126      0.4740    
      R2  5.9329      0.0149  * 
      R3  0.0688      0.7932    
      R4  0.0093      0.9231    
      R5  0.0026      0.9592    
      R6  9.7515      0.0018  **
      R7  5.2111      0.0224  * 
      R8  0.0037      0.9518    
      R9  2.7367      0.0981  . 
      R10 1.8363      0.1754    
      R11 2.0607      0.1511    
      R12 3.0119      0.0827  . 
      R13 1.1911      0.2751    
      R14 0.1871      0.6654    
      R15 0.7101      0.3994    
      R16 3.8040      0.0511  . 
      R17 1.5132      0.2186    
      R18 0.1219      0.7270    
      R19 6.9075      0.0086  **
      R20 9.7346      0.0018  **
      R21 2.4667      0.1163    
      R22 3.7512      0.0528  . 
      R23 1.0600      0.3032    
      R24 0.3149      0.5747    
      R25 0.3647      0.5459    
      R26 0.2691      0.6039    
      R27 0.0963      0.7563    
      R28 3.4448      0.0635  . 
      R29 3.2854      0.0699  . 
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as uniform DIF items:
       R2
       R6
       R7
       R19
       R20

---

    Code
      (fit5 <- difORD(Data, group, focal.name = 1, model = "adjacent", type = "nudif")
      )
    Output
      Detection of non-uniformDifferential Item Functioning for
      ordinal data using adjacent category logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value  
      R1  0.7425      0.3889   
      R2  0.0196      0.8886   
      R3  0.0164      0.8980   
      R4  0.1165      0.7329   
      R5  1.0406      0.3077   
      R6  0.1104      0.7397   
      R7  4.7424      0.0294  *
      R8  1.0083      0.3153   
      R9  0.0853      0.7702   
      R10 3.4049      0.0650  .
      R11 0.4467      0.5039   
      R12 1.0224      0.3119   
      R13 0.4305      0.5117   
      R14 0.3198      0.5717   
      R15 0.9458      0.3308   
      R16 0.1404      0.7079   
      R17 0.2585      0.6112   
      R18 0.0017      0.9672   
      R19 2.2852      0.1306   
      R20 1.3898      0.2384   
      R21 0.5792      0.4466   
      R22 0.0468      0.8287   
      R23 1.7244      0.1891   
      R24 0.1989      0.6557   
      R25 0.6718      0.4124   
      R26 0.6833      0.4085   
      R27 0.1974      0.6568   
      R28 0.9431      0.3315   
      R29 0.2067      0.6494   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as non-uniform DIF items:
       R7

---

    Code
      (fit6a <- difORD(Data, group, model = "adjacent", focal.name = 1, match = "score")
      )
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.2551      0.5339   
      R2   5.9526      0.0510 . 
      R3   0.0852      0.9583   
      R4   0.1258      0.9390   
      R5   1.0432      0.5936   
      R6   9.8619      0.0072 **
      R7   9.9535      0.0069 **
      R8   1.0119      0.6029   
      R9   2.8220      0.2439   
      R10  5.2412      0.0728 . 
      R11  2.5074      0.2855   
      R12  4.0344      0.1330   
      R13  1.6216      0.4445   
      R14  0.5069      0.7761   
      R15  1.6559      0.4370   
      R16  3.9444      0.1391   
      R17  1.7717      0.4124   
      R18  0.1236      0.9401   
      R19  9.1928      0.0101 * 
      R20 11.1244      0.0038 **
      R21  3.0459      0.2181   
      R22  3.7980      0.1497   
      R23  2.7844      0.2485   
      R24  0.5137      0.7735   
      R25  1.0364      0.5956   
      R26  0.9524      0.6211   
      R27  0.2938      0.8634   
      R28  4.3879      0.1115   
      R29  3.4921      0.1745   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R19
       R20

---

    Code
      (fit6b <- difORD(Data, group, model = "adjacent", focal.name = 1, match = "restscore")
      )
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.5208      0.4675   
      R2   6.3025      0.0428 * 
      R3   0.0528      0.9740   
      R4   0.1180      0.9427   
      R5   1.1060      0.5752   
      R6   9.0875      0.0106 * 
      R7  10.2041      0.0061 **
      R8   1.0272      0.5983   
      R9   2.4786      0.2896   
      R10  4.9273      0.0851 . 
      R11  2.7030      0.2589   
      R12  3.5580      0.1688   
      R13  1.5899      0.4516   
      R14  0.5471      0.7607   
      R15  1.8928      0.3881   
      R16  4.4791      0.1065   
      R17  1.8432      0.3979   
      R18  0.2343      0.8894   
      R19  8.8618      0.0119 * 
      R20 11.3123      0.0035 **
      R21  2.9970      0.2235   
      R22  4.1696      0.1243   
      R23  3.1700      0.2049   
      R24  0.7125      0.7003   
      R25  1.3713      0.5038   
      R26  0.9296      0.6282   
      R27  0.3159      0.8539   
      R28  5.0765      0.0790 . 
      R29  3.1817      0.2038   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R2
       R6
       R7
       R19
       R20

---

    Code
      (fit6c <- difORD(Data, group, model = "adjacent", focal.name = 1, match = "zrestscore")
      )
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.5208      0.4675   
      R2   6.3025      0.0428 * 
      R3   0.0528      0.9740   
      R4   0.1180      0.9427   
      R5   1.1060      0.5752   
      R6   9.0875      0.0106 * 
      R7  10.2041      0.0061 **
      R8   1.0272      0.5983   
      R9   2.4786      0.2896   
      R10  4.9273      0.0851 . 
      R11  2.7030      0.2589   
      R12  3.5580      0.1688   
      R13  1.5899      0.4516   
      R14  0.5471      0.7607   
      R15  1.8928      0.3881   
      R16  4.4791      0.1065   
      R17  1.8432      0.3979   
      R18  0.2343      0.8894   
      R19  8.8618      0.0119 * 
      R20 11.3123      0.0035 **
      R21  2.9970      0.2235   
      R22  4.1696      0.1243   
      R23  3.1700      0.2049   
      R24  0.7125      0.7003   
      R25  1.3713      0.5038   
      R26  0.9296      0.6282   
      R27  0.3159      0.8539   
      R28  5.0765      0.0790 . 
      R29  3.1817      0.2038   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R2
       R6
       R7
       R19
       R20

---

    Code
      (fit6d <- difORD(Data, group, model = "adjacent", focal.name = 1, match = match)
      )
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.2551      0.5339   
      R2   5.9526      0.0510 . 
      R3   0.0852      0.9583   
      R4   0.1258      0.9390   
      R5   1.0432      0.5936   
      R6   9.8619      0.0072 **
      R7   9.9535      0.0069 **
      R8   1.0119      0.6029   
      R9   2.8220      0.2439   
      R10  5.2412      0.0728 . 
      R11  2.5074      0.2855   
      R12  4.0344      0.1330   
      R13  1.6216      0.4445   
      R14  0.5069      0.7761   
      R15  1.6559      0.4370   
      R16  3.9444      0.1391   
      R17  1.7717      0.4124   
      R18  0.1236      0.9401   
      R19  9.1928      0.0101 * 
      R20 11.1244      0.0038 **
      R21  3.0459      0.2181   
      R22  3.7980      0.1497   
      R23  2.7844      0.2485   
      R24  0.5137      0.7735   
      R25  1.0364      0.5956   
      R26  0.9524      0.6211   
      R27  0.2938      0.8634   
      R28  4.3879      0.1115   
      R29  3.4921      0.1745   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R19
       R20

---

    Code
      (fit6e <- difORD(Data, group, model = "adjacent", focal.name = 1, match = match)
      )
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.2551      0.5339   
      R2   5.9526      0.0510 . 
      R3   0.0852      0.9583   
      R4   0.1258      0.9390   
      R5   1.0432      0.5936   
      R6   9.8619      0.0072 **
      R7   9.9535      0.0069 **
      R8   1.0119      0.6029   
      R9   2.8220      0.2439   
      R10  5.2412      0.0728 . 
      R11  2.5074      0.2855   
      R12  4.0344      0.1330   
      R13  1.6216      0.4445   
      R14  0.5069      0.7761   
      R15  1.6559      0.4370   
      R16  3.9444      0.1391   
      R17  1.7717      0.4124   
      R18  0.1236      0.9401   
      R19  9.1928      0.0101 * 
      R20 11.1244      0.0038 **
      R21  3.0459      0.2181   
      R22  3.7980      0.1497   
      R23  2.7844      0.2485   
      R24  0.5137      0.7735   
      R25  1.0364      0.5956   
      R26  0.9524      0.6211   
      R27  0.2938      0.8634   
      R28  4.3879      0.1115   
      R29  3.4921      0.1745   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R19
       R20

---

    Code
      (fit6f <- difORD(Data, group, model = "adjacent", focal.name = 1, match = match)
      )
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.2551      0.5339   
      R2   5.9526      0.0510 . 
      R3   0.0852      0.9583   
      R4   0.1258      0.9390   
      R5   1.0432      0.5936   
      R6   9.8619      0.0072 **
      R7   9.9535      0.0069 **
      R8   1.0119      0.6029   
      R9   2.8220      0.2439   
      R10  5.2412      0.0728 . 
      R11  2.5074      0.2855   
      R12  4.0344      0.1330   
      R13  1.6216      0.4445   
      R14  0.5069      0.7761   
      R15  1.6559      0.4370   
      R16  3.9444      0.1391   
      R17  1.7717      0.4124   
      R18  0.1236      0.9401   
      R19  9.1928      0.0101 * 
      R20 11.1244      0.0038 **
      R21  3.0459      0.2181   
      R22  3.7980      0.1497   
      R23  2.7844      0.2485   
      R24  0.5137      0.7735   
      R25  1.0364      0.5956   
      R26  0.9524      0.6211   
      R27  0.2938      0.8634   
      R28  4.3879      0.1115   
      R29  3.4921      0.1745   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R19
       R20

---

    Code
      (fit7 <- difORD(Data, group, focal.name = 1, model = "cumulative"))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using cumulative logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value    
      R1   0.5461      0.7611    
      R2   4.0133      0.1344    
      R3   0.4022      0.8178    
      R4   0.1243      0.9397    
      R5   0.1602      0.9230    
      R6  13.8917      0.0010 ***
      R7   9.3795      0.0092 ** 
      R8   1.2370      0.5388    
      R9   4.3732      0.1123    
      R10  6.1645      0.0459 *  
      R11  1.9460      0.3779    
      R12  3.8974      0.1425    
      R13  1.8335      0.3998    
      R14  0.7203      0.6976    
      R15  0.8203      0.6636    
      R16  3.4129      0.1815    
      R17  1.0467      0.5925    
      R18  0.9115      0.6340    
      R19  9.0748      0.0107 *  
      R20 10.6796      0.0048 ** 
      R21  5.9576      0.0509 .  
      R22  3.7256      0.1552    
      R23  2.5638      0.2775    
      R24  0.2662      0.8754    
      R25  1.1370      0.5664    
      R26  0.7860      0.6750    
      R27  0.5704      0.7519    
      R28  2.9219      0.2320    
      R29  3.5553      0.1690    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R10
       R19
       R20

# difORD - other examples

    Code
      (fit8 <- difORD(Data[, -c(2, 6, 7, 19, 20)], group, focal.name = 1))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value 
      R1  1.6404      0.4403  
      R3  0.0320      0.9841  
      R4  0.0008      0.9996  
      R5  0.8556      0.6520  
      R8  0.7217      0.6971  
      R9  2.7545      0.2523  
      R10 4.2581      0.1190  
      R11 2.8458      0.2410  
      R12 3.7298      0.1549  
      R13 1.2787      0.5276  
      R14 0.4712      0.7901  
      R15 1.9018      0.3864  
      R16 4.1030      0.1285  
      R17 2.2937      0.3176  
      R18 0.1617      0.9223  
      R21 2.7952      0.2472  
      R22 4.2428      0.1199  
      R23 2.1986      0.3331  
      R24 0.7738      0.6792  
      R25 0.5600      0.7558  
      R26 0.6024      0.7399  
      R27 0.7387      0.6912  
      R28 4.2304      0.1206  
      R29 3.1896      0.2030  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      None of items is detected as DIF

---

    Code
      (difORD(Data[, -c(2, 6, 7, 16, 19, 20, 22, 28)], group, focal.name = 1, type = "udif")
      )
    Output
      Detection of uniform Differential Item Functioning for
      ordinal data using adjacent category logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value  
      R1  1.3723      0.2414   
      R3  0.0431      0.8356   
      R4  0.1352      0.7131   
      R5  0.2152      0.6427   
      R8  0.0188      0.8910   
      R9  2.1395      0.1436   
      R10 0.7101      0.3994   
      R11 2.8381      0.0921  .
      R12 2.2716      0.1318   
      R13 0.7219      0.3955   
      R14 0.4644      0.4956   
      R15 1.5201      0.2176   
      R17 2.4646      0.1164   
      R18 0.3905      0.5321   
      R21 2.0723      0.1500   
      R23 1.8330      0.1758   
      R24 0.9434      0.3314   
      R25 0.5988      0.4390   
      R26 0.0395      0.8424   
      R27 0.5795      0.4465   
      R29 1.9700      0.1604   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      None of items is detected as uniform DIF

---

    Code
      (difORD(Data[, -c(2, 6, 7, 19, 20)], group, focal.name = 1, type = "nudif"))
    Output
      Detection of non-uniformDifferential Item Functioning for
      ordinal data using adjacent category logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value  
      R1  0.8087      0.3685   
      R3  0.0316      0.8588   
      R4  0.0005      0.9821   
      R5  0.7859      0.3753   
      R8  0.7215      0.3956   
      R9  0.1145      0.7351   
      R10 3.0934      0.0786  .
      R11 0.6004      0.4384   
      R12 0.6746      0.4114   
      R13 0.2193      0.6395   
      R14 0.2624      0.6085   
      R15 0.8815      0.3478   
      R16 0.0718      0.7887   
      R17 0.4181      0.5179   
      R18 0.0047      0.9451   
      R21 0.3873      0.5337   
      R22 0.2730      0.6013   
      R23 1.0133      0.3141   
      R24 0.3488      0.5548   
      R25 0.2105      0.6464   
      R26 0.3306      0.5653   
      R27 0.6151      0.4329   
      R28 0.6801      0.4095   
      R29 0.3372      0.5614   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      None of items is detected as non-uniform DIF

---

    Code
      (fit9 <- difORD(Data[, -c(2, 6, 7, 19)], group, focal.name = 1, purify = TRUE))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was applied with 1 iteration
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.6404      0.4403   
      R3   0.0320      0.9841   
      R4   0.0008      0.9996   
      R5   0.8556      0.6520   
      R8   0.7217      0.6971   
      R9   2.7545      0.2523   
      R10  4.2581      0.1190   
      R11  2.8458      0.2410   
      R12  3.7298      0.1549   
      R13  1.2787      0.5276   
      R14  0.4712      0.7901   
      R15  1.9018      0.3864   
      R16  4.1030      0.1285   
      R17  2.2937      0.3176   
      R18  0.1617      0.9223   
      R20 11.2324      0.0036 **
      R21  2.7952      0.2472   
      R22  4.2428      0.1199   
      R23  2.1986      0.3331   
      R24  0.7738      0.6792   
      R25  0.5600      0.7558   
      R26  0.6024      0.7399   
      R27  0.7387      0.6912   
      R28  4.2304      0.1206   
      R29  3.1896      0.2030   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R20

---

    Code
      difORD(Data[, -c(1, 2, 9)], group, focal.name = 1, purify = TRUE)
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was applied with 1 iteration
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R3   0.0821      0.9598   
      R4   0.0169      0.9916   
      R5   0.8245      0.6622   
      R6   8.4757      0.0144 * 
      R7   9.3900      0.0091 **
      R8   0.7862      0.6750   
      R10  4.9074      0.0860 . 
      R11  2.8545      0.2400   
      R12  3.9523      0.1386   
      R13  1.4053      0.4953   
      R14  0.3149      0.8543   
      R15  1.7206      0.4230   
      R16  3.8803      0.1437   
      R17  1.9476      0.3777   
      R18  0.1024      0.9501   
      R19  8.1194      0.0173 * 
      R20 10.3655      0.0056 **
      R21  2.9738      0.2261   
      R22  3.8193      0.1481   
      R23  2.1419      0.3427   
      R24  0.8296      0.6605   
      R25  0.5562      0.7572   
      R26  0.8007      0.6701   
      R27  0.7215      0.6972   
      R28  4.0558      0.1316   
      R29  3.5816      0.1668   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R19
       R20

# testing paper code - R Journal 2020 - generated data

    Code
      summary(DataORD)
    Output
       Item1   Item2   Item3   Item4   Item5       group    
       0:488   0:376   0:417   0:530   0:556   Min.   :0.0  
       1:229   1:237   1:331   1:226   1:253   1st Qu.:0.0  
       2:150   2:195   2:170   2:129   2:123   Median :0.5  
       3: 93   3:114   3: 71   3: 83   3: 47   Mean   :0.5  
       4: 40   4: 78   4: 11   4: 32   4: 21   3rd Qu.:1.0  
                                               Max.   :1.0  

---

    Code
      (fit1 <- difORD(DataORD, group = "group", focal.name = 1, model = "cumulative"))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using cumulative logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
            Chisq-value P-value   
      Item1  7.4263      0.0244 * 
      Item2 13.4267      0.0012 **
      Item3  0.6805      0.7116   
      Item4  5.6662      0.0588 . 
      Item5  2.7916      0.2476   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       Item1
       Item2

---

    Code
      (fit2 <- difORD(DataORD, group = 6, focal.name = 1, model = "adjacent"))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
            Chisq-value P-value   
      Item1  8.9024      0.0117 * 
      Item2 12.9198      0.0016 **
      Item3  1.0313      0.5971   
      Item4  4.3545      0.1134   
      Item5  2.3809      0.3041   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       Item1
       Item2

---

    Code
      coef(fit2)[[1]]
    Output
                        b1        b2       b3       b4        a       bDIF1
      estimate  0.01290771 0.6026391 1.499871 2.500063 1.776348 -0.04192627
      CI2.5    -0.11300539 0.4593669 1.316037 2.223942 1.553720 -0.15544582
      CI97.5    0.13882081 0.7459112 1.683704 2.776184 1.998976  0.07159328
                     bDIF2       bDIF3      bDIF4       aDIF
      estimate -0.12056646 -0.24021154 -0.3735864 0.27332175
      CI2.5    -0.22781499 -0.39912272 -0.6281280 0.04802373
      CI97.5   -0.01331792 -0.08130036 -0.1190447 0.49861976

---

    Code
      coef(fit2, IRTpars = FALSE)[[1]]
    Output
               (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4        x
      estimate   -0.02292859    -1.0704966     -2.664292     -4.440982 1.776348
      CI2.5      -0.24583456    -1.3476911     -3.079675     -5.059781 1.553720
      CI97.5      0.19997739    -0.7933022     -2.248909     -3.822182 1.998976
                     group    x:group
      estimate  0.08240705 0.27332175
      CI2.5    -0.13183983 0.04802373
      CI97.5    0.29665392 0.49861976

# testing paper code - R Journal 2020 - LearningToLearn

    Code
      summary(LtL6_change_ord[, 1:4])
    Output
       track    Item6A_changes Item6B_changes Item6C_changes
       BS:391   0: 33          0: 33          0: 36         
       AS:391   1:318          1:335          1:494         
                2:431          2:414          2:252         

---

    Code
      (fitex5 <- difORD(Data = LtL6_change_ord, group = "track", focal.name = "AS",
        model = "adjacent", match = zscore6))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
                     Chisq-value P-value  
      Item6A_changes 5.5961      0.0609  .
      Item6B_changes 7.8798      0.0194  *
      Item6C_changes 1.1242      0.5700   
      Item6D_changes 7.7136      0.0211  *
      Item6E_changes 8.6452      0.0133  *
      Item6F_changes 5.6488      0.0593  .
      Item6G_changes 0.8469      0.6548   
      Item6H_changes 0.6567      0.7201   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       Item6B_changes
       Item6D_changes
       Item6E_changes

