list.files()

de1 <- read.csv("ElectricFish.csv")

# t-test 
t.test(de1$speciesUpstream, de1$speciesDownstream, paired = TRUE)

# interpret output 
# p value (0.0826) > alpha = 5% we fail to reject that the true difference in means is equal to 0 at an alpha = 5% significance level 
# confidence intervall: confidence intervall goes from  -3.95 to 0.27 and contains 0 what is the mean between the two groups. Therefore we fail to reject that true difference in means is equal to 0 at an alpha = 5% significance level   

# t-value -1.9096 > -2.2 ->  we fail to reject the null hypothesis that the true difference in means is equal to 0 at a 5% significance level 

# calculation and the output are the same 