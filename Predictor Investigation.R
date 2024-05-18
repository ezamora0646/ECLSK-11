
###### ANALYSIS OF PREDICTORS #####
# first grade

frst.predictors <- frst.atl[, c("x4tchapp", "x3tchapp", "x4clsnss", "x4cnflct")] #since prelas was not readminsitered to proficient students... what to use as prof?
fst.pred.corr <- frst.predictors %>% correlation() %>% print()

frst.ch.covars <- frst.atl[, c("x4tchapp", "x3age", "x4age","x_chsex_r","x4prnapp", "x4attnfs", "x3tchext","x4tchext", "x4inbcnt")]
#spring age less NAs
frst.ch.covars.corr <- frst.ch.covars %>% correlation() %>% print()
#ATL highly correlated with atten focus (.8) & inhb cntrl (.78), moderately corr with ext fall (-.52) and spring (.-56)
#atten focus & inhb cntrl .75 corr -> only include 1 as control?

ggplot(frst.atl, aes(x_chsex_r, x4tchapp)) + geom_boxplot()
#not much variation, probably not strongly correlated

ggplot(frst.atl, aes(fst_par_ed, x4tchapp)) + geom_boxplot()
#not much variation across groups, except for grad or prof school which is lower ATL... strange

ggplot(frst.atl, aes(x4povty_i, x4tchapp)) + geom_boxplot()
#use poverty for SES bc less levels so I don't have to recode... not much variation

frst.dem.covars <- frst.atl[, c("x4tchapp", "x4povty_i", "fst_par_ed")]
ggpairs(frst.dem.covars, upper = list(continuous = wrap(ggally_cor, stars = F))) #correlation plots for multiple variables in a df

frst.atl<- frst.atl %>%
  mutate(fst_par_ed = recode(p4hig_1_i, 
                             "-1: NOT APPLICABLE" = "-1",
                             "7: 7TH GRADE OR LESS"  = "11",                                               
                            "8: 8TH GRADE"  = "11",                                                       
                            "9: 9TH GRADE"  = "11",                                                       
                            "10: 10TH GRADE"  = "11",                                                     
                            "11: 11TH GRADE"  = "11",                                                     
                            "12: 12TH GRADE BUT NO DIPLOMA" = "11", 
                             "13: HIGH SCHOOL EQUIVALENT/GED" = "12",                                     
                               "14: HIGH SCHOOL DIPLOMA" = "12",
                             "15: VOC/TECH PROGRAM AFTER HIGH SCHOOL BUT NO VOC/TECH DIPLOMA" = "13",     
                               "16: VOC/TECH PROGRAM AFTER HIGH SCHOOL DIPLOMA" = "13",                     
                            "17: SOME COLLEGE BUT NO DEGREE"= "13", 
                            "18: ASSOCIATE'S DEGREE" = "13",
                             "19: BACHELOR'S DEGREE" = "14", 
                             "20: GRADUATE OR PROFESSIONAL SCHOOL BUT NO DEGREE" = "15",                  
                               "21: MASTER'S (MA MS)" = "15",                                               
                               "22: DOCTORATE DEGREE (PHD EDD)" = "15",                                     
                               "23: PROFESSIONAL DEGREE AFTER BACHELOR'S DEGREE (MD/DDS/LAW/JD/LLB)" = "15", 
                            .default = NULL
                             ))
frst.atl$fst_par_ed <- factor(frst.atl$fst_par_ed, labels = c("Less than HS", "HS or equiv", 
                                                              "Voc/AA: degree or no degree", "Bachelors", "Grad or prof school", "Not applicable"), ordered = TRUE)
 
frst.tch.covars <- frst.atl[, c("x4tchapp", "a4nmell", "a4shisp", "a4yrsch", "a4yrstch", "a4wksgrp", "a4dscptim", "a4hghstd", 
                                "a4early", "a4esl", "a4devlp", "a4yrborn", "a4highql", "a4spnin", "a4ell")]
fst.tch.corr <- frst.tch.covars %>% correlation() %>% print()
#none of the continuous variables (# of ells, # of hisp students, years at school, total xp) correlated w ATL... so do not include in model?


ggplot(frst.atl, aes(a4wksgrp, x4tchapp)) + geom_boxplot()
ggplot(frst.atl, aes(a4dscptim, x4tchapp)) + geom_boxplot()
ggplot(frst.atl, aes(a4hghstd, x4tchapp)) + geom_boxplot()
ggplot(frst.atl, aes(a4early, x4tchapp)) + geom_boxplot()
ggplot(frst.atl, aes(a4esl, x4tchapp)) + geom_boxplot()
ggplot(frst.atl, aes(a4devlp, x4tchapp)) + geom_boxplot()
ggplot(frst.atl, aes(a4yrborn, x4tchapp)) + geom_boxplot()
ggplot(frst.atl, aes(a4highql, x4tchapp)) + geom_boxplot() #don't include
ggplot(frst.atl, aes(a4spnin, x4tchapp)) + geom_boxplot()
ggplot(frst.atl, aes(a4ell, x4tchapp)) + geom_boxplot()


###### Analysis of Predictors: Kinder ######

predictors <- kinder.atl[, c("x2tchapp", "x1tchapp", "x2pltot", "x2clsnss", "x2cnflct")]
pred.corr <- predictors %>% correlation() %>% print()

child.covars <- kinder.atl[, c("x2tchapp","x2kage_r", "x1kage_r", "x1inbcnt", "x2inbcnt", "x1attnfs", "x2attnfs", 
                               "x1tchext", "x2tchext", "x2pltot", "x1pltot", "x1tchapp")]
ch.covar.corr <- child.covars %>% correlation() %>% print()

ch.dem.covars <- kinder.atl[, c("x2tchapp","x2povty","kndr_par_ed")] #income category too many levels, use poverty... 
#will have to recode parent ed
ggpairs(ch.dem.covars, upper = list(continuous = wrap(ggally_cor, stars = F)))

kinder.atl <- kinder.atl %>% 
  mutate(kndr_par_ed = recode(p1hig_1, 
                              "-1: NOT APPLICABLE" = "-1",
                              "-7: REFUSED" = "-1",                                                        
                              "-8: DON'T KNOW" = "-1",                                                     
                              "-9: NOT ASCERTAINED" = "-1", 
                              "7: 7TH GRADE OR LESS"  = "11",                                               
                              "8: 8TH GRADE"  = "11",                                                       
                              "9: 9TH GRADE"  = "11",                                                       
                              "10: 10TH GRADE"  = "11",                                                     
                              "11: 11TH GRADE"  = "11",                                                     
                              "12: 12TH GRADE BUT NO DIPLOMA" = "11",
                              "13: HIGH SCHOOL DIPLOMA/EQUIVALENT" = "12",                                     
                              "14: HIGH SCHOOL DIPLOMA" = "12",
                              "15: VOC/TECH PROG AFTER HIGH SCHOOL NO DIPLOMA"= "13",     
                              "16: VOC/TECH PROGRAM AFTER HIGH SCHOOL" = "13",                     
                              "17: SOME COLLEGE BUT NO DEGREE"= "13", 
                              "18: ASSOCIATE'S DEGREE" = "13",
                              "19: BACHELOR'S DEGREE" = "14", 
                              "20: GRADUATE/PROFESSIONAL SCHOOL - NO DEGREE" = "15",                  
                              "21: MASTER'S DEGREE (MA, MS)" = "15",                                               
                              "22: DOCTORATE DEGREE (PH.D., ED.D.)" = "15",                                     
                              "23: PROFESSIONAL DEGREE AFTER BACHELOR'S DEGREE (MD/DDS/LAW/JD/LLB)" = "15",
                              .default = NULL
                              ))

kinder.atl$kndr_par_ed <- factor(kinder.atl$kndr_par_ed, labels = c("Less than HS", "HS or equiv", 
                                       "Voc/AA: degree or no degree", "Bachelors", "Grad or prof school", "Not applicable"), ordered = TRUE)


###### Distributions of kinder predictors #####
ggplot(kinder.atl, aes(x=x2tchapp)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(kinder.atl$x2tchapp, na.rm=TRUE), sd = sd(kinder.atl$x2tchapp, na.rm=TRUE)), color="red")

ggplot(kinder.atl, aes(x=x2pltot)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(kinder.atl$x2pltot, na.rm=TRUE), sd = sd(kinder.atl$x2pltot, na.rm=TRUE)), color="red")

ggplot(kinder.atl, aes(x=x2clsnss)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(kinder.atl$x2clsnss, na.rm=TRUE), sd = sd(kinder.atl$x2clsnss, na.rm=TRUE)), color="red")

ggplot(kinder.atl, aes(x=x2cnflct)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(kinder.atl$x2cnflct, na.rm=TRUE), sd = sd(kinder.atl$x2cnflct, na.rm=TRUE)), color="red")


###### REGRESSION ASSUMPTION CHECK ####### 
#fitting regression 
full.reg.ck <- lm(x2tchapp~x2kage_r+x2povty+kndr_par_ed+
                    x1tchapp+x2prnapp+x2inbcnt+x2attnfs+x2tchext+
                    a1yrstch+a1yrborn+a1hghstd+
                    x2pltot+x2clsnss+x2pltot:x2clsnss, data=dll.atl.kndr)
summary(full.reg.ck)
anova(full.reg.ck)

#checking for outliers (+/- 3 SDs from mean)
kinder.atl$x2tchappZ <- (kinder.atl$x2tchapp-mean(kinder.atl$x2tchapp, na.rm=TRUE))/sd(kinder.atl$x2tchapp, na.rm=TRUE)
kinder.atl$x2pltotZ <- (kinder.atl$x2pltot - mean(kinder.atl$x2pltot, na.rm=TRUE)) / sd(kinder.atl$x2pltot, na.rm=TRUE)
kinder.atl$x2clsnssZ <- (kinder.atl$x2clsnss - mean(kinder.atl$x2clsnss, na.rm=TRUE)) / sd(kinder.atl$x2clsnss, na.rm=TRUE)
#whole column is NA so will have to remove NAs to calculate??

#Skewness and Kurtosis w NAs removed; acceptable skewness b/t -.5 & .5 kurtosis K-3
skewness(kinder.atl$x2tchapp, na.rm=TRUE)
skewness(kinder.atl$x2pltot, na.rm=TRUE)
skewness(kinder.atl$x2clsnss, na.rm=TRUE)
kurtosis(kinder.atl$x2tchapp, na.rm=TRUE)
kurtosis(kinder.atl$x2clsnss, na.rm=TRUE)
kurtosis(kinder.atl$x2clsnss, na.rm=TRUE)

#linearity & Homoscedacity 

#linearity of predictors 
pairs(predictors[,1:4], pch=19)
pairs(child.covars[,1:6], pch=19)


#homoscedacity
predicted <- fitted(full.reg.ck)
resids <- residuals(full.reg.ck)
plot(predicted, resids, xlab="Predicted Values", ylab="Residuals", pch=20)
abline(h=0, col="blue")

#Normality w PP Plot 
rstandard <- (resids - mean(resids))/sd(resids)
qqnorm(rstandard)
qqline(rstandard)

#residuals vs IVs 
plot(kinder.atl$x2pltot, resids, xlab="Total preLAS", ylab="Residuals", pch=25)
abline(h = 0, col = "blue")
plot(kinder.atl$x2clsnss, resids, xlab= "Closeness", ylab="Residuals", pch=25)
abline(h = 0, col = "blue")
# i get an error... possibly due to NAs??

#histogram of standardized residuals 
hist(rstandard, breaks=50, prob=TRUE)
curve(dnorm(x, mean=mean(rstandard), sd(rstandard)), add=TRUE)

#multicollinearity
vif(full.reg.ck, type = "predictor")
1/vif(full.reg.ck,type = "predictor")
#appears to sugges that inhibitory control and attentional focus present multicolinearity issues but not above threshold >5
#makes sense tho bc the two variables were highly correlated with each other 

#keep atten foc since it is more correlated w ATL than inhb cntr by.02... test model fit with and without for both?

#year born and years XP may also be presenting multicolinearity issues... 
#should keep years XP since it may be more correlated with ATL and less levels