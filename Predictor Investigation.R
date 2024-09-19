######## #replacing -9 with NA - prep for imputation #####
occurrences <- sapply(dll.atl.kndr, function(col) sum(col == "-9: NOT ASCERTAINED"))
print(occurrences)

dll.atl.frst$x_chsex_r[dll.atl.frst$x_chsex_r == "-9: NOT ASCERTAINED"] <- NA
dll.atl.frst$x_chsex_r <- droplevels(dll.atl.frst$x_chsex_r)


occurrences <- sapply(dll.atl.kndr, function(col) sum(col == "-9: NOT ASCERTAINED"))
print(occurrences)


dll.atl.kndr <- dll.atl.kndr %>% replace_with_na_all(condition = ~.x =="-9: NOT ASCERTAINED") %>% 
  mutate(across(where(is.factor), fct_drop))
dll.atl.frst <- dll.atl.frst %>% replace_with_na_all(condition = ~.x =="-9: NOT ASCERTAINED")

###### ATL AS BINARY OUTCOME VARIABLE ####### 
#kinder
summary(dll.atl.kndr$x2tchapp)
#Q3 is median of largest values in distribition --> 3.714
Q3 <- 3.714
med <- 3.14
avg <- 3.120

#adding vertical line at Q3 on histogram 
ggplot(dll.atl.kndr, aes(x=x2tchapp)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(dll.atl.kndr$x2tchapp, na.rm=TRUE), sd = sd(dll.atl.kndr$x2tchapp, na.rm=TRUE)), color="red") +
  geom_vline(xintercept = Q3, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "ATL Distribution for DLLs",
       x = "ATL Spring Scores",
       y = "Density")

table(dll.atl.kndr$x2tchapp >= Q3, useNA = "ifany") # 353 students with high ATL, 906 low ATL 

#binary outcome variable 
dll.atl.kndr <- dll.atl.kndr %>% mutate(cat.atl = factor(if_else(x2tchapp>=Q3,1,0, missing=NULL),
                                                         levels = c(0,1),
                                                         labels = c("Low", "High")))
table(dll.atl.kndr$cat.atl, useNA = "ifany")

# first grade ATL recoding 
summary(dll.atl.frst$x4tchapp)
fst.q3 <- 3.571

#histogram w q3 vert line
ggplot(dll.atl.frst, aes(x=x4tchapp)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(dll.atl.frst$x4tchapp, na.rm=TRUE), sd = sd(dll.atl.frst$x4tchapp, na.rm=TRUE)), color="red") +
  geom_vline(xintercept = fst.q3, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "ATL Distribution for DLLs",
       x = "1st ATL Spring Scores",
       y = "Density")

table(dll.atl.frst$x4tchapp >= fst.q3, useNA = "ifany")

#binary outcome variable 1st grade atl 
dll.atl.frst <- dll.atl.frst %>% mutate(fst.cat.atl = factor(if_else(x4tchapp>=fst.q3,1,0, missing=NULL),
                                                             levels = c(0,1),
                                                             labels = c("Low", "High")))
table(dll.atl.frst$fst.cat.atl, useNA = "ifany")

#check correlation kinder and 1st categorical variable 

###### Creating preLAS variable for first grade #####
table(kinder.atl$x1pltot, useNA = "ifany") #36 NAs... at this point these can be taken as truly missing since everyone as administered
table(kinder.atl$x2pltot, useNA = "ifany") #18 NAs truly missing... everyone was administered prelas in fall and spring
36+18

kinder.atl %>% mutate(condition = 
                        (x1pltot>=16) | (x2pltot >=16)) %>% 
  reframe(frequencies=table(condition)) 
# so 1024 students will have NA in first grade since they passed at some point in kinder

kinder.atl <- kinder.atl%>% mutate(prelas.diff = x2pltot - x1pltot) #report descriptively 

table(frst.atl$x3pltot, useNA = "ifany")
table(frst.atl$x3pltot >=16, useNA = "ifany") #19 students achieved prof at fall of first, 52 did not 
#1333 total NAs in fall of 1st - 1024 who passed in kinder = 309 true NAs 
1333-1024

table(frst.atl$x4pltot, useNA = "ifany") # 5 more NAs than in fall of 1st
table(frst.atl$x4pltot >=16, useNA = "ifany") #26 students achieved prof in spring, 40 did not  
19+26

frst.atl %>% mutate(condition = 
(x3pltot >=16 | x4pltot >=16)) %>% 
  reframe(frequencies=table(condition)) #45 students achieved prof in fall/spring of 1st

race2.sub <- race2.sub %>% mutate(frst.prof = case_when(
                      (x1pltot>=16 | x2pltot >=16) ~1, 
                      (x3pltot >=16 | x4pltot >=16) ~ 2, 
                      TRUE ~ 3))
table(race2.sub$frst.prof, useNA = "ifany")

ggplot(race2.sub, aes(x = factor(frst.prof))) +
  geom_bar() +
  labs(title = "Distribution of frst.prof",
       x = "frst.prof levels",
       y = "Count")

temp.race2.sub <- getData(data= race2.sub, varnames = c("childid", "frst.prof"), addAttributes = TRUE, dropOmittedLevels = FALSE)
#dll.atl.frst$childid <- as.integer(dll.atl.frst$childid)

dll.atl.frst <- dll.atl.frst %>% 
  left_join(dplyr::select(temp.race2.sub, childid, frst.prof), by = "childid")

dll.atl.frst <- dll.atl.frst %>% mutate(frst.prof = factor(frst.prof, levels= c(1,2,3), labels= c("In K", "In 1st", "After 1st")))
#dll.atl.frst <- subset(dll.atl.frst, select=-frst.prof)

###### ANALYSIS OF PREDICTORS ##### conducted prior to shift to logistic regression... AKA linear regression check
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


###### Recoding parent education 1st grade #####


# Step 1: Recode -1: NOT APPLICABLE to 0
frst.atl <- recode.sdf(frst.atl, recode = list(
  p4hig_1_i = list(from = c("-1: NOT APPLICABLE"), to = c("Not Applicable"))
))

# Step 2: Recode all values for 7TH GRADE OR LESS to 12TH GRADE BUT NO DIPLOMA to 11
frst.atl <- recode.sdf(frst.atl, recode = list(
  p4hig_1_i = list(from = c("7: 7TH GRADE OR LESS", "8: 8TH GRADE", "9: 9TH GRADE", 
                            "10: 10TH GRADE", "11: 11TH GRADE", "12: 12TH GRADE BUT NO DIPLOMA"), 
                   to = c("Less than HS"))
))

# Step 3: Recode HIGH SCHOOL EQUIVALENT/GED to HIGH SCHOOL DIPLOMA to 12
frst.atl <- recode.sdf(frst.atl, recode = list(
  p4hig_1_i = list(from = c("13: HIGH SCHOOL EQUIVALENT/GED", "14: HIGH SCHOOL DIPLOMA"), 
                   to = c("HS or equiv"))
))

# Step 4: Recode VOC/TECH PROGRAM and SOME COLLEGE to ASSOCIATE'S DEGREE to 13
frst.atl <- recode.sdf(frst.atl, recode = list(
  p4hig_1_i = list(from = c("15: VOC/TECH PROGRAM AFTER HIGH SCHOOL BUT NO VOC/TECH DIPLOMA", 
                            "16: VOC/TECH PROGRAM AFTER HIGH SCHOOL DIPLOMA", 
                            "17: SOME COLLEGE BUT NO DEGREE", 
                            "18: ASSOCIATE'S DEGREE"), 
                   to = c("Some College: Voc/AA"))
))

# Step 5: Recode BACHELOR'S DEGREE to 14
frst.atl <- recode.sdf(frst.atl, recode = list(
  p4hig_1_i = list(from = c("19: BACHELOR'S DEGREE"), 
                   to = c("Bachelors"))
))

# Step 6: Recode GRADUATE OR PROFESSIONAL SCHOOL to PROFESSIONAL DEGREE AFTER BACHELOR'S DEGREE to 15
frst.atl <- recode.sdf(frst.atl, recode = list(
  p4hig_1_i = list(from = c("20: GRADUATE OR PROFESSIONAL SCHOOL BUT NO DEGREE", 
                            "21: MASTER'S (MA MS)", 
                            "22: DOCTORATE DEGREE (PHD EDD)", 
                            "23: PROFESSIONAL DEGREE AFTER BACHELOR'S DEGREE (MD/DDS/LAW/JD/LLB)"), 
                   to = c("Grad or prof school"))
))



table(frst.atl$p4hig_1_i, useNA = "ifany")

#dply syntax not compatible with EdSurvey light objexts
#frst.atl<- frst.atl %>%
  #mutate(fst_par_ed = recode(p4hig_1_i, 
                   #          "-1: NOT APPLICABLE" = "0",
                  #           "7: 7TH GRADE OR LESS"  = "11",                                               
                 #           "8: 8TH GRADE"  = "11",                                                       
                #             "10: 10TH GRADE"  = "11",                                                     
              #              "11: 11TH GRADE"  = "11",                                                     
             #               "12: 12TH GRADE BUT NO DIPLOMA" = "11", 
            #                 "13: HIGH SCHOOL EQUIVALENT/GED" = "12",                                     
           #                    "14: HIGH SCHOOL DIPLOMA" = "12",
           #                  "15: VOC/TECH PROGRAM AFTER HIGH SCHOOL BUT NO VOC/TECH DIPLOMA" = "13",     
          #                     "16: VOC/TECH PROGRAM AFTER HIGH SCHOOL DIPLOMA" = "13",                     
         #                   "17: SOME COLLEGE BUT NO DEGREE"= "13", 
        #                    "18: ASSOCIATE'S DEGREE" = "13",
       #                      "19: BACHELOR'S DEGREE" = "14", 
      #                       "20: GRADUATE OR PROFESSIONAL SCHOOL BUT NO DEGREE" = "15",                  
     #                          "21: MASTER'S (MA MS)" = "15",                                               
    #                           "22: DOCTORATE DEGREE (PHD EDD)" = "15",                                     
   #                            "23: PROFESSIONAL DEGREE AFTER BACHELOR'S DEGREE (MD/DDS/LAW/JD/LLB)" = "15", 
  #                          .default = NULL
 #                            ))
#frst.atl$fst_par_ed <- factor(frst.atl$fst_par_ed, labels = c("Less than HS", "HS or equiv", 
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


kinder.atl$p1hig_1[kinder.atl$p1hig_1 == "-9: NOT ASCERTAINED"] <- NA
kinder.atl$p1hig_1[kinder.atl$p1hig_1 == "-7: REFUSED"] <- NA
kinder.atl$p1hig_1 <- droplevels(kinder.atl$p1hig_1)

kinder.atl <- recode.sdf(kinder.atl, recode = list(
  p1hig_1 = list(from = c("-8: DON'T KNOW"),
                 to = c("Not Applicable"))
))

kinder.atl <- recode.sdf(kinder.atl, recode = list(
  p1hig_1 = list(from = c("7: 7TH GRADE OR LESS",                                              
                           "8: 8TH GRADE",                                                       
                           "9: 9TH GRADE",                                                       
                           "10: 10TH GRADE",                                                     
                           "11: 11TH GRADE",                                                     
                           "12: 12TH GRADE BUT NO DIPLOMA"),
                 to = c("Less than HS"))
))

kinder.atl <- recode.sdf(kinder.atl, recode = list(
  p1hig_1 = list(from = c("13: HIGH SCHOOL DIPLOMA/EQUIVALENT",                                 
                          "14: HIGH SCHOOL DIPLOMA" ),
                 to = c("HS or equiv"))
))

kinder.atl <- recode.sdf(kinder.atl, recode = list(
  p1hig_1 = list(from = c("15: VOC/TECH PROG AFTER HIGH SCHOOL NO DIPLOMA",                     
                          "16: VOC/TECH PROGRAM AFTER HIGH SCHOOL",                             
                          "17: SOME COLLEGE BUT NO DEGREE",                                     
                          "18: ASSOCIATE'S DEGREE"),
                 to = c("Some College: Voc/AA"))
))

kinder.atl <- recode.sdf(kinder.atl, recode = list(
  p1hig_1 = list(from = c("19: BACHELOR'S DEGREE"),
                 to = c("Bachelors"))
))

kinder.atl <- recode.sdf(kinder.atl, recode = list(
  p1hig_1 = list(from = c("20: GRADUATE/PROFESSIONAL SCHOOL - NO DEGREE",                       
                          "21: MASTER'S DEGREE (MA, MS)",                                       
                          "22: DOCTORATE DEGREE (PH.D., ED.D.)",                                
                          "23: PROFESSIONAL DEGREE AFTER BACHELOR'S DEGREE (MD/DDS/LAW/JD/LLB)"),
                 to = c("Grad or prof school"))
))


table(kinder.atl$p1hig_1, useNA = "ifany")

kinder.atl <- kinder.atl %>% 
  mutate(kndr_par_ed = recode(p1hig_1, 
                              "-7: REFUSED" = NA_character_,                                                        
                              "-8: DON'T KNOW" = NA_character_,                                                     
                              "-9: NOT ASCERTAINED" = NA_character_,
                              "-1: NOT APPLICABLE" = "-1",
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
                                       "Voc/AA: degree or no degree", "Bachelors", "Grad or prof school"), ordered = TRUE)


7: 7TH GRADE OR LESS 
259 
8: 8TH GRADE 
48 
9: 9TH GRADE 
124 
10: 10TH GRADE 
48 
11: 11TH GRADE 
63 
12: 12TH GRADE BUT NO DIPLOMA 
69 
13: HIGH SCHOOL DIPLOMA/EQUIVALENT 
25 
14: HIGH SCHOOL DIPLOMA 
188 
15: VOC/TECH PROG AFTER HIGH SCHOOL NO DIPLOMA 
14 
16: VOC/TECH PROGRAM AFTER HIGH SCHOOL 
30 
17: SOME COLLEGE BUT NO DEGREE 
73 
18: ASSOCIATE'S DEGREE 
                                                                 18 
                                              19: BACHELOR'S DEGREE 
33 
20: GRADUATE/PROFESSIONAL SCHOOL - NO DEGREE 
2 
21: MASTER'S DEGREE (MA, MS) 
                                                                 11 
                                22: DOCTORATE DEGREE (PH.D., ED.D.) 
                                                                  2 
23: PROFESSIONAL DEGREE AFTER BACHELOR'S DEGREE (MD/DDS/LAW/JD/LLB) 
2 
-7: REFUSED 
6 
-8: DONT KNOW 
                                                                  4 
                                                -9: NOT ASCERTAINED 
                                                                 11 
                                                               <NA> 
                                                                374 



#kinder.atl <- subset(kinder.atl, select = -kndr_par_ed)

###### Distributions of kinder predictors #####
ggplot(kinder.atl, aes(x=x2tchapp)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(kinder.atl$x2tchapp, na.rm=TRUE), sd = sd(kinder.atl$x2tchapp, na.rm=TRUE)), color="red")

ggplot(kinder.atl, aes(x=x2pltot)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(kinder.atl$x2pltot, na.rm=TRUE), sd = sd(kinder.atl$x2pltot, na.rm=TRUE)), color="red")

ggplot(kinder.atl, aes(x=x2clsnss)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(kinder.atl$x2clsnss, na.rm=TRUE), sd = sd(kinder.atl$x2clsnss, na.rm=TRUE)), color="red")

ggplot(kinder.atl, aes(x=x2cnflct)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(kinder.atl$x2cnflct, na.rm=TRUE), sd = sd(kinder.atl$x2cnflct, na.rm=TRUE)), color="red")


###### REGRESSION ASSUMPTION CHECK ####### 
#fitting regression 
full.reg.ck <- lm(x2tchapp~x2kage_r+x2povty+kndr_par_ed+
                    x1tchapp+x2prnapp+x2inbcnt+x2attnfs+x2tchext+x2cnflct+
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
plot(predicted, resids, xlab="Predicted Values - Original", ylab="Residuals", pch=20)
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



#log transformation of variables 
dll.atl.kndr$atl_log <- log(dll.atl.kndr$x2tchapp) #still skewed after + 5, 10 and 20
describe(dll.atl.kndr$atl_log)

ggplot(dll.atl.kndr, aes(x=atl_log)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(dll.atl.kndr$atl_log, na.rm=TRUE), sd = sd(dll.atl.kndr$atl_log, na.rm=TRUE)), color="red")

dll.atl.kndr$atl_inv <- 1/(max(dll.atl.kndr$x2tchapp + 1) - dll.atl.kndr$x2tchapp)

dll.atl.kndr$atl_sqrt <- sqrt(dll.atl.kndr$x2tchapp)

ggplot(dll.atl.kndr, aes(x=atl_sqrt)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(dll.atl.kndr$atl_sqrt, na.rm=TRUE), sd = sd(dll.atl.kndr$atl_sqrt, na.rm=TRUE)), color="red")

#sqaured transformation 
dll.atl.kndr$atl_sqrd <- (dll.atl.kndr$x2tchapp)^2
ggplot(dll.atl.kndr, aes(x=atl_sqrd)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(dll.atl.kndr$atl_sqrd, na.rm=TRUE), sd = sd(dll.atl.kndr$atl_sqrd, na.rm=TRUE)), color="red")

describe(dll.atl.kndr$x2tchapp)
describe(dll.atl.kndr$atl_sqrd)


dll.atl.kndr$clsnss_sqrd <- (dll.atl.kndr$x2clsnss)^2
ggplot(dll.atl.kndr, aes(x=clsnss_sqrd)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill="white") + 
  stat_function(fun = dnorm, args = list(mean = mean(dll.atl.kndr$clsnss_sqrd, na.rm=TRUE), sd = sd(dll.atl.kndr$clsnss_sqrd, na.rm=TRUE)), color="red")

describe(dll.atl.kndr$x2clsnss)
describe(dll.atl.kndr$clsnss_sqrd)

dll.atl.kndr[,-c(#,#)] to delete columns #-# 
)]

#box-cox to ID proper transformation 
boxcox(lm(dll.atl.kndr$x2tchapp ~ 1)) #x^2 transformation
shapiro.test(dll.atl.kndr$atl_sqrd)

boxcox(lm(dll.atl.kndr$x2clsnss ~ 1)) #x^2 transformation

boxcox(lm(dll.atl.kndr$x2cnflct ~ 1)) #1/x^2 transformation


#new regression assumption check - only DV adjusted
adj.reg.chk <- lm(atl_sqrd~x2kage_r+x2povty+kndr_par_ed+
                    x1tchapp+x2prnapp+x2inbcnt+x2attnfs+x2tchext+
                    a1yrstch+a1yrborn+a1hghstd+
                    x2pltot+x2clsnss+x2pltot:x2clsnss, data=dll.atl.kndr)
summary(adj.reg.chk)
anova(adj.reg.chk)


#homoscedacity
predicted2 <- fitted(adj.reg.chk)
resids2 <- residuals(adj.reg.chk)
plot(predicted2, resids2, xlab="Predicted Values - Transformed", ylab="Residuals", pch=20)
abline(h=0, col="blue")

#Normality w PP Plot 
rstandard2 <- (resids2 - mean(resids2))/sd(resids2)
qqnorm(rstandard2)
qqline(rstandard2)
#variance could be constant within clusters... havent inputted clustered SEs
adj.se <- coeftest(adj.reg.chk, vcov. = vcovCL, cluster= ~s2_id)
adj.pred <- fitted(adj.se)


#histogram of standardized residuals 
hist(rstandard2, breaks=50, prob=TRUE)
curve(dnorm(x, mean=mean(rstandard2), sd(rstandard2)), add=TRUE)


####### principal component analysis  - RERUN after imputing #####
pca.kinder.df <-  subset(dll.atl.kndr, select= c(x2clsnss, x2cnflct)) #PCA for closeness and conflict only
# do just closeness and conflict 
# save factor from output - computed DV used in analysis: weighted factor score across 2 subscales, % of var, eigen value 
# factor 1 40% of variance, notably bigger 

library(factoextra)
comp.pca.kndr <- na.omit(pca.kinder.df)
pca.kinder.rslt <- prcomp(comp.pca.kndr, scale=TRUE)
summary(pca.kinder.rslt) #lists importance of components
names(pca.kinder.rslt) #rotation provides eigen vectors, loadings per component/variable 

pca.kinder.rslt$rotation 
eigen.val <- (pca.kinder.rslt$sdev)^2 #report when justifying composite relationship variable 

pca.kinder.rslt$x #principal component scores for each obs/ student

fviz_eig(pca.kinder.rslt, addlabels=TRUE) #explained variance per component, helps decide optimal number of components to be retained in analysis
#first component explains 65.2% of variance in linear combinations of 2 variables 
fviz_pca_biplot(pca.kinder.rslt, label = "var")



## power Analysis 
rm(logit)

logit <- glm(I(cat.atl=="High") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
               x1tchapp+x2prnapp+x2inbcnt+x2attnfs+
               a1yrstch+a1hghstd+
               x2pltot+x2cnflct+x2clsnss+x2pltot:x2clsnss, 
             data = comp.kndr,
             family="binomial")

vif(logit) #VIF checked for model accounting for conflict & without conflict

logit <- glm.cluster(data= d, 
                     formula=I(cat.atl=="High") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
                       x1tchapp+x2prnapp+x2inbcnt+x2attnfs+
                       a1yrstch+a1hghstd+
                       x2pltot+x2cnflct+x2clsnss+x2pltot:x2clsnss, 
                     cluster=d$s2_id,
                     family="binomial")

summary(logit)
exp(coefficients(logit))
pwr.f2.test(logit, u=14, f2= .3, sig.level = .05)

pwrss.z.logistic(p0 = 0.15, p1 = 0.10, alpha = .05, power = .8, distribution = "binomial")

