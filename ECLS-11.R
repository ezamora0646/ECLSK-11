library(haven) # get .dta and then use read_dta 
library(EdSurvey) 
library(tidyverse)
library(labelled)

setwd('../')

ecls <- file.path('~/desktop/299/ecls')
dir.create(path='ecls')

downloadECLS_K(years=2011, root=ecls, cache =FALSE)

eclsk11 <- readECLS_K2011('~/desktop/299/ecls/ECLS_K/2011')
  
View(showCodebook(eclsk11))
View(eclsk11)

searchSDF("approaches to learning", levels = TRUE, eclsk11)
showWeights(data = eclsk11, verbose = TRUE) #once survey weight is selected, specify it using wieghtVar argument in analytic functions

main_df <- getData(data = eclsk11, varnames = colnames(eclsk11), addAttributes = TRUE, omittedLevels = FALSE) #takes data from ed.survey list to traditional R df


####        General Variables     #####
"childid" # Child ID num
"s1_id" , "s2_id" , "s3_id" , "s4_id" # Fall/Spring Kinder/1st  SCHOOL IDENTIFICATION NUMBER
"x1kage_r" , "x2kage_r" # Fall/Spring Kinder CHILD ASSESSMENT AGE(MNTHS)-REV  -9: NOT ASCERTAINED
"x3age" , "x4age"  # Fall/spring 1st COMPOSITE CHILD ASSESSMENT AGE(MNTHS)  -9: NOT ASCERTAINED
"x_chsex_r" # CHILD COMPOSITE SEX - REVISED 1: MALE; 2: FEMALE; -9: NOT ASCERTAINED
"x1locale" , "x2locale" , "x3locale" , "x4locale" # Fall/Spring Kinder/1st LOCATION TYPE OF SCHOOL  1: CITY(11, 12, 13); 2: SUBURB (21, 22, 23); 3: TOWN (31, 32, 33); 4: RURAL (41, 42, 43); -1: NOT APPLICABLE; -9: NOT ASCERTAINED
"x4par1ed_i" # X4 PARENT 1 EDUCATION LEVEL (IMPUTED)
"x2inccat_i" , "x4inccat_i"  # Spring Kinder/1st INCOME CATEGORY (IMPUTED)  
              #1: $5,000 OR LESS; 2: $5,001 TO $10,000; 3: $10,001 TO $15,000; 4: $15,001 TO $20,000; 5: $20,001 TO $25,000; 6: $25,001 TO $30,000; 
              #7: $30,001 TO $35,000; 8: $35,001 TO $40,000; 9: $40,001 TO $45,000; 10: $45,001 TO $50,000; 11: $50,001 TO $55,000; 
              # 12: $55,001 TO $60,000; 13: $60,001 TO $65,000; 14: $65,001 TO $70,000; 15: $70,001 TO $75,000; 16: $75,001 TO $100,000; 17: $100,001 TO $200,000; 18: $200,001 OR MORE; -9: NOT ASCERTAINED 
"x1pubpri" , "x2pubpri" , "x3pubpri" , "x4pubpri" # Fall/Spring Kinder/1st PUBLIC OR PRIVATE SCHOOL  1: PUBLIC; 2: PRIVATE; -1: NOT APPLICABLE; -9: NOT ASCERTAINED


#####       Sample Inclusion Criteria     ######
#   X1 FIRST-TIME KINDERGARTNER   1: YES; 2: NO; -9: NOT ASCERTAINED
"x1firkdg" 

#   Fall/Spring Kinder/1st CHILD ASSESSMENT IN SPANISH   1: YES; 2: NO; -9: NOT ASCERTAINED
"c1spasmt", "c2spasmt" , "c3spasmt" , "c4spasmt"

#   Fall/Spring Kinder/1st SPEAK NON-ENGLISH LANGUAGE AT HOME   1: YES; 2: NO; -9: NOT ASCERTAINED
"c1enghm" , "c2enghm" , "c3nenghm" , "c4nenghm"

#   Fall/spring Kinder/1st SPEAK SPANISH AT HOME   1: YES; 2: NO; -9: NOT ASCERTAINED
"c1sphome" , "c2sphome" , "c3sphome" , "c4sphome"

#P1 P1 report of  PRIMARY LANG AT HOME for PARENT 1 & 2   1: ENGLISH (0); 2: SPANISH (12) etc... 
"p1prmln1", "p1prmln2" 

# P1 PLQ060 WHAT PRIMARY LANGUAGE AT HOME   1: ENGLISH (0); 2: SPANISH (12) etc... 
"p1prmlng" 

#   CHILD HISPANIC/LATINO (ANY RACE) - REV   1: YES; 2: NO; -9: NOT ASCERTAINED
"x_hisp_r" 


########                  Variables of Interest       ######### 
###         ATL     ####
# x1= Fall kinder, x2= spring kinder, x3=f fall 1st, x4= spring 1st... IMPORTANT -> X4K SPRING 1ST FOR KIDS WHO WERE RETAINED IN KINDER 
("x1tchapp" , "x2tchapp", "x3tchapp", "x4tchapp","x4ktchapp") 

###   Parent report ATL   ###
"x1prnapp" , "x2prnapp" , "x4prnapp"

###        S-T Relationships     ###
# x2 = spring Kinder, x4 spring 1st 
("x2clsnss" , "x2cnflct" ,"x4clsnss", "x4cnflct")

###   Prelas Simon Says score ### 
"x1plss" , "x2plss" , "x3plss" , "x4plss"

###  Prelas Art show score   ###
"x1plart" , "x2plart" , "x3plart" , "x4plart"

### Prelas total score ###
("x1pltot" , "x2pltot" , "x3pltot" , "x4pltot")
head(main_df$x1pltot, 500)


#####     Teacher Interview Child-level variables     ######
#t2 = spring 2011
#Eng Native Lang 
t2ennat, 

#type of language instruction 
t2nstnl, 

# ATL items 
t1keeps, t2keeps, t3keeps, t4keeps
t1shows, t2shows, t3shows, t4shows
t1works, t2works, t3works, t4works
t1adapts, t2adapts, t3adapts, t4adapts
t1persis, t2persis, t3persis, t4persis
t1atten, t2atten, t3atten, t4atten
t1follow, t2follow, t3follow, t4follow

# teacher level variables; a1 2010 fall, a2 2011 spring
#teacher directed small group activities
a2smlgrp

#daily use of non-eng language (am, pm, ad)
a2anonen
a2pnonen
a2dnonen #is ad all day?

##### Teacher covariates #####
# number of years as school teacher
a1yrstch

#years at this school 
a1yrsch

#highest lvl educ achieved
a1hghstd

#year born; categorical 
a1yrborn

#Spanish used for instruction (am, pm, ad)
a1aspnin
a1pspnin
a1dspnin

a4spnin

#teacher speaks only english (am, pm, ad)
a1atnoot
a1ptnoot
a1dtnoot

#teacher speaks spanish (am, pm, ad)
a1atspnh
a1ptspnh
a1dtspnh

a4tspnh

#number of hisp in class spring 2012
a4shisp
#gender, certification, race/eth suppressed 