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
large.df <- getData(data = eclsk11, varnames = c("childid", "s1_id" , "s2_id" , "s3_id" , "s4_id", "x1kage_r" , 
                                                 "x2kage_r","x3age" , "x4age", "x_chsex_r", "x1locale" , "x2locale" , "x3locale" , "x4locale",
                                                 "x2inccat_i" , "x4inccat_i", "x2povty", "x4povty_i", "p1hig_1", "p2hig_1", "p4hig_1_i", 
                                                 "x1pubpri" , "x2pubpri" , "x3pubpri" , "x4pubpri",
                                                 "x1firkdg", "t1firkdg","c1spasmt", "c2spasmt" , "c3spasmt" , "c4spasmt", "c1enghm" , "c2enghm" , 
                                                 "c3nenghm" , "c4nenghm", "c1sphome" , "c2sphome" , "c3sphome" , "c4sphome", "p1prmln1", "p1prmln2", 
                                                 "p1prmlng", "t2ennat", "t4ennat", "t4kennat","x_hisp_r", "x1tchapp" , "x2tchapp", "x3tchapp", "x4tchapp","x4ktchapp",
                                                 "x1prnapp" , "x2prnapp" , "x4prnapp", "x2clsnss" , "x2cnflct" ,"x4clsnss", "x4kclsnss","x4cnflct","x4kcnflct", 
                                                 "x1tchext", "x2tchext", "x3tchext", "x4tchext", "x4ktchext", "x1inbcnt", "x2inbcnt", "x4inbcnt", "x4kinbcnt",
                                                 "x1attnfs", "x2attnfs", "x4attnfs", "x4kattnfs", 
                                                 "x1plss" , "x2plss" , "x3plss" , "x4plss", "x1plart" , "x2plart" , "x3plart" , "x4plart",
                                                 "x1pltot" , "x2pltot" , "x3pltot" , "x4pltot", "t1keeps", "t2keeps", "t3keeps", "t4keeps", "t4kkeeps",
                                                 "t1shows", "t2shows", "t3shows", "t4shows", "t4kshows",
                                                 "t1works", "t2works", "t3works", "t4works", "t4kworks",
                                                 "t1adapts", "t2adapts", "t3adapts", "t4adapts", "t4kadapts",
                                                 "t1persis", "t2persis", "t3persis", "t4persis", "t4kpersis",
                                                 "t1atten", "t2atten", "t3atten", "t4atten", "t4katten",
                                                 "t1follow", "t2follow", "t3follow", "t4follow", "t4kfollow",
                                                 "t1class","t2nstnl", "a2smlgrp", "a4wksgrp", "a2anonen", "a2pnonen", "a2dnonen", "a4noneng", "a4nonin",
                                                 "a1timdis", "a4dscptim", "a1yrstch", "a4yrstch", "a1yrsch", "a4yrsch", "a1hghstd", "a4hghstd", "a1early", "a4early",
                                                 "a1esl", "a4esl", "a1devlp", "a4devlp", "a1yrborn", "a4yrborn", "a1highql", "a4highql",
                                                 "a1aspnin", "a1pspnin", "a1dspnin", "a4spnin", "a1atnoot", "a1ptnoot", "a1dtnoot", "a4tnoot",
                                                 "a1atspnh", "a1ptspnh", "a1dtspnh", "a4tspnh","a1ahisp", "a1phisp", "a1dhisp", "a1acspnh", "a1pcspnh", "a1dcspnh", "a4cspnh", 
                                                 "a1aell", "a1pell", "a1dell", "a2aell", "a2pell", "a2dell", "a4ell", "a1anmell", "a1pnmell", "a1dnmell", "a4nmell",
                                                 "a1ahisp", "a1phisp", "a1dhisp", "a4shisp"), 
                    addAttributes = TRUE, omittedLevels = FALSE)
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

#P1 P1 and P2 report of  PRIMARY LANG AT HOME for PARENT 1 & 2   1: ENGLISH (0); 2: SPANISH (12) etc... 
"p1prmln1", "p1prmln2", 

# P1 PLQ060 WHAT PRIMARY LANGUAGE AT HOME   1: ENGLISH (0); 2: SPANISH (12) etc... 
"p1prmlng", "p4prmlng" 

#   CHILD HISPANIC/LATINO (ANY RACE) - REV   1: YES; 2: NO; -9: NOT ASCERTAINED
"x_hisp_r" 


########                  Variables of Interest       ######### 
###         ATL     ####
# x1= Fall kinder, x2= spring kinder, x3=f fall 1st, x4= spring 1st... IMPORTANT -> X4K SPRING 1ST FOR KIDS WHO WERE RETAINED IN KINDER 
("x1tchapp" , "x2tchapp", "x3tchapp", "x4tchapp","x4ktchapp") 

###   Parent report ATL   ###
"x1prnapp" , "x2prnapp" , "x4prnapp" # no parent report for 4k; will have to check if it is included in x4 time point

###        S-T Relationships     ###
# x2 = spring Kinder, x4 spring 1st 
("x2clsnss" , "x2cnflct" ,"x4clsnss", "x4cnflct")

###   Prelas Simon Says score ### 
"x1plss" , "x2plss" , "x3plss" , "x4plss"

###  Prelas Art show score   ###
"x1plart" , "x2plart" , "x3plart" , "x4plart"

### Prelas total score ###
("x1pltot" , "x2pltot" , "x3pltot" , "x4pltot")
#no 4k data for any prelas items 
head(main_df$x1pltot, 500)


#####     Teacher Interview Child-level variables     ######
#t2 = spring K
# t4 = spring 1st grade

#porgram type (fully day= ad, morning-am, afternoon=pm)
t1class

#first time kindergartner 
t1firkdg

#Eng Native Lang 
"t2ennat", "t4ennat", "t4kennat"

#type of language instruction 
"t2nstnl", #t4klngint suppressed 

# ATL items 
"t1keeps", "t2keeps", "t3keeps", "t4keeps", "t4kkeeps"
"t1shows", "t2shows", "t3shows", "t4shows", "t4kshows"
"t1works", "t2works", "t3works", "t4works", "t4kworks"
"t1adapts", "t2adapts", "t3adapts", "t4adapts", "t4kadapts"
"t1persis", "t2persis", "t3persis", "t4persis", "t4kpersis"
"t1atten", "t2atten", "t3atten", "t4atten", "t4katten"
"t1follow", "t2follow", "t3follow", "t4follow", "t4kfollow"

# teacher level variables; a1 2010 fall, a2 2011 spring
#teacher directed small group activities
"a2smlgrp", "a4wksgrp"
#a4k suppressed

#daily use of non-eng language (am, pm, ad)
"a2anonen", "a2pnonen", "a2dnonen", "a4noneng"
"a4nonin" #teacher uses non-english lang for instructional support
#a4 broken down by subject, may just have to use teacher speaks spanish variable

##### Teacher covariates #####

#time spent on discipline
"a1timdis", "a4dscptim"

# number of years as school teacher
"a1yrstch", "a4yrstch"

#years at this school 
"a1yrsch", "a4yrsch"

#highest lvl educ achieved
"a1hghstd", "a4hghstd"

# teacher took early ed course; courses only asked about in fall for kidner 
"a1early", "a4early"

#teacher took teaching ESL courses
"a1esl", "a4esl"

#teacher took child development courses 
"a1devlp", "a4devlp"

#year born; categorical 
"a1yrborn", "a4yrborn"

#teacher is high quality 
"a1highql", "a4highql"

#Spanish used for instruction (am, pm, ad)
"a1aspnin", "a1pspnin", "a1dspnin", "a4spnin"

#teacher speaks only english (am, pm, ad)
"a1atnoot", "a1ptnoot", "a1dtnoot", "a4tnoot"

#teacher speaks spanish (am, pm, ad)
"a1atspnh", "a1ptspnh", "a1dtspnh", "a4tspnh"

# number of hispanic in k (am, pm, ad)
"a1ahisp", "a1phisp", "a1dhisp"

#students speak spanish (am, pm, ad)
"a1acspnh", "a1pcspnh", "a1dcspnh", "a4cspnh"

#is ELL (am, pm, ad) fall, spring k
"a1aell", "a1pell", "a1dell", "a2aell", "a2pell", "a2dell", "a4ell"

#number of ells in class 
"a1anmell", "a1pnmell", "a1dnmell", "a4nmell"
#number of hisp in class spring 2012
"a1ahisp", "a1phisp", "a1dhisp", "a4shisp"

#gender, certification, esl credential race/eth suppressed 
# ALL TEACHER-LEVEL INTERVIEW DATA (A4K) SUPPRESSED -> OMIT STUDENTS HELD BACK IN ANALYTIC SAMPLE?