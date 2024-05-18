#conditional statements for sample inclusion criteria 
library(haven) # get .dta and then use read_dta 
library(EdSurvey) 
library(tidyverse)
library(labelled)
library(naniar)
#library(pwr)#power analysis
library(finalfit) #missing value patterns
library(MissMech)
library(correlation)#can run pairwise correlations of vars in a df 
library(psych)
library(GGally)#correlation plots for multiple variables in a df

large.df <- read.csv(file.choose())

###### INVESTIGATING VARIBALES #####
#typeof() to investigate var type and determine whether it needs recoding before conditional statements
#if need recoding then make new df without var attr

typeof(large.df$x_hisp_r)  
c1spasmt, n=20)

nrow(large.df[large.df$c1spasmt == 1,]) #counts # of observations where value =1; using $ should include NA values
nrow(large.df[large.df$c2spasmt == 1,]) #counts # of observations where value =1
nrow(large.df[large.df$c3spasmt == 1,]) #counts # of observations where value =1
nrow(large.df[large.df$c4spasmt == 1,]) #counts # of observations where value =1

nrow(large.df[large.df$x_raceth_r == 3,]) #4207
nrow(large.df[large.df$x_raceth_r == 4,]) #385
nrow(large.df[large.df$x_hisp_r == 1,]) #4324; 268 difference between two variables 

nrow(large.df[large.df$x_racethp_r == 3,]) #4192
nrow(large.df[large.df$x_racethp_r == 4,]) #132; total 4324; race from parent int matches x_hisp_r variable


sum(is.na(large.df$____)) #counts number of NAs in column; use for other variable exploration

#lang.test.sub <- subset(large.df, (x1firkdg == "1: YES") &
           #             (c1spasmt %in% "1: YES" | c2spasmt %in% "1: YES" | c3spasmt %in% "1: YES" | c3spasmt %in% "1: YES"|
              #            c1sphome %in% " 1: YES" | c2sphome %in% "1: YES" | c3sphome %in% "1: YES" | c4sphome %in% "1: YES") &
            #              (x_hisp_r == "1: YES"))

race2.sub <- subset(large.df, (x1firkdg == "1: YES") & (x_raceth_r %in% "3: HISPANIC, RACE SPECIFIED" | 
                                                x_raceth_r %in% "4: HISPANIC, NO RACE SPECIFIED") &
                      (c1sphome %in% " 1: YES" | c2sphome %in% "1: YES" | c3sphome %in% "1: YES" | c4sphome %in% "1: YES"), 
                    addAttributes = FALSE, omittedLevels = FALSE)
               #       c1spasmt %in% "1: YES" | c2spasmt %in% "1: YES" | c3spasmt %in% "1: YES" | c4spasmt %in% "1: YES")) #adds tested in spanish at one point; how to ID when student achieved proficiency 
race2.sub <- subset(race2.sub, select= (-c(x4ktchapp,t4kennat,x4kclsnss,
                                         x4kcnflct,x4ktchext,x4kinbcnt,x4kattnfs,t4kkeeps,t4kshows,t4kworks,t4kadapts,t4kpersis,t4katten,t4kfollow)))

#race2.lang <- subset(large.df, (x1firkdg == "1: YES") &
                      #    (c1sphome %in% " 1: YES" | c2sphome %in% "1: YES" | c3sphome %in% "1: YES" | c4sphome %in% "1: YES" |
                    #         c1spasmt %in% "1: YES" | c2spasmt %in% "1: YES" | c3spasmt %in% "1: YES" | c4spasmt %in% "1: YES") &
                     #  (x_raceth_r %in% "3: HISPANIC, RACE SPECIFIED" | 
                                                        #  x_raceth_r %in% "4: HISPANIC, NO RACE SPECIFIED"))
#order of lang assessment and spanish in home doest not matter --> race indicator causing different # of observations 
#diff.order.race2 <- subset(large.df, (x1firkdg == "1: YES") &
                         #    (c1spasmt %in% "1: YES" | c2spasmt %in% "1: YES" | c3spasmt %in% "1: YES" | c3spasmt %in% "1: YES"|
                           #     c1sphome %in% " 1: YES" | c2sphome %in% "1: YES" | c3sphome %in% "1: YES" | c4sphome %in% "1: YES") &
                          #   (x_raceth_r %in% "3: HISPANIC, RACE SPECIFIED" | 
                                    #      x_raceth_r %in% "4: HISPANIC, NO RACE SPECIFIED"))

#diff.race <- subset(large.df, (x1firkdg == "1: YES") & (x_hisp_r == "1: YES") &
                         #  (c1sphome %in% "1: YES" | c2sphome %in% "1: YES" | c3sphome %in% "1: YES" | c4sphome %in% "1: YES" |
                        #   c1spasmt %in% "1: YES" | c2spasmt %in% "1: YES" | c3spasmt %in% "1: YES" | c4spasmt %in% "1: YES"))
#correct syntax / use of operators

#data.diff <- setdiff(race2.sub, race2.lang) #difference arises in hispanic no race specified; value is not ascertained in x_ hisp and no race specified in x_race
# go with race2.sub

#data.diff <- setdiff(lang.test.sub, race2.sub) --> to see differences between data sets
#table(large.df$x_hisp_r[large.df$x_hisp_r %in% c("1: YES","2: NO")])  --> checks multiple conditions

#OR statement inside one (), separate from &; don't use distributive operators

kinder.atl <- subset(race2.sub, select= c(childid, s1_id, s2_id, x1kage_r, x2kage_r, x_chsex_r, x_hisp_r, x_raceth_r, x1locale, x2locale, x2inccat_i, x2povty, p1hig_1,
                                          p2hig_1, x1pubpri, x2pubpri, x1firkdg, c1spasmt, c2spasmt, c1enghm, c2enghm, c1sphome, c2sphome, p1prmlng, p1prmln1, p1prmln2,
                                          t2ennat, x1tchapp, x2tchapp, x1prnapp, x2prnapp, x2clsnss, x2cnflct, x1tchext, x2tchext, x1inbcnt, x2inbcnt, x1attnfs, x2attnfs,
                                          x1plss, x2plss, x1plart, x2plart, x1pltot, x2pltot, t1keeps, t2keeps, t1shows, t2shows, t1works, t2works, t1adapts, t2adapts, 
                                          t1persis, t2persis, t1atten, t2atten, t1follow, t2follow, t1class, t2nstnl, a2smlgrp, a2anonen, a2pnonen, a2dnonen, a1timdis, a1yrstch, 
                                          a1yrsch, a1hghstd, a1early, a1esl, a1devlp, a1yrborn, a1highql, a1aspnin, a1pspnin, a1dspnin, a1atnoot, a1ptnoot, a1dtnoot, a1atspnh, a1ptspnh, a1dtspnh,
                                          a1phisp, a1dhisp, a1acspnh, a1pcspnh, a1dcspnh, a1aell, a1pell, a1dell, a2aell, a2pell, a2dell, a1anmell, a1pnmell, a1dnmell, 
                                          a1ahisp, a1phisp, a1dhisp), addAttributes = TRUE, omittedLevels = FALSE)

frst.atl <- subset(race2.sub, select= c(childid, s3_id, s4_id, x3age, x4age, x_chsex_r, x_hisp_r, x_raceth_r, x3locale, x4locale, x4inccat_i, x4povty_i, p4hig_1_i, x3pubpri, x4pubpri, c3spasmt, c4spasmt,
                                        c3nenghm, c4nenghm, c3sphome, c4sphome, t4ennat, x3tchapp, x4tchapp, x4prnapp, x4clsnss, x4cnflct, x3tchext, x4tchext, x4inbcnt, x4attnfs, x3plss, x4plss, 
                                        x3plart, x4plart, x3pltot, x4pltot, t3keeps, t4keeps, t3shows, t4shows, t3works, t4works, t3adapts, t4adapts, t3persis, t4persis, t3atten, t4atten, t3follow, t4follow,
                                        a4wksgrp, a4noneng, a4nonin, a4dscptim, a4yrstch, a4yrsch, a4hghstd, a4early, a4esl, a4devlp, a4yrborn, a4highql, a4spnin, a4tnoot, a4tspnh, a4cspnh, a4ell, a4nmell, a4shisp))

dll.atl.kndr <- subset(kinder.atl, select= c(childid, x2kage_r, x2povty, kndr_par_ed, x2tchapp, x1tchapp, x2pltot, x2clsnss, x2cnflct, x2prnapp, x2inbcnt, x2attnfs, x2tchext, a1yrstch, a1yrborn, a1hghstd))
#final kinder dataset with significant predictors (w least NAs)

dll.atl.frst <- subset(frst.atl, select= c(childid, x4age, x4povty_i, fst_par_ed, x4tchapp, x3tchapp, x4clsnss, x4cnflct, x4prnapp, x4attnfs, x4inbcnt, x4tchext, a4yrstch, a4yrborn,a4hghstd))
#final first grade dataset with significant predictors (one's with least NAs)  
  
#power analysis
##### Missing Values Investigation #####
miss.var <- miss_var_summary(kinder.atl) #na summary as df 
frst.miss.var <- miss_var_summary(frst.atl)

dll.atl.kndr %>% complete.cases() %>% sum() %>% print() # n = 679
dll.atl.frst %>% complete.cases() %>% sum() %>% print() #n = 439

# this syntax pairs with final fit functions below
fall.explanatory <- c("x2pltot", "x2clsnss")
dependent <- c("x2tchapp")
gen.explanatory <- c("x2kage_r", "x2pltot", "x2clsnss", "x2cnflct", "x2prnapp") # had to remove categorical variables, only numeric variables work for MCAR test

child.covars <- c("x2kage_r", "x1kage_r","x2kage_r", "x1inbcnt", "x2inbcnt", "x1attnfs", "x2attnfs", 
                  "x1tchext", "x2tchext", "x2pltot", "x1pltot", "x1tchapp") #missingness ranges from 1-11%
ch.dem.covars <- c("x2tchapp","x2povty", "p1hig_1", "p1prmlng", "p1prmln1")
class.covar <- c("t1class", "t2nstnl", "a2smlgrp", )


#final fit functions
kinder.atl %>% ff_glimpse(dependent, gen.explanatory) #na summary by vairable types/lvls

kinder.atl %>% missing_pattern(dependent, gen.explanatory) #patterns in NAs --> 18 patterns in ATL missingness by prelas total & 146-7 by closeness/conflict = not completely at random

kinder.atl %>% missing_pairs(dependent, gen.explanatory) #visual distributions for patterns between observed and missing data
#876 missing cells among these variables

kinder.atl %>% missing_compare(dependent, fall.explanatory)
kinder.atl %>% missing_compare(dependent, gen.explanatory)

kinder.atl %>% select(all_of(gen.explanatory)) %>% mcar_test() #data for explanatory variables not missing completely at random

kinder.atl %>% select(all_of(gen.explanatory)) %>% TestMCARNormality()
#kinder.atl %>% select(all_of(gen.explanatory)) %>% MissMech::TestMCARNormality(., del.lesscases=1) #Hawkins Test = reject hyp that data is MCAR; parametric insufficient evidence to reject MCAR

TestMCARNormality(data=kinder.atl , del.lesscases=1)

