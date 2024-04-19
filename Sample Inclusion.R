#conditional statements for sample inclusion criteria 
library(haven) # get .dta and then use read_dta 
library(EdSurvey) 
library(tidyverse)
library(labelled)

###### INVESTIGATING VARIBALES #####
#typeof() to investigate var type and determine whether it needs recoding before conditional statements
#if need recoding then make new df without var attr

typeof(large.df$c1spasmt) #interger 
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

lang.test.sub <- subset(large.df, c1spasmt %in% "1: YES" | c2spasmt %in% "1: YES" | c3spasmt %in% "1: YES" | c3spasmt %in% "1: YES" &
                          c1sphome %in% " 1: YES" | c2sphome %in% "1: YES" | c3sphome %in% "1: YES" | c4sphome %in% "1: YES" &
                          x_hisp_r %in% "1: YES" &
                          x1firkdg %in% "1:YES"
                        )
#399 tested in spanish k-1st (1st filter line); 1768 spanish at home; 1758 hispanic + first time kinder

race2.sub <- subset(large.df, x1firkdg ==1 & (x_raceth_r %in% "3: HISPANIC, RACE SPECIFIED" | 
                                                x_raceth_r %in% "4: HISPANIC, NO RACE SPECIFIED") &
                      c1sphome %in% " 1: YES" | c2sphome %in% "1: YES" | c3sphome %in% "1: YES" | c4sphome %in% "1: YES" &
                      c1spasmt %in% "1: YES" | c2spasmt %in% "1: YES" | c3spasmt %in% "1: YES" | c4spasmt %in% "1: YES")
#15042 first time kinder; 3673 school reported hispanic; 1760 speak spanish at home; 1750 assessed in spanish at one point k-1st

diff.order.sub <- subset(large.df, x1firkdg ==1 & x_hisp_r %in% "1: YES" &
                           c1sphome %in% " 1: YES" | c2sphome %in% "1: YES" | c3sphome %in% "1: YES" | c4sphome %in% "1: YES" &
                           c1spasmt %in% "1: YES" | c2spasmt %in% "1: YES" | c3spasmt %in% "1: YES" | c4spasmt %in% "1: YES")
#3478 first time kinder hispanics; 1760 speak spanish at home; 1750 assessed in spanish at one point k-1st
