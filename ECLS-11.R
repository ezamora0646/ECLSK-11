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

main_df <- getData(data = eclsk11, varnames = colnames(eclsk11), addAttributes = TRUE, omittedLevels = FALSE)


########                  Variables of Interest       ######### 
###         ATL     ####
# x1= Fall kinder, x2= spring kinder, x3=f fall 1st, x4= spring 1st... IMPORTANT -> X4K SPRING 1ST FOR KIDS WHO WERE RETAINED IN KINDER 
("x1tchapp" , "x2tchapp", "x3tchapp", "x4tchapp","x4ktchapp") 


###        S-T Relationships     ####
# x2 = spring Kinder, x4 spring 1st 
("x2clsnss" , "x2cnflct" ,"x4clsnss", "x4cnflct")





#inclusion criteria for DLL sample 
# X12HISP
# X12LANGST