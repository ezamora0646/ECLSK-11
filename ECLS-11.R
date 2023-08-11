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
