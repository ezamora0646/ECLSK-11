
###### Imputation Predictive Mean Matching######
library(mice)
install.packages("VIM")
library(VIM)
library(naniar)
#recoding vars to be compatible w mice package
str(dll.atl.kndr)
#some variables are not coded correctly... character 
#ID VARS SHOULD BE FACTOR 

dll.atl.kndr$childid <- as.factor(dll.atl.kndr$childid)
dll.atl.kndr$s2_id <- as.factor(dll.atl.kndr$s2_id)
dll.atl.kndr$x_chsex_r <- as.factor(dll.atl.kndr$x_chsex_r)  
dll.atl.kndr$x2pubpri <- as.factor(dll.atl.kndr$x2pubpri)
dll.atl.kndr$a1yrborn <- as.factor(dll.atl.kndr$a1yrborn)
dll.atl.kndr$a1hghstd <- as.factor(dll.atl.kndr$a1hghstd)
dll.atl.kndr$x2povty <- as.factor(dll.atl.kndr$x2povty) 
dll.atl.kndr$x2locale <- as.factor(dll.atl.kndr$x2locale)

#Missing Data Investigation pt2 #

# Kinder
    #multivariate - more than 1 var w missing data
    #general: % missing not arranged across vars

num.var.kndr <- kinder.atl[, c("x2kage_r", "x1tchapp" , "x2tchapp", "x2prnapp", "x2pltot", "x2clsnss", "x2cnflct", "x2inbcnt","x2attnfs", 
                               "x2tchext", "a1yrstch")]
md.pattern(num.var.kndr) #CONNECTED, vars can be reached w horizontal and vertical moves... 
    #CONNECTED VARS = CORRELATION COEFFICIENT
md.pattern(dll.atl.kndr) #CONNECTED

p <- md.pairs(dll.atl.kndr)
print(p) #r= observed, m=missing

flux(dll.atl.kndr)
# influx = missing & obs variable pairs / total obs data cells
# outflux = vars' usefulness in imputing other values... completely obs -> 1, ; completely missing -> 0
# vars w high influx and outflux preferred

fluxplot(dll.atl.kndr) #ID vars that clutter imputation model
# remove uninteresting vars and those that are located in lower regions (closer to lower left corner)

marginplot(dll.atl.kndr[,c("x2clsnss", "x2tchapp")])

ggplot(dll.atl.kndr,
       aes(x = x2clsnss,
           y = x2tchapp)) +
  geom_miss_point()
#values of ATL when closeness is NA... not uniform not MCAR

ggplot(dll.atl.kndr,
       aes(x = x2pltot,
           y = x2tchapp)) +
  geom_miss_point()
#values of ATL when prelass is missing... not uniform not MCAR

vis_miss(dll.atl.kndr, cluster = TRUE)

dll.atl.kndr %>% arrange(x2pltot) %>% vis_miss() #missing values cluster by this var... MAR
dll.atl.kndr %>% arrange(x2inbcnt) %>% vis_miss() #missing values cluster by this var...MAR
dll.atl.kndr %>% arrange(x2clsnss) %>% vis_miss() #missing values cluster by this var... MAR

#####imputation kinder ######
#removing vars with no missing data
kndr.miss.df <- subset(dll.atl.kndr, select = -c(childid, s2_id))


dll.atl.frst$x4locale <- frst.atl$x4locale
dll.atl.frst$x4pubpri <- frst.atl$x4pubpri

dll.atl.frst$x4locale <- as.factor(dll.atl.frst$x4locale)
dll.atl.frst$x4pubpri <- as.factor(dll.atl.frst$x4pubpri)
# 1st grade 
str(dll.atl.frst)
dll.atl.frst <- dll.atl.frst %>% subset(select = -s4_id)
#recoding variable types to be compatible with imputation
dll.atl.frst$s4_id <- frst.atl$s4_id
str(dll.atl.frst$s4_id)
dll.atl.frst$s4_id <- as.factor(dll.atl.frst$s4_id)
dll.atl.frst$childid <- as.factor(dll.atl.frst$childid) 
dll.atl.frst$x_chsex_r <- as.factor(dll.atl.frst$x_chsex_r)
dll.atl.frst$x4povty_i <- as.factor(dll.atl.frst$x4povty_i)
dll.atl.frst$a4yrborn <- as.factor(dll.atl.frst$a4yrborn)
dll.atl.frst$a4hghstd <- as.factor(dll.atl.frst$a4hghstd)

md.pattern(dll.atl.frst) #connected & non-monotone
md.pairs(dll.atl.frst) #r= observed, m=missing

flux(dll.atl.frst) # influx = missing & obs variable pairs / total obs data cells
# outflux = vars' usefulness in imputing other values... completely obs -> 1, ; completely missing -> 0
# vars w high influx and outflux preferred

fluxplot(dll.atl.frst)




#logistic regression powewr analysis - dist normal or poisson = well over power with comp and imputed data
#pwrss.z.logreg(p0 = 0.15, p1 = 0.10, r2.other.x = 0.20,
 #              power = 0.80, alpha = 0.05, 
  #             dist = "poisson")


#decisions made: 
# multilevel imputation 
  # schoolid = cluster
  # 
#number of imputed datasets or m start w 5, increase to avg of pct missing 9.33 so try with 5 and 10
#Rationale: see van buren 2013

mean(miss.var$pct_miss) # try m=5 & m=10
mean(frst.miss.var$pct_miss) # try m=5 & m=20

skew(dll.atl.kndr$x2tchapp) #slight skewness
skew(dll.atl.frst$x4tchapp) #slight skew

imp <- mice(dll.atl.kndr, maxit=0)
  # multicollinearity presents issue when imputing whole data set 
    # will subset into numeric and categorical variables, then merge imputed data sets


imp$pred
#shows you what variable acts as predictor in imputation
#missing data patterns, what variable used for imputation

imp$method


#tried deleting variables 1 by 1 and still get an error message
kndr.num.imp <- mice(num.var.kndr, m=1,maxit = 1)
kndr.num.imp

#bind id set with imputed
#look into multilevel imputation 
#no classroom ID... assume no random effect of class... no variation between baseline of classrooms

num.var.kndr <- kinder.atl[, c("childid", "x2tchapp")]

missing_pattern()
mice::md.pattern(dll.atl.kndr, plot = TRUE)

#impute longitudinal by converting to wide to use previous measures
