library(mice)
library(miceadds)
library(mitml)
####### Imputation by Joint Modeling ###### 
#data 
d <- dll.atl.kndr[ , c("s2_id", "cat.atl", "par.ed.k", "x1pltot", "x2kage_r", 
                     "x_chsex_r", "x2povty", "x2tchapp", "x1tchapp", 
                     "x2clsnss", "x2cnflct", "x1prnapp", "x1inbcnt", 
                     "x1attnfs", "a1yrstch")]


# EXAMPLE FROM VAN BUUREN 7.0
#fml <- list(lpo + iqv ~ 1 + (1 | sch), den ~ 1)
#mit <- mitml::jomoImpute(data = d, formula = fml, m = 10,
                        # silent = TRUE)
##


fm1 <- list(cat.atl + x2kage_r + x_chsex_r + x2povty + kndr_par_ed + x2tchapp + x1tchapp + x2pltot + 
            x2clsnss + x2cnflct + x2prnapp + x2inbcnt + x2attnfs + x2tchext + a1yrstch + 
              a1yrborn + a1hghstd ~ 1 + (1 | s2_id), 
            x2pubpri + x2locale ~ 1)

#mit <- jomoImpute(data=d, formula = fm1, m = 2, n.burn= 100, n.iter=1, 
 #                silent = TRUE, seed =805)
#mit$data$x2povty 
#cluster 1111 did not have any value imputed for cat.atl... other single clusters similar behavior 
# maybe more iterations 

#mit2 <- jomoImpute(data=d, formula = fm1, m = 10, n.burn= 100, n.iter= 10, 
 #                         silent = TRUE, seed =805)
#mit2$data

#is.na(mit$data)
#is.na(mit2$data)

#mit3 <- jomoImpute(data=d, formula = fm1, m = 10, n.burn= 100, n.iter= 15, 
                  # silent = TRUE, seed =805)
#is.na(mit3$data)
#mit3$data


blk <- make.blocks(d, "collect")
fm2 <- list(collect = fm1)

imp <- mice(d, meth = "jomoImpute", blocks = blk, form = fm2,
             print = FALSE, seed = 805, maxit = 5,
             m = 10, n.burn = 100)
#can add more imputations by calling on "imp" object that is already created

#running single level pmm... relationship of missingness and obs is same across schools


###### DIAGNOSTICS ######

imp$method
imp$predictorMatrix
imp$visitSequence
imp$loggedEvents

imp$chainMean
imp$chainVar

rest.k.imp$imp$a1yrstch

conv <- convergence(imp, diagnostic = "all")

plot(conv$ac, conv$.it) #GOAL AC MUST BECOME SMALL BUT STABLE
#CONCLUSION: ITERATIONS NO LONGER HIGHLY CORRELATED 

plot(conv$psrf, conv$.it) #PSRF CLOSE TO 1 AS ITERATIONS INCREASE... SPREAD IS AROUND 1 COULD BE TIGHTER 

#TRACE PLOTS - VALUES SHOULD NOT VARY SIGNIFICANTLY ACROSS ITERATIONS 

plot(imp)
#clear trends = more variation between chains than within
#closeness, conflict, inhibitory control, years teaching... correlation problems 

stripplot(imp) #only shows continuous variables 
#one imputation resulted in NEGATIVE value 

bwplot(imp, main= "Joint Modeling")
densityplot(imp, main ="Joint modeling") #obs data = blue ... imputed = red
#closess and conflict imputation has more variation... fixed effect imputaiton??
#fixed effect imputation --> school id as factor... each school is imputed one at a time
#for school

propplot(imp)
#1 bar = proportion of data in a given level of var 
#black = dist of og incomplete data
#blue = distribution of levels in each imputed data set (m= 5, 10, etc)
#under MAR blue and black should have same distributions 

#RESULT: SIMILAR DISTRIBUTIONS; HIGHER PEAK, ESPECIALLY FOR A1YRBRN

#check distributions of locale, sex, yr born
#check distributions of prelas, conflict, closeness, externalization 

#under MAR: distributions should be similar when imputation model (fm1) is a good fit

densityplot(imp, ~x2pltot | x2clsnss + x2kage_r)
#imputation method is not restricting to plausible values==> explains shape 


#long.comp.k <- complete(imp, "long") # will need this format for other part 
childid <- dll.atl.kndr$childid

#comp.k <- complete(imp, "all", include=TRUE)
#comp.k <- comp.k_with_id <- lapply(comp.k, function(df) {
#  df$childid <- childid  # Add the childid column back to the dataset
#  return(df)
#})
#checking for mismatches in childid
#for (i in 1:length(comp.k_with_id)) {
#  if (!all(comp.k_with_id[[i]]$childid == dll.atl.kndr$childid)) {
#    stop(paste("Row order mismatch detected in imputed dataset", i))
#  }
#}
#extracting all closeness / conflict columns
#str.list <- lapply(comp.k[2:11], function(df) {
#  df[, c(12, 13), drop = FALSE]  # drop=FALSE ensures the result is a data frame
#})
#running PCA on all subsette columns
#k.pca.rslt <- lapply(str.list, function(df) {
#  pca <- prcomp(df, scale. = TRUE)
#  return(pca)
#})
#Viewing results across data sets 
# Access and view PCA results for each dataset
#for (i in 1:length(k.pca.rslt)) {
#  cat("PCA Results for Data Frame", i, ":\n")
  # Extract PCA object
#  pca <- k.pca.rslt[[i]]
  # Print the standard deviations (eigenvalues)
#  cat("Standard Deviations (Eigenvalues):\n")
#  print(pca$sdev)
  # Print the proportion of variance explained by each component
#  variance_explained <- pca$sdev^2 / sum(pca$sdev^2)
#  cat("Proportion of Variance Explained:\n")
#  print(variance_explained)
  # Print cumulative variance
#  cumulative_variance <- cumsum(variance_explained)
#  cat("Cumulative Variance Explained:\n")
#  print(cumulative_variance)
#  cat("\n")
#}
#pc_scores_list <- lapply(k.pca.rslt, function(pca) {
#  pca$x[, 1]  # Extract the first principal component scores
#})
# Add the first principal component scores to each imputed data frame (1 to 10) in comp.k
#comp.k_updated <- lapply(1:10, function(i) {
  # Extract the i-th imputed dataset (1 to 10)
#  df <- comp.k[[i + 1]]  # Adjust index to match comp.k list (1 corresponds to comp.k[[2]])
    # Add the first principal component scores
#  df$relation.pca <- pc_scores_list[[i]]
#  return(df)
#})
#making new mids object that has similar structure to imp but has column for PCA score in m= 1:10
#comp.k.final <- vector("list", length(comp.k))
#comp.k.final[[1]] <- comp.k[[1]]
#for (i in 1:10) {  # Loop from 1 to 10
#  comp.k.final[[i + 1]] <- comp.k_updated[[i]]  # Updated imputed datasets with PCA variable
#}
#long.comp.k <- complete(imp, "long", include=TRUE)
#pca_scores_list <- vector("list", length = imp$m)
#for (i in 1:imp$m) {
  # Extract the current imputation dataset
#  current_data <- long.comp.k[long.comp.k$.imp == i, ]
  
  # Ensure the selected columns are in the correct format (data frame or matrix)
#  selected_data <- current_data[, c("x2clsnss", "x2cnflct"), drop = FALSE]
  # Convert selected_data to a matrix if it is a data frame
  #if (is.data.frame(selected_data)) {
#    selected_data <- as.matrix(selected_data)
#  }
  
  # Check if selected_data is a matrix
#  if (!is.matrix(selected_data)) {
#    stop("Selected data is not in the correct format.")
#  }
  
  # Check for missing or infinite values in the variables used for PCA
#  if (any(is.na(selected_data)) || any(is.infinite(selected_data))) {
#    stop("Data contains missing or infinite values in the variables used for PCA.")
#  }
  # Perform PCA on the x2clsnss and x2cnflct variables
#  pca_result <- prcomp(selected_data, center = TRUE, scale. = TRUE)
    # Extract the first principal component scores
#  pc1_scores <- pca_result$x[, 1]
  # Store PCA scores in the list
#  pca_scores_list[[i]] <- pc1_scores
#}
# Create a new column for PCA scores in the long format data
#long.comp.k$relation.pca <- NA  # Initialize the new column
# Loop through each imputation dataset
#for (i in 1:imp$m) {
  # Extract rows corresponding to the current imputation
#  current_imp_rows <- long.comp.k$.imp == i
  # Assign the PCA scores to the correct rows
  #long.comp.k$relation.pca[current_imp_rows] <- pca_scores_list[[i]]
#}
#long.comp.k$cat.atl <- as.numeric(as.character(long.comp.k$cat.atl))
#mids.pca <- as.mids(long.comp.k)
## POOLING RESULTS FROM IMPUTATION
#reg.model <- with(imp, glm(I(cat.atl=="High") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
    #                         x1tchapp+x2prnapp+x2inbcnt+x2attnfs+
   #                          a1yrstch+a1hghstd+
  #                           x2pltot+x2clsnss+x2pltot:x2clsnss, 
 #                          family=binomial))
#round.summary(reg.model, digits=3,  exponentiate = T)[-1, c("estimate", "2.5 %", "97.5 %", "p.value")]
#summary(pool(reg.model))
#cat.mod <- lapply(imp.list, FUN=function(data){
  #glm.cluster(data= d, 
      #        formula=I(cat.atl=="High") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
     #         x1tchapp+x2prnapp+x2inbcnt+x2attnfs+
    #          a1yrstch+a1hghstd+
   #           x2pltot+x2clsnss+x2pltot:x2clsnss, 
  #          cluster=d$s2_id,
 #           family="binomial") 
#})
#with(comp.k.final, glm.cluster(data = comp.k, formula = I(cat.atl=="High") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
 #                                x1tchapp+x2prnapp+x2inbcnt+x2attnfs+
  #                               a1yrstch+a1hghstd+
   #                              x2pltot+x2clsnss+x2pltot:x2clsnss, 
    #                           cluster=s2_id,
     #                          family="binomial"))
#pca.model <- with(comp.k.final, expr = glm(I(cat.atl=="High") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
#                        x1tchapp+x2prnapp+x2inbcnt+x2attnfs+
#                        a1yrstch+a1hghstd+
#                        x2pltot+relation.pca+x2pltot:relation.pca, 
#                        family=binomial))

#summary(pool(pca.model), conf.int = T, exponentiate = F) #exponentiate = results in odds ratios
#round.summary(pca.model, conf.int= T, exponentiate = T)[-1, c("estimate", "2.5 %", "97.5 %", "p.value")]
#)

##### Conditional imputation  ######

#re run on 8/23 - edits made 8/21 5:30
#post <- make.post(d)
# post["x2clsnss"] <- 
  # "imp[[j]][, i][imp[[j]][, i] < 1] <- 1; imp[[j]][, i][imp[[j]][, i] > 5] <- 5"

# post["x2cnflct"] <- 
  # "imp[[j]][, i][imp[[j]][, i] < 1] <- 1; imp[[j]][, i][imp[[j]][, i] > 5] <- 5"

# post["x2tchext"] <- 
  # "imp[[j]][, i][imp[[j]][, i] < 1] <- 1; imp[[j]][, i][imp[[j]][, i] > 4] <- 4"

# post["a1yrstch"] <- 
  # "imp[[j]][, i][imp[[j]][, i] < 1] <- 1; imp[[j]][, i][imp[[j]][, i] > 37] <- 37"

#post["x2clsnss"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 5))"

# post["x2cnflct"] <- 
  # "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 5))"

# post["x2tchext"] <- 
  # "imp[[j]][, i] <- squeeze(imp[[j]][,i], c(1, 4))"

# post["a1yrstch"] <- 
  # "imp[[j]][, i] <- squeeze(imp[[j]][,i], c(1, 37))"

#rest.k.imp <- mice(d, method = "jomoImpute", blocks = blk, form = fm2,
              #     print = FALSE, m=10, maxit=5, n.burn = 100, seed=805, post = post)
# densityplot(rest.k.imp)
# bwplot(rest.k.imp, main = "restricted imputation")



##### First Grade Imputation #####

df <- dll.atl.frst[,c("s4_id","x4age", "x_chsex_r", "x4povty_i", "fst_par_ed", "x4tchapp", "x3tchapp",
                   "x4clsnss", "x4cnflct", "x4prnapp", "x4attnfs", "x4inbcnt", "x4tchext", "a4yrstch",
                   "a4yrborn", "a4hghstd", "fst.cat.atl", "frst.prof", "x4pubpri", "x4locale")]
df$x4locale <- as.factor(df$x4locale)
df$x4pubpri <- as.factor(df$x4pubpri)

fst.fm <- list(fst.cat.atl + x4age + x_chsex_r + x4povty_i + fst_par_ed + x4tchapp + x3tchapp + frst.prof +
                 x4clsnss + x4cnflct + x4prnapp + x4attnfs + x4inbcnt + x4tchext + a4yrstch + a4yrborn + a4hghstd 
              ~ 1 + (1 | s4_id),
               x4pubpri + x4locale ~1)

rm(meth)

blk2 <- make.blocks(df, "collect")
fst.fm2 <- list(collect=fst.fm)

fst.imp <- mice(df, meth = "jomoImpute", blocks = blk2, form = fst.fm2,
                print = FALSE, seed = 805, maxit = 5,
                m = 10, n.burn = 100)

#### First grade Diagnostics #####
fst.imp$method
fst.imp$predictorMatrix
fst.imp$loggedEvents

fst.imp$chainMean
fst.imp$chainVar

#fst.imp$imp$

fst.conv <- convergence(fst.imp, diagnostic = "all")

plot(fst.conv$ac, fst.conv$.it)
plot(fst.conv$psrf, fst.conv$.it)

plot(fst.imp)

stripplot(fst.imp)
bwplot(fst.imp)

densityplot(fst.imp)
propplot(fst.imp)

##if convergence has been reached, rerun imputations after getting rid of -9 Not Ascertained
## levels have been adjusted 8/21 5:00

###### function for propplot ######
propplot <- function(x, formula, facet = "wrap", ...) {
  library(ggplot2)
  
  cd <- data.frame(mice::complete(x, "long", include = TRUE))
  cd$.imp <- factor(cd$.imp)
  
  r <- as.data.frame(is.na(x$data))
  
  impcat <- x$meth != "" & sapply(x$data, is.factor)
  vnames <- names(impcat)[impcat]
  
  if (missing(formula)) {
    formula <- as.formula(paste(paste(vnames, collapse = "+",
                                      sep = ""), "~1", sep = ""))
  }
  
  tmsx <- terms(formula[-3], data = x$data)
  xnames <- attr(tmsx, "term.labels")
  xnames <- xnames[xnames %in% vnames]
  
  if (paste(formula[3]) != "1") {
    wvars <- gsub("[[:space:]]*\\|[[:print:]]*", "", paste(formula)[3])
    # wvars <- all.vars(as.formula(paste("~", wvars)))
    wvars <- attr(terms(as.formula(paste("~", wvars))), "term.labels")
    if (grepl("\\|", formula[3])) {
      svars <- gsub("[[:print:]]*\\|[[:space:]]*", "", paste(formula)[3])
      svars <- all.vars(as.formula(paste("~", svars)))
    } else {
      svars <- ".imp"
    }
  } else {
    wvars <- NULL
    svars <- ".imp"
  }
  
  for (i in seq_along(xnames)) {
    xvar <- xnames[i]
    select <- cd$.imp != 0 & !r[, xvar]
    cd[select, xvar] <- NA
  }
  
  
  for (i in which(!wvars %in% names(cd))) {
    cd[, wvars[i]] <- with(cd, eval(parse(text = wvars[i])))
  }
  
  meltDF <- reshape2::melt(cd[, c(wvars, svars, xnames)], id.vars = c(wvars, svars))
  meltDF <- meltDF[!is.na(meltDF$value), ]
  
  
  wvars <- if (!is.null(wvars)) paste0("`", wvars, "`")
  
  a <- plyr::ddply(meltDF, c(wvars, svars, "variable", "value"), plyr::summarize,
                   count = length(value))
  b <- plyr::ddply(meltDF, c(wvars, svars, "variable"), plyr::summarize,
                   tot = length(value))
  mdf <- merge(a,b)
  mdf$prop <- mdf$count / mdf$tot
  
  plotDF <- merge(unique(meltDF), mdf)
  plotDF$value <- factor(plotDF$value,
                         levels = unique(unlist(lapply(x$data[, xnames], levels))),
                         ordered = T)
  
  p <- ggplot(plotDF, aes(x = value, fill = get(svars), y = prop)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme(legend.position = "bottom", ...) +
    ylab("proportion") +
    scale_fill_manual(name = "",
                      values = c("black",
                                 colorRampPalette(
                                   RColorBrewer::brewer.pal(9, "Blues"))(x$m + 3)[1:x$m + 3])) +
    guides(fill = guide_legend(nrow = 1))
  
  if (facet == "wrap")
    if (length(xnames) > 1) {
      print(p + facet_wrap(c("variable", wvars), scales = "free"))
    } else {
      if (is.null(wvars)) {
        print(p)
      } else {
        print(p + facet_wrap(wvars, scales = "free"))
      }
    }
  
  if (facet == "grid")
    if (!is.null(wvars)) {
      print(p + facet_grid(paste(paste(wvars, collapse = "+"), "~ variable"),
                           scales = "free"))
    }
}


####### function for round.summary

round.summary <- function(FIT.MI, conf.int=T, digits=4, DATA.FRAME=T, ...) {
  # The round.summary() function is provided with no express or implied warranty.
  
  # Function to allow rounding of summary(pool(FIT.MI))
  require(tidyverse)
  # Convert to a data.frame
  DF <- as.data.frame(summary(pool(FIT.MI), conf.int=conf.int, ...))
  # Move term from column to rownames
  rownames(DF) <- DF$term
  DF <- DF %>% 
    select(-term)
  # Convert to matrix, round
  DF <- round(as.matrix(DF), digits)
  # Convert back to a data.frame (if DATA.FRAME=T)
  if (DATA.FRAME) DF <- as.data.frame(DF)
  return(DF)
}


mi.n.p <- function(IMP, X) {
  # The mi.n.p() function is provided with no express or implied warranty.
  
  # Compute frequency and proportion using multiply imputed data
  # For ONE variable at a time
  
  # IMP is a mids object created by mice()
  # X is a character value (e.g., "varname")
  
  # So I can refer to X later
  # Must use include = TRUE here or as.mids will not work 
  IMPDAT    <- complete(IMP, "long", include = TRUE)
  IMPDAT$.x <- IMPDAT[[X]]
  IMP       <- as.mids(IMPDAT)
  
  # with() automatically excludes the original data
  # and just does this on the imputed data
  TAB <- matrix(unlist(with(IMP, table(.x))$analyses),
                nrow=IMP$m, byrow = T)
  N <- apply(TAB, 2, mean)
  P <- N/sum(N)
  OUT <- data.frame(n = N, p = P)
  rownames(OUT) <- paste(X, levels(IMPDAT$.x), sep = ": ")
  return(OUT)
}




mi.mean.se.sd <- function(IMP, X) {
  # The mi.mean.se.sd() function is provided with no express or implied warranty.
  
  # Compute mean and standard error of the mean using multiply imputed data
  # For ONE variable at a time
  
  # IMP is a mids object created by mice()
  # X is a character value (e.g., "varname")
  
  # So I can refer to X later
  # Do NOT use include = TRUE here as these computations
  # are just on the imputed data.
  IMPDAT    <- complete(IMP, "long")
  IMPDAT$.x <- IMPDAT[[X]]
  
  # Compute stats within each imputation
  MEAN     <- tapply(IMPDAT$.x, IMPDAT$.imp, mean)
  VAR.MEAN <- tapply(IMPDAT$.x, IMPDAT$.imp, var)
  SD       <- tapply(IMPDAT$.x, IMPDAT$.imp, sd)
  
  # Use Rubin's Rules to pool
  POOLED  <- pool.scalar(Q = MEAN,
                         U = VAR.MEAN,
                         n = nrow(IMP$data))
  
  OUT <- data.frame(mean = POOLED$qbar,
                    se   = sqrt(POOLED$t),
                    sd   = mean(SD))
  rownames(OUT) <- X
  return(OUT)
}

#counts of categorical variable by another var 
mi.n.p.by <- function(IMP, X, BY) {
  # The mi.n.p.by() function is provided with no express or implied warranty.
  
  # Compute frequency and proportion using multiply imputed data by another variable
  # For ONE variable at a time
  
  # IMP is a mids object created by mice()
  # X is a character value (e.g., "varname")
  # BY is a character value for a factor variable to stratify by
  
  # So I can refer to X and BY = Z later
  # Must use include = TRUE here or as.mids will not work 
  IMPDAT    <- complete(IMP, "long", include = TRUE)
  IMPDAT$.x <- IMPDAT[[X]]
  IMPDAT$.z <- IMPDAT[[BY]]
  IMP       <- as.mids(IMPDAT)
  
  LEVELS <- levels(IMPDAT$.z)
  
  for(i in 1:length(LEVELS)) {
    # with() automatically excludes the original data
    # and just does this on the imputed data
    TAB <- matrix(unlist(with(IMP, table(.x[.z == levels(.z)[i]]))$analyses),
                  nrow=IMP$m, byrow = T)
    N <- apply(TAB, 2, mean)
    P <- N/sum(N)
    if (i == 1) {
      N.P <- cbind(N,P)
    } else {
      N.P <- cbind(N.P, N, P) 
    }
  }
  
  OUT <- data.frame(N.P)
  names(OUT) <- c(rbind(paste("n", 1:length(LEVELS), sep="."),
                        paste("p", 1:length(LEVELS), sep=".")))
  rownames(OUT) <- paste(X, levels(IMPDAT$.x), sep = ": ")
  return(OUT)
}



