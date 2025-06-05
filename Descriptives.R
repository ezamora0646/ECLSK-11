#### Descriptives after RAC 10/25 ####

describe(comp.kndr %>% select(x2kage, x1pltot, x2pltot, x1tchapp, x2tchapp, 
                              x2clsnss, x2cnflct, x1prnapp, x2prnapp, 
                              x1inbcnt, x2inbcnt, x1attnfs, x2attnfs))

k.corr <- comp.kndr %>%
  select(x2kage, x1pltot, x2pltot, x1tchapp, x2tchapp, x2clsnss, x2cnflct, 
         x1prnapp, x2prnapp, x1inbcnt, x2inbcnt, x1attnfs, x2attnfs) %>%
  cor(use = "pairwise.complete.obs")

# Convert the matrix to a data frame
k.corr <- as.data.frame(k.corr)

# Keep only the lower triangle and set upper triangle to NA
k.corr[upper.tri(k.corr)] <- NA

# Add row names as a column
k.corr$Variable <- rownames(k.corr)

# Rearranging the data frame to have variable names as the first column
k.corr <- k.corr %>% 
  select(Variable, everything())

# View the cleaned correlation data frame
print(k.corr)

# Optionally, export to CSV
write.csv(k.corr, "correlation_matrix.csv", row.names = FALSE)


k.atl.freq <- table(comp.kndr$x_chsex_r, comp.kndr$cat.atl)
par_ed_k.freq <- table(comp.kndr$x_chsex_r, comp.kndr$par.ed.k)
x2povty.freq <- table(comp.kndr$x_chsex_r, comp.kndr$x2povty)

# Combine results into a single list for better viewing
k.counts <- list(
  cat_atl = k.atl.freq,
  par_ed_k = par_ed_k.freq,
  x2povty = x2povty.freq
)

# View the counts
k.counts

comp.kndr <- comp.kndr %>%
  rename(x2kage = x2kage_r)

##New approachs for K Corr matrix
# Select the variables for analysis
vars_kndr <- c("x2kage", "x1pltot", "x2pltot", "x1tchapp", "x2tchapp", 
               "x2clsnss", "x2cnflct", "x1prnapp", "x2prnapp", 
               "x1inbcnt", "x2inbcnt", "x1attnfs", "x2attnfs")

# Compute descriptive statistics (Mean and SD)
descriptives_kndr <- comp.kndr %>%
  select(all_of(vars_kndr)) %>%
  summarise(across(everything(), list(mean = ~ mean(.x, na.rm = TRUE),
                                      sd = ~ sd(.x, na.rm = TRUE),
                                      n = ~ sum(!is.na(.x)))))

# Rename columns to match required structure
colnames(descriptives_kndr) <- gsub("_", "-", colnames(descriptives_kndr))

# Pivot the data longer
descriptives_kndr_long <- descriptives_kndr %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_sep = "-", 
               values_to = "Value") %>%
  arrange(match(Variable, vars_kndr))

# Convert correlation matrix to a long format
lower_tri <- cor_matrix
lower_tri[upper.tri(lower_tri)] <- NA  # Keep only the lower triangle of the correlation matrix

# Convert to long format
lower_tri_long <- as.data.frame(as.table(lower_tri)) %>%
  filter(!is.na(Freq)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>%
  filter(match(Variable1, vars_kndr) > match(Variable2, vars_kndr)) %>%
  mutate(Correlation_with_Stars = case_when(
    p_matrix[cbind(match(Variable1, rownames(p_matrix)), match(Variable2, colnames(p_matrix)))] < 0.01 ~ 
      paste0(Correlation, "**"),
    p_matrix[cbind(match(Variable1, rownames(p_matrix)), match(Variable2, colnames(p_matrix)))] < 0.05 ~ 
      paste0(Correlation, "*"),
    TRUE ~ as.character(Correlation)
  ))

# Combine correlation matrix with descriptives
final_table <- descriptives_kndr_long %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%  # Separate mean, sd, n into columns
  rowwise() %>%
  mutate(Correlations = list(lower_tri_long %>%
                               filter(Variable1 == Variable) %>%
                               select(Variable2, Correlation_with_Stars))) %>%
  unnest(Correlations) %>%
  pivot_wider(names_from = Variable2, values_from = Correlation_with_Stars, names_sep = "_") %>%
  arrange(match(Variable, vars_kndr))

# View the final table
print(final_table)

# Export to CSV
write.csv(final_table, "lower_diagonal_corr_matrix_with_stars.csv", row.names = FALSE)





##### First grade Descriptives ####
#changed to longi controls df on 12/4

#filtering and then adding x4tchapp to longi.comp df
x4tchapp_filtered <- race2.sub %>%
  select(childid, x4tchapp) %>%  # Select childid and x4tchapp
  filter(childid %in% longi.comp.fst$childid, !is.na(x4tchapp))

longi.comp.fst <- longi.comp.fst %>%
  left_join(x4tchapp_filtered, by = "childid")
if (!"x4tchapp" %in% colnames(longi.comp.fst)) {
  stop("x4tchapp was not successfully added to longi.comp.fst.")
}


longi.comp.fst %>% select(x4age, x2tchapp, x4tchapp, x4prnapp, 
                    x4clsnss, x4cnflct, x2attnfs, x2inbcnt) %>% cor()


# Define variables of interest
vars <- c("x4age", "x2tchapp", "x4tchapp", "x4prnapp", 
          "x4clsnss", "x4cnflct", "x2attnfs", "x2inbcnt")

# Compute correlation matrix with significance
cor_results <- Hmisc::rcorr(as.matrix(longi.comp.fst[vars]), type = "pearson")

# Extract components
cor_matrix <- cor_results$r
p_matrix <- cor_results$P
n_matrix <- cor_results$n

# Compute descriptive statistics (Mean and SD)
descriptives <- longi.comp.fst %>%
  select(all_of(vars)) %>%
  summarise(across(everything(), list(mean = ~ mean(.x, na.rm = TRUE),
                                      sd = ~ sd(.x, na.rm = TRUE),
                                      n = ~ sum(!is.na(.x)))))

# Format descriptive statistics
descriptives <- descriptives %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", ".value"), 
               names_sep = "_") %>%
  arrange(match(Variable, vars))

# Create lower-diagonal correlation matrix
lower_tri <- cor_matrix
lower_tri[upper.tri(lower_tri)] <- NA

# Convert to long format
lower_tri_long <- as.data.frame(as.table(lower_tri)) %>%
  filter(!is.na(Freq)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>%
  filter(match(Variable1, vars_kndr) > match(Variable2, vars_kndr)) %>%
  mutate(Correlation_with_Stars = case_when(
    p_matrix[cbind(match(Variable1, rownames(p_matrix)), match(Variable2, colnames(p_matrix)))] < 0.01 ~ 
      paste0(Correlation, "**"),
    p_matrix[cbind(match(Variable1, rownames(p_matrix)), match(Variable2, colnames(p_matrix)))] < 0.05 ~ 
      paste0(Correlation, "*"),
    TRUE ~ as.character(Correlation)
  ))

# Combine correlation matrix with descriptives
final_table <- descriptives_kndr_long %>%
  rename(n = n, M = mean, SD = sd) %>%
  rowwise() %>%
  mutate(Correlations = list(lower_tri_long %>%
                               filter(Variable1 == Variable) %>%
                               select(Variable2, Correlation_with_Stars))) %>%
  unnest(Correlations) %>%
  pivot_wider(names_from = Variable2, values_from = Correlation_with_Stars, names_sep = "_") %>%
  arrange(match(Variable, vars_kndr))

# View the final table
print(final_table)

# Export to CSV
write.csv(final_table, "first_corr_matrix_with_stars.csv", row.names = FALSE)





#OLD APPROACH FOR DESCRIPTIVES + CORR MATRIX (SEPARATE DFS)
# Create the correlation matrix
longi.corr <- longi.comp.fst %>%
  select(x4age, x2tchapp, x4tchapp, x4prnapp, 
         x4clsnss, x4cnflct, x2attnfs, x2inbcnt) %>%
  cor(use = "pairwise.complete.obs")

# Convert to data frame and set upper triangle to NA to keep only lower triangle
cor_df <- as.data.frame(cor_matrix)
cor_df[upper.tri(cor_df)] <- NA

# Add row names as a column to retain variable names in the data frame
cor_df$Variable <- rownames(cor_df)

# Reorder so that 'Variable' column comes first
cor_df <- cor_df %>% select(Variable, everything())

# View the resulting lower diagonal correlation data frame
print(cor_df)

# Export to CSV if needed
write.csv(cor_df, "first correlation matrix.csv", row.names = FALSE)


## first categorical counts
fst.atl.feq <- table(longi.comp.fst$x_chsex_r, longi.comp.fst$fst.cat.atl)
fst.pvty.feq <- table(longi.comp.fst$x_chsex_r, longi.comp.fst$x4povty_i)
fst.prof.feq <- table(longi.comp.fst$x_chsex_r, longi.comp.fst$frst.prof)
fst.ed.feq <- table(longi.comp.fst$x_chsex_r, longi.comp.fst$fst.par.ed)

fst.counts <- list(
  atl=fst.atl.feq, 
  poverty=fst.pvty.feq, 
  prof=fst.prof.feq, 
  ed=fst.ed.feq
)

fst.counts_with_pct <- lapply(fst.counts, function(count_table) {
  # Calculate percentages by row and round
  percentages <- round(100 * prop.table(count_table, margin = 1), 1)
  # Bind counts and percentages side-by-side
  cbind(Counts = as.data.frame.matrix(count_table), Percentages = as.data.frame.matrix(percentages))
})


##### New 1st Grade df - Longitudinal controls #####

longi.fst <- race2.sub %>% select(c(childid, x_chsex_r, x4age, 
                                    x2tchapp, x2inbcnt, x2attnfs, x2clsnss, x2cnflct,
                                    x4clsnss, x4cnflct,
                                    x4prnapp,
                                    a4yrstch,
                                    w4cf4p21, w4cf4p22, w4cf4p23, w4cf4p24, 
                                    w4cf4p25, w4cf4p26, w4cf4p27, w4cf4p28, 
                                    w4cf4p29, w4cf4p210, w4cf4p211, w4cf4p212, 
                                    w4cf4p213, w4cf4p214, w4cf4p215, w4cf4p216, 
                                    w4cf4p217, w4cf4p218, w4cf4p219, w4cf4p220, 
                                    w4cf4p221, w4cf4p222, w4cf4p223, w4cf4p224, 
                                    w4cf4p225, w4cf4p226, w4cf4p227, w4cf4p228, 
                                    w4cf4p229, w4cf4p230, w4cf4p231, w4cf4p232, 
                                    w4cf4p233, w4cf4p234, w4cf4p235, w4cf4p236, 
                                    w4cf4p237, w4cf4p238, w4cf4p239, w4cf4p240, 
                                    w4cf4p20))
longi.fst$x4povty_i <- dll.atl.frst$x4povty_i
longi.fst$fst.par.ed <- dll.atl.frst$fst.par.ed
longi.fst$frst.prof <- dll.atl.frst$frst.prof
longi.fst$fst.cat.atl <- dll.atl.frst$fst.cat.atl
longi.fst$s4_id <- dll.atl.frst$s4_id
# not included bc using kinder controls: x3tchapp, x4tchapp, x4attnfs, x4inbcnt
#longi.fst <- as.data.frame(longi.fst)
longi.comp.fst <- na.omit(longi.fst)


