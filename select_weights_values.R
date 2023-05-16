source('compute_new_columns.R')
source('get_non_gov_data.R')
source('extract_oes_data.R')
names(which(apply(df, 2, function(i){sum(is.na(i)) > 0})))

# Split weights from values
wt_cols <- c('asian', 'bilingual', 'black', 'commute_t', 'fam_size', 'hispanic',
             'no_insurance', 'over65', 'persons_in_poverty', 'under18',
             'with_disability', 'vet_wt', 'people_per_unit', 'years_mortgage',
             'transport_wt', 'house_rep_net', 'house_rep_nofood', 'rent_wt')

vl_cols <- c('bachelor', 'broadband', 'computer', 'emp_growth', 'fem_labor',
             'fem_persons', 'foreigners', 'high_school', 'home_owners', 
             'labor_force', 'non_movers', 'percap_income', 'pop_growth',
             'retail_capita', 'white', 'active_build', 'rest_cost', 
             'social_percap', 'retail_percap', 'mortgage_wt', 'firm_size',
             'emp_rate', 'small_firms')

wt_dt <- 
  df[, .SD, .SDcols = wt_cols] %>% 
  cbind(df$variable, .) %>% 
  setnames(., old = 'V1', new = 'state')

vl_dt <- 
  df[, .SD, .SDcols = vl_cols] %>% 
  cbind(df$variable, .) %>%
  setnames(., old = 'V1', new = 'state')

rm(wt_cols, vl_cols, df)

# Pick values relevant to analysis to simplify weight assignment and interpretation
old_vl <- old_vl[, .SD, .SDcols = c('state', 'avg_hottest', 'cost_area_rt')]
vl_dt  <- vl_dt[, .SD, .SDcols = c('state', 'home_owners', 'mortgage_wt')]
values_data  <- 
  merge(vl_dt, old_vl, by = 'state', all.x = TRUE) %>% 
  merge(., oes_data, by = 'state', all.x = TRUE) %>% 
  .[complete.cases(.), ]
rm(old_vl, vl_dt, oes_data)

# Convert weights to uncorrelated principal components
# Remove all correlated variables with absolute correlation more than 0.5
states  <- wt_dt$state
wt_dt   <- merge(wt_dt, old_wt, by = 'state') %>% .[, !c('state'), with = FALSE]
my_data <- wt_dt

normal_data <- scale(my_data)
corr_matrix <- cor(normal_data)
# corrplot::corrplot(corr_matrix, method = 'number', type = 'lower')

pca_data <- princomp(normal_data)
summary(pca_data)

component_weights <- c(0.32, 0.17, 0.15, 0.09, 0.09, 0.05, 0.04)

new_data <- pca_data$scores[, 1:7]
new_corr <- cor(new_data)
# corrplot::corrplot(new_corr, method = 'number', type = 'lower')

new_data <- 
  as.data.table(new_data) %>% 
  setnames(., colnames(.), paste0('comp', seq(ncol(.))))

wt_data <- cbind(states, new_data)
setnames(wt_data, 'states', 'state')
wt_data <- wt_data[state %in% values_data$state, ]
rm(old_wt, wt_dt, states, new_data, my_data, normal_data, corr_matrix, new_corr, pca_data)

# Drop states that have 0 values for any of the variables in values section as
# we are not interested in those states
bad_states  <- values_data[['state']][which(rowSums(values_data == 0) > 0)]
values_data <- values_data[!(state %in% bad_states), ]
wt_data     <- wt_data[!(state %in% bad_states), ]

states      <- values_data$state
values_data <- values_data[, !c('state'), with = FALSE]
wt_data     <- wt_data[, !c('state'), with = FALSE]
rm(bad_states)

# Rank states before giving it the weights
wt_ranks <- as.data.table(apply(wt_data, 2, frank, ties.method = 'min'))
vl_ranks <- as.data.table(apply(values_data, 2, frank, ties.method = 'min'))

# Add weights to each feature to reduce impact from less important features
component_weights <- setNames(component_weights, colnames(wt_ranks))
wt_ranks <- as.data.table(
  lapply(setNames(colnames(wt_ranks), colnames(wt_ranks)),
         function(i){round(wt_ranks[[i]] * component_weights[[i]], 3)})
)

rm(component_weights)