non_gov_dt <- 
  fread('./non_gov_data/state_data.csv', stringsAsFactors = FALSE) %>% 
  .[, !c('state_short', 'state_capital', 'region'), with = FALSE]

non_gov_dt$state <- gsub(' ', '.', non_gov_dt$state)

# Replace comma for dot in all columns to be able to convert it to numeric
non_gov_dt <- as.data.table(apply(non_gov_dt, 2, function(i){gsub(",", ".", i)}))
num_cols   <- setdiff(colnames(non_gov_dt), c('state'))
non_gov_dt <- non_gov_dt[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# Compute columns to be included into consideration
non_gov_dt$home_price_idx <- round(non_gov_dt$mzh / non_gov_dt$mhp, 2)

# Pick relevant variables and split them into weights and values
relevant_cols <- c('state' = 'state', 
                   'home_price_idx' = 'home_price_idx', 
                   'middle_class' = 'smc', 
                   'avg_hottest'  = 'aht', 
                   'avg_coldest'  = 'act', 
                   'avg_percip'   = 'ayp', 
                   'cost_area_rt' = 'csr',
                   'mean_level'   = 'mhl',
                   'air_quality'  = 'aqi',
                   'avg_ds_wage'  = 'ads',
                   'open_ds_job'  = 'qds')

old_dt <- 
  non_gov_dt[, .SD, .SDcols = relevant_cols] %>% 
  setnames(., colnames(.), names(relevant_cols))

rm(non_gov_dt, num_cols, relevant_cols)

old_wt <- 
  c('home_price_idx', 'avg_percip', 'air_quality') %>% 
  old_dt[, .SD, .SDcols = .] %>% 
  cbind(old_dt$state, .) %>% 
  setnames(., old = 'V1', new = 'state')

old_vl <- 
  setdiff(colnames(old_dt), c('state', 'home_price_idx', 'avg_percip', 'air_quality')) %>% 
  old_dt[, .SD, .SDcols = .] %>% 
  cbind(old_dt$state, .) %>% 
  setnames(., old = 'V1', new = 'state')

rm(old_dt)
