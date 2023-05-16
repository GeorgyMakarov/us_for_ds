# OES data describes statistics of data science jobs by state. Drop hourly 
# rates as those are strongly correlated with annual salaries and do not add 
# valuable information to out analysis. Drop annual wage quantile except median
# as the latter is required to compute coefficients of asymmetry to describe 
# wage distribution.
oes_data <- 
  readxl::read_xlsx('./gov_data/data_scientists_oes_report.xlsx') %>% 
  as.data.table() %>% 
  .[, !colnames(.)[grepl('rate', colnames(.))], with = FALSE] %>% 
  .[, .SD, .SDcols = c('state', 'employed', 'annual_wage', 'wage_median', 'loc_quot', 'sigma_wage')]

# Compute skewness coefficient to distinct between stages where more jobs earn
# more than average salary. Unlike generally accepted skewness computation
# subtract average from median to compute skewness because then larger values
# will correspond to negative skew and it will be handy to convert them into
# scaled vector from 0 to 1 to be able to use it for ranking.
tmp <- oes_data[, .SD, .SDcols = c('annual_wage', 'wage_median', 'sigma_wage')]
tmp$denomin <- tmp$annual_wage * (100 - tmp$sigma_wage) / 100
tmp$skew_k  <- round((tmp$wage_median - tmp$annual_wage) * 3 / tmp$denomin, 2)

tmp[is.na(tmp)] <- 0
tmp$skewness_k  <- round(scales::rescale(tmp$skew_k, to = c(0, 1)), 2)

oes_data$skewness <- tmp$skewness_k

oes_data <- oes_data[, !c('annual_wage', 'sigma_wage'), with = FALSE]
oes_data$state <- gsub(' ', '.', oes_data$state)
rm(tmp)
