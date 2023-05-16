library(dplyr)
library(data.table)

extract_raw_data <- function(folder_name, file_name){
  return(
    fread(paste0(folder_name, file_name), stringsAsFactors = F)[1:67, ] %>% 
      .[, !c('Fact', 'Fact Note'), with = FALSE] %>%
      .[,
        !names(which(apply(., 2, function(i){sum(is.na(i)) == nrow(.)}))), 
        with = FALSE]
  )
}


# Read raw files and extract 67 rows that contain values. Drop columns which
# contain comments to the values as not relevant to analysis.
dt <- 
  lapply(
    list.files('./raw_data'), 
    function(i){extract_raw_data('./raw_data/', i)}
  ) %>%
  as.data.table(.) %>% 
  cbind(
    fread(paste0('./raw_data/', list.files('./raw_data')[[1]]))[1:67, 1], 
    .
  ) %>% 
  .[,
    !names(which(apply(., 2, function(i){sum(is.na(i)) > 0}))),
    with = FALSE] %>% 
  .[c(1, 5, 10, 11, 12, 13, 14, 16, 19, 21, 22, seq(23, 52, 1), 54, 55, 63, 64, 65), ]

# Replace fact names with future column names
new_names <- 
  tolower(dt[['Fact']]) %>% 
  gsub(",.*", "", .) %>% 
  gsub("-\\.*", "", .) %>% 
  gsub("population", 'pop', .) %>%
  gsub(" ", "_", .) %>% 
  gsub('_per_', '_', .) %>% 
  gsub('_a_', '_', .) %>% 
  gsub('estimates', 'est', .) %>% 
  gsub('_in_square_miles', '', .) %>% 
  gsub('_square_mile', '', .) %>% 
  gsub('_or_latino', '', .) %>% 
  gsub('persons_under_18_years', 'under18', .) %>% 
  gsub('persons_65_years_and_over', 'over65', .) %>% 
  gsub('female', 'fem', .) %>% 
  gsub('_alone', '', .) %>% 
  gsub('_or_african_american', '', .) %>% 
  gsub('owneroccupied_housing_unit_rate', 'home_owners', .) %>% 
  gsub('median_value_of_owneroccupied_housing_units', 'home_val', .) %>%
  gsub('median_selected_monthly_owner_costs_with_mortgage', 'life_cost', .) %>% 
  gsub('median_selected_monthly_owner_costs_without_mortgage', 'no_mortgage', .) %>% 
  gsub('median_gross_rent', 'rent_cost', .) %>% 
  gsub('living_in_same_house_1_year_ago', 'non_movers', .) %>%
  gsub('language_other_than_english_spoken_at_home', 'bilingual', .) %>%
  gsub('households_with_computer', 'computer', .) %>%
  gsub('households_with_broadband_internet_subscription', 'broadband', .) %>%
  gsub('high_school_graduate_or_higher', 'high_school', .) %>%
  gsub("bachelor's_degree_or_higher", 'bachelor', .) %>%
  gsub('persons__without_health_insurance', 'no_insurance', .) %>%
  gsub('total_accommodation_and_food_services_sales', 'restaurants', .) %>%
  gsub('total_health_care_and_social_assistance_receipts/revenue', 'social', .) %>%
  gsub('total_transportation_and_warehousing_receipts/revenue', 'transport', .) %>%
  gsub('total_retail_sales', 'retail', .) %>%
  gsub('total_retail_sales_capita', 'retail_percap', .) %>%
  gsub('mean_travel_time_to_work_(minutes)', 'commute', .) %>%
  gsub('median_household_income_(in_2021_dollars)', 'income', .) %>%
  gsub('per_capita_income_in_past_12_months_(in_2021_dollars)', 'percap_income', .) %>%
  gsub('total_employer_establishments', 'employers', .) %>%
  gsub('total_nonemployer_establishments', 'self_employed', .)

new_names[ 2] <- 'pop_growth'
new_names[11] <- 'foreigners'
new_names[20] <- 'fam_size'
new_names[29] <- 'labor_force'
new_names[30] <- 'fem_labor'
new_names[36] <- 'commute_t'
new_names[37] <- 'income'
new_names[38] <- 'percap_income'
new_names[41] <- 'employed'
new_names[42] <- 'emp_growth'
new_names[44] <- 'pop_dens1'
new_names[44] <- 'pop_dens0'

dt[['col_name']] <- new_names
rm(new_names, extract_raw_data)

df <- 
  dt[, !c('Fact'), with = FALSE] %>% melt(., id.var = 'col_name') %>% 
  dcast(., variable ~ col_name) %>% 
  .[, variable := as.character(variable), ] %>% 
  setorder(., variable)

# Make all feature columns numeric to allow computations
has_percent <- names(which(apply(df, 2, function(i){any(grepl('%', i))})))
has_comma   <- names(which(apply(df, 2, function(i){any(grepl(',', i))})))
has_dollar  <- names(which(apply(df, 2, function(i){any(grepl('\\$', i))})))
is_numeric  <- setdiff(colnames(df), 'variable')

df[ , 
    (has_percent) := lapply(.SD, function(i){gsub("%", "", i)}), 
    .SDcols = has_percent]

df[ , 
    (has_comma) := lapply(.SD, function(i){gsub(",", "", i)}), 
    .SDcols = has_comma]

df[ , 
    (has_dollar) := lapply(.SD, function(i){gsub("\\$", "", i)}), 
    .SDcols = has_dollar]

df[ , (is_numeric) := lapply(.SD, as.numeric), .SDcols = is_numeric]

df[is.na(df)] <- 0

names(which(apply(df, 2, function(i){sum(is.na(i)) > 0})))
rm(has_comma, has_dollar, has_percent, is_numeric)

# column_match <- dt[, .SD, .SDcols = c('Fact', 'col_name')]
rm(dt)
