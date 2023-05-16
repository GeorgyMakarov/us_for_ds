library(dplyr)
library(data.table)
library(GA)
source('make_input_data.R')

fitness <- function(x)
{
  current_survpoint <- x %*% my_data$values
  current_weight    <- x %*% my_data$weight
  if (current_weight > max_weight){
    return(0)
  } else {
    return(current_survpoint)
  }
}

ga_output <- ga(type       = 'binary',
                fitness    = fitness,
                nBits      = nrow(my_data),
                maxiter    = 120,
                popSize    = 10000,
                seed       = 123,
                keepBest   = FALSE,
                monitor    = FALSE)

best_solution <- as.numeric(ga_output@solution)
best_states   <- 
  my_data[best_solution == 1, ] %>% 
  setorder(., -values) %>% 
  head(5) %>% 
  pull(., var = 'states')

print(best_states)

# summary(ga_output)
# plot(ga_output)
