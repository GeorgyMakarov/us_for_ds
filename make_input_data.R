source('select_weights_values.R')

sum_ranks  <- rowSums(wt_ranks)
sum_value  <- rowSums(vl_ranks)
max_weight <- sum(tail(sort(sum_ranks), 5))
my_data    <- as.data.table(data.frame(states = states,
                                       weight = sum_ranks,
                                       values = sum_value))

rm(states, sum_ranks, sum_value, vl_ranks, wt_ranks)
