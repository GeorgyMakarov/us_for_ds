my_data <- fread('weights_data.csv', stringsAsFactors = F)
states  <- my_data$state
my_data <- my_data[, !c('state'), with = F]

normal_data <- scale(my_data)
corr_matrix <- cor(normal_data)
corrplot::corrplot(corr_matrix, method = 'number', type = 'lower')

pca_data <- princomp(normal_data)
summary(pca_data)

new_data <- pca_data$scores[, 1:7]
new_corr <- cor(new_data)
corrplot::corrplot(new_corr, method = 'number', type = 'lower')

new_data <- 
  as.data.table(new_data) %>% 
  setnames(., colnames(.), paste0('comp', seq(ncol(.))))

rm(list = setdiff(ls(), 'new_data')) # TODO: replace when embedded into main code
