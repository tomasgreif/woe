#' Recreate sample datasets
#'
#' Internal function used to simulate bigger datasets
#'

iv.gen.data <- function() {
  german_data_10k <- german_data[rep(1:nrow(german_data),10),]
  rownames(german_data) <- seq_along(german_data[,1])
  save(german_data_10k,file="data//german_data_10k.RData")
  
  german_data_100k <- german_data_10k[rep(1:nrow(german_data_10k),10),]
  rownames(german_data_100k) <- seq_along(german_data_100k[,1])
  save(german_data_100k,file="data//german_data_100k.RData")
  
  german_data_1000k <- german_data_100k[rep(1:nrow(german_data_100k),10),]
  rownames(german_data_1000k) <- seq_along(german_data_1000k[,1])
  save(german_data_1000k,file="data//german_data_1000k.RData")  
}
