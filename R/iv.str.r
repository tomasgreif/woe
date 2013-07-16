#' Calculate Information Value for character or factor columns
#'
#' This function calculates information value for character or factor columns of data frame. 
#'
#' @param df data frame with at least two columns (predictor x and outcome y)
#' @param x column (character or factor) for which Information Value should be calculated
#' @param y column (integer) with binary outcome. y has to be a column in df data frame. y has to be numeric
#' (integer or double) with values 0 and 1. It is recommended that 1 is used for predicted event and 0 otherwise.
#' @examples
#' iv.str(german_data,"purpose","gbbin")
#' iv.str(german_data,"savings","gbbin")

iv.str <- function(df,x,y) {
  if (!(class(df)=="data.frame")) {
    stop("Parameter df has to be a data frame.")
  } 
  if (!(is.character(df[, x]) || is.factor(df[, x]))) {
    stop("Input is not a character or factor!")
    } 
  if (!(is.numeric(df[, y]) || is.factor(df[, y]))) {
    stop("Outcome is not a number!")
  } 
  if (length(unique(df[, y])) != 2) {
    stop("Not a binary outcome")
    }
  if (!(all(sort(unique(df[, y])) == c(0,1)) || all(sort(unique(as.integer(df[, y])) == c(1,2))))) {
    ifelse(is.numeric(df[, y]), stop("Outcome not encoded as 0 and 1."), 
           stop("Outcome not encoded as 1 and 2."))
  }

  iv_data <- data.frame(unclass(table(df[, x],df[, y])))
  names(iv_data) <- c("outcome_0","outcome_1")
  iv_data$class <- row.names(iv_data)
  iv_data$variable <- x
  iv_data <- iv_data[c(4,3,1,2)]
  total_1 <- ifelse(is.numeric(df[, y]), sum(df[, y]), sum(as.integer(df[, y])-1))
  total_0 <- nrow(df) - total_1
  
  #if(any(iv_data$outcome_0 == 0)) {
  #  warning("Some group for outcome 0 has zero count. This will result in -Inf or Inf WOE.")
  #} else if (any(iv_data$outcome_1 == 0)) {
  #  warning("Some group for outcome 1 has zero count. This will result in -Inf or Inf WOE.")
  #}
  
  iv_data$pct_1 <- iv_data$outcome_1 / total_1
  iv_data$pct_0 <- iv_data$outcome_0 / total_0
  iv_data$odds <- iv_data$pct_1 / iv_data$pct_0
  iv_data$woe <- log(iv_data$odds)
  iv_data$miv <- (iv_data$pct_1 - iv_data$pct_0) * iv_data$woe
  iv_data
}

