#' Calculate Information Value for character vectors
#'
#' This function calculates Information Value for character vectors
#'
#' @param df data frame with at least two columns
#' @param x column (character) for which Information Value should be calculated
#' @param y column (integer) with binary outcome
#' @export
#' @examples
#' iv.str(german_data,"purpose","gbbin")
iv.str <- function(df,x,y) {

  if (!is.character(df[, x])) {
    stop("Input (predictor x) is not a character!")
    } 
  if (!is.numeric(df[, y])) {
    stop("Outcome (y) is not a number!")
  } 
  if (length(unique(df[, y])) != 2) {
    stop("Not a binary outcome")
    }
  if (!all(sort(unique(df[, y])) == c(0,1))) {
    stop("Outcome (y) not encoded as 0 and 1.")
  }

  #length(unique(df[, y])),
  iv_data <- data.frame(unclass(table(df[, x],df[, y])))
  names(iv_data) <- c("outcome_0","outcome_1")
  iv_data$class <- row.names(iv_data)
  iv_data$variable <- x
  iv_data <- iv_data[c(4,3,1,2)]
  total_1 <- sum(df[, y])
  total_0 <- nrow(df) - total_1
  iv_data$pct_1 <- iv_data$outcome_1 / total_1
  iv_data$pct_0 <- iv_data$outcome_0 / total_0
  iv_data$odds <- iv_data$pct_1 / iv_data$pct_0
  iv_data$woe <- log(iv_data$odds)
  iv_data$miv <- (iv_data$pct_1 - iv_data$pct_0) / iv_data$woe
  iv_data
}
