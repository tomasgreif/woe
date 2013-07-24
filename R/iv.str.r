#' Calculate Information Value for character or factor columns
#'
#' This function calculates information value for character or factor columns of data frame. 
#'
#' @param df data frame with at least two columns (predictor x and outcome y)
#' @param x column (character or factor) for which Information Value should be calculated
#' @param y column (integer) with binary outcome. y has to be a column in df data frame. y has to be numeric
#' (integer or double) with values 0 and 1. It is recommended that 1 is used for predicted event and 0 otherwise.
#' @param verbose Prints additional details when TRUE. Useful mainly for debugging.
#' @export
#' @examples
#' iv.str(german_data,"purpose","gb")
#' iv.str(german_data,"savings","gb")

iv.str <- function(df,x,y,verbose=FALSE) {
  if (!(class(df)=="data.frame")) {
    stop("Parameter df has to be a data frame.")
  } 
  if (!(is.character(df[, x]) || is.factor(df[, x]))) {
    stop(paste("Input is not a character or factor! Variable:", x))
    } 
  if (!(is.numeric(df[, y]) || is.factor(df[, y]))) {
    stop("Outcome is not a number nor factor!")
  } 
  if (length(unique(df[, y])) != 2) {
    if(verbose) paste(cat(unique(df[,y])),"\n")
    stop("Not a binary outcome")
    }
  if (!(all(sort(unique(df[, y])) == c(0,1))) && is.numeric(df[,y])) {
    stop("Numeric outcome has to be encoded as 0 (good) and 1 (bad). \n")
  }
  if (is.factor(df[,y]) && all(levels(df[,y])[order(levels(df[,y]))]==c("bad","good"))) {
    if (verbose) cat("Assuming good = level 'good' and bad = level 'bad' \n")
    total_1 <- sum(df[,y]=="bad")
    } else if (is.factor(df[,y])) {
    if (verbose) cat("Factor: Assuming bad = level 2 and good = level 1 \n")
    total_1 <- sum(as.integer(df[, y])-1)

    } else {
    if (verbose) cat("Numeric: Assuming bad = 1 and good = 0 \n")
    total_1 <-sum(df[, y])

  }

  total_0 <- nrow(df) - total_1      
  iv_data <- data.frame(unclass(table(df[, x],df[, y])))
  
  if (all(names(iv_data)==c("bad","good"))) {
    iv_data <- iv_data[,c(2,1)]
  }
  
  names(iv_data) <- c("outcome_0","outcome_1")
  iv_data$class <- row.names(iv_data)
  iv_data$variable <- x
  iv_data <- iv_data[c(4,3,1,2)]

  iv_data$pct_1 <- iv_data$outcome_1 / total_1
  iv_data$pct_0 <- iv_data$outcome_0 / total_0
  iv_data$odds <- iv_data$pct_0 / iv_data$pct_1
  iv_data$woe <- log(iv_data$odds)
  iv_data$miv <- (iv_data$pct_0 - iv_data$pct_1) * iv_data$woe

  if(any(iv_data$outcome_0 == 0) || any(iv_data$outcome_1 == 0)) {
    warning("Some group for outcome 0 has zero count. This will result in -Inf or Inf WOE. Replacing - ODDS=1, WoE=0, MIV=0. \n The bin is either too small or suspiciously predictive. \n You should fix this before running any model. It does not make any sense to keep WoE = 0 for such bin.")
      iv_data$woe <- ifelse(is.infinite(iv_data$woe),0,iv_data$woe)
      iv_data$miv <- ifelse(is.infinite(iv_data$miv),0,iv_data$miv)
      iv_data$odds <- ifelse(is.infinite(iv_data$odds),1,iv_data$odds)
  }
  
  rownames(iv_data) <- NULL
  iv_data
}

