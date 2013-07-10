#' Calculate Information Value for all character columns in data frame
#'
#' Calculates information value for all character vectors in given data frame
#'
#' @param df data frame with at least two columns
#' @param y column (integer) with binary outcome
#' @param summary Only total information value for variable is returned when summary is TRUE. Output is sorted by
#' information value, starting with highest value.
#' @param vars List of variables. If not specified, all character variables will be used
#' @export
#' @examples
#' iv.str.mult(german_data,"gbbin")
#' iv.str.mult(german_data,"gbbin",T)
#' iv.str.mult(german_data,"gbbin",T,c("ca_status","housing","job"))
#' iv.str.mult(german_data,"gbbin",vars=c("ca_status","housing","job"))

iv.str.mult <- function(df,y,summary=F,vars=NULL) {

  require(plyr)
  
  if(is.null(vars)) {
    char_col_bol <- sapply(names(df),function (x) is.character(df[,x]))
    char_col_list <- c(names(char_col_bol[which(char_col_bol==TRUE)]))    
  } else {
    char_col_list = vars    
  }

  ivlist <- lapply(char_col_list, function (x) iv.str(df,x,y))
  
  if (!summary) {
    ivlist <- lapply(char_col_list, function (x) iv.str(df,x,y))
  } else {
    ivlist <- rbind.fill(ivlist)
    ivlist <- aggregate(ivlist$miv, by=list(Category=ivlist$variable), FUN=sum)    
    ivlist <- ivlist[with(ivlist,order(-x)), ]
  }
  ivlist
}

