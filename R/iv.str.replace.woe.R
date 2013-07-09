#' Create data frame where values are replaced with information value
#'
#' Calculates information value for all character vectors in given data frame
#'
#' @param df data frame with at least two columns
#' @param y column (integer) with binary outcome
#' @param summary Only total information value for variable is returned when summary is TRUE
#' @export
#' @examples
#' iv.str.all(german_data,"gbbin")
#' iv.str.all(german_data,"gbbin",T)

iv.str.all <- function(df,y,summary=F) {
  char_col_bol <- sapply(names(df),function (x) is.character(df[,x]))
  char_col_list <- c(names(char_col_bol[which(char_col_bol==TRUE)]))
  ivlist <- lapply(char_col_list, function (x) iv.str(df,x,"gbbin"))
  
  if (!summary) {
    ivlist <- lapply(char_col_list, function (x) iv.str(df,x,"gbbin"))
  } else {
    ivlist <- rbind.fill(ivlist)
    ivlist <- aggregate(ivlist$miv, by=list(Category=ivlist$variable), FUN=sum)    
  }
  ivlist
}