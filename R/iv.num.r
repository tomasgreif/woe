#' Calculate Information Value for numeric (double/integer) vectors
#'
#' This function calculates information value for numeric vectors. This is done using decision tree.
#'
#' @param df data frame with at least two columns
#' @param x column (character) for which Information Value should be calculated
#' @param y column (integer) with binary outcome
#' @examples
#' iv.num(german_data,"mob","gbbin")
#' iv.num(german_data,"age","gbbin")

iv.num <- function(df,x,y) {

  model   <- rpart(data=df,formula=df[,y]~df[,x],control=rpart.control(cp=0.001,minbucket=nrow(df)/100))
  df["___tmp_class_iv___"] <- as.character(model$where)
  iv_data <- iv.str(df,"___tmp_class_iv___",y)
  iv_data["variable"] <- x
  iv_data
}
