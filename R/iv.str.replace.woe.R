#' Replace values with infofrmation value
#'
#' Calculates information value for all character vectors in given data frame
#'
#' @param df data frame with original data
#' @param iv list of information values for variables - output from iv.str.mult
#' @export
#' @examples
#' outiv <- iv.str.mult(german_data,"gbbin",vars=c("ca_status","housing","job"))
#' x <- iv.str.replace(german_data,outiv)
#' str(x)
#' outiv <- iv.str.mult(german_data,"gbbin")
#' x <- iv.str.replace(german_data,outiv)
#' str(x)

iv.str.replace <- function(df,iv) {

require(sqldf)

iv_df <- rbind.fill(iv)

 for (n in iv) { 
   variable_name <- n[1,1]
   variable_name_woe <- paste(variable_name,"_woe",sep="")
   # print(paste0("Var Name: ",variable_name))
   # print(paste0("WOE Name: ",variable_name_woe))
   df[variable_name_woe] <- 1
   sqlstr <-  paste("select il.woe as ", variable_name_woe ," from df as gd join iv_df as il on (gd.", variable_name ," = il.class and il.variable ='",variable_name,"')",sep="")
   # print(sqlstr)
   df[variable_name_woe] <- sqldf(sqlstr,drv="SQLite")
 }
    df
}
