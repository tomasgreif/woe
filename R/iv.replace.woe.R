#' Replace values with information value
#'
#' Calculates information value for all character vectors in given data frame
#'
#' @param df data frame with original data
#' @param iv list of information values for variables - output from iv.str.mult
#' @param verbose Prints additional details when TRUE. Useful mainly for debugging.
#' @export
#' @examples
#' outiv <- iv.mult(german_data,"gb",vars=c("ca_status","housing","duration"))
#' x <- iv.replace.woe(german_data,outiv)
#' str(x)
#' outiv <- iv.mult(german_data,"gb")
#' x <- iv.replace.woe(german_data,outiv)
#' str(x)

iv.replace.woe <- function(df,iv,verbose=FALSE) {

iv_df <- rbind.fill(iv)

 for (n in iv) { 
   variable_name <- n[1,1]
   variable_name_woe <- paste(variable_name,"_woe",sep="")

  if(verbose) {
    cat(paste0("Var Name: ",variable_name,"\n"))
    cat(paste0("WOE Name: ",variable_name_woe,"\n"))       
  }
 
   if(!("sql" %in% colnames(n)))
   {
     sqlstr <-  paste("select df.*, iv.woe as ", variable_name_woe ," from df join iv_df as iv on (df.", variable_name ," = iv.class and iv.variable ='",variable_name,"')",sep="")
     df <- sqldf(sqlstr,drv="SQLite")
   } else {
     sqlstr_woe <- ifelse(paste(n$sql,collapse= " ")=="when  then 0.0" || any(is.infinite(n$woe)) ,"0",paste("case ",paste(n$sql,collapse= " "),"else 0 end"))
     sqlstr <- paste("select df.*,",sqlstr_woe," as", variable_name_woe, "from df")
     df <-sqldf(sqlstr,drv="SQLite")
   }

 }
   df
}


