#' Calculate Information Value for defined columns in data frame
#'
#' Calculates information value for defined columns in given data frame. Columns can have numeric or character type.
#'
#' @param df data frame with at least two columns
#' @param y column (integer) with binary outcome
#' @param summary Only total information value for variable is returned when summary is TRUE. Output is sorted by
#' information value, starting with highest value.
#' @param vars List of variables. If not specified, all character variables will be used
#' @export
#' @examples
#' iv.mult(german_data,"gbbin")
#' iv.mult(german_data,"gbbin",T)
#' iv.mult(german_data,"gbbin",T,c("ca_status","housing","job","mob")) # str(german_data)
#' iv.mult(german_data,"gbbin",vars=c("ca_status","housing","job","mob","gb"))
#' iv.mult(german_data,vars=c('ca_status','mob','credit_history','purpose',
#'                            'credit_amount','savings','present_employment_since',
#'                            'status_sex','installment_rate_income','other_debtors',
#'                            'present_residence_since','property','age','other_installment',
#'                            'housing','existing_credits','job','liable_maintenance_people',
#'                            'telephone','foreign_worker'),
#'                            "gbbin", summary=T)


iv.mult <- function(df,y,summary=F,vars=NULL) {

  if(is.null(vars)) {
    vars <- names(df)[names(df) !=y]
  }
  
  ivlist <- lapply(vars, function (x) {
      if(is.numeric(df[,x])) {
        iv.num(df,x,y)
      } else iv.str(df,x,y)
    }
                   )
  
  if (summary) {
    ivlist <- rbind.fill(ivlist)
    ivlist <- aggregate(ivlist$miv, by=list(Category=ivlist$variable), FUN=sum)    
    ivlist <- ivlist[with(ivlist,order(-x)), ]
    
    ivlist$Strength[ivlist$x >= 1] <- 1
    ivlist$Strength[ivlist$x > .5 & ivlist$x < 1] <- 2
    ivlist$Strength[ivlist$x > .2 & ivlist$x < .5] <- 3
    ivlist$Strength[ivlist$x > .1 & ivlist$x < .2] <- 4
    ivlist$Strength[ivlist$x > .02 & ivlist$x < .1] <- 5
    ivlist$Strength[ivlist$x <= .02] <- 6
    ivlist$Strength <- factor(ivlist$Strength, levels=c(1,2,3,4,5,6), 
                              labels= c("Suspicious","Very strong","Strong","Average","Weak","Wery weak"))
    names(ivlist) <- c("Variable","InformationValue","Strength")
  }
  ivlist
}
