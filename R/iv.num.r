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

  model   <- rpart(data=df,formula=as.integer(df[,y])~df[,x],control=rpart.control(cp=0.001,minbucket=nrow(df)/10))
  model_where <- data.frame(node_id=model$where,obs_id=as.numeric(names(model$where)),stringsAsFactors=F) # str(model_where)
  model_frame <- data.frame(model$frame,tree_node=rownames(model$frame),node_id=row(model$frame["var"]))
  
  log <- capture.output({
    rpart.rules <- path.rpart(model,rownames(model$frame)[model$frame$var=="<leaf>"])
  })  
  tree_rules <- iv.parse.rpart.rule(x,rpart.rules)
    
  t <- sqldf("select obs_id, tr.class_label as tmp_iv_calc_label
              from 
                  model_where mw
                  join model_frame mf using (node_id)        
                  join tree_rules tr using (tree_node)")
  df <- merge(df, t["tmp_iv_calc_label"], by=0, all=TRUE) # str(df)
  iv_data <- iv.str(df,"tmp_iv_calc_label",y)
  iv_data$variable <- x
  sqldf("select iv.* from iv_data iv join tree_rules tr on (iv.class=tr.class_label) order by tr.min")
}

