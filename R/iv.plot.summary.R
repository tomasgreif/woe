#' Plot summay information values for variables
#'
#' Plots calculated information values
#'
#' @param iv Information values summary for variables - output from iv.str.mult with summary=T
#' @export

iv.plot.summary <- function(iv) {

#  require(ggplot2)
#  require(RColorBrewer)
  fills <- rev(brewer.pal(6, 'Blues'))
  
  ggplot(data=iv)+
    geom_bar(aes(x=reorder(iv$Variable,iv$InformationValue),y=iv$InformationValue,fill=iv$Strength), stat="identity") + 
    coord_flip() +
    scale_fill_manual(values=fills) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype="dashed",colour="grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=element_blank),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()      
      ) +
     xlab("Variable") +
     ylab("Information Value") +
     ggtitle("Information Value Summary")  
 
}

