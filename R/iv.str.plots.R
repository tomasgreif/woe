x <- iv.str(german_data,"purpose","gbbin")
x$pct_diff <- x$pct_1 - x$pct_0

require(gridExtra)
require(reshape2)
require(ggplot2)

d <- melt(x[,c("variable","class","outcome_1","outcome_0")],variable.name="melt_var")
p <- ggplot(data=d)

p +
  geom_bar(aes(x=class,y=value,fill=melt_var),stat="identity",position="dodge",alpha=.5) +
  scale_fill_manual(values=c("red", "green"), labels=c("1","0")) +
  xlab("Class of variable X") +
  ylab("Observations") +
  labs(fill="Outcome") +
  theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype="dashed",colour="grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill=element_blank),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()      
      ) +
  ggtitle(paste("Counts for variable:",x[1,1]))

ggplot(data=iv.str(german_data,"purpose","gbbin")) +
  geom_bar(aes(x=class,y=miv),stat="identity",position="dodge",alpha=.5) +
  scale_fill_manual(values=c("red", "green"), labels=c("1","0")) +
  xlab("Class of variable X") +
  ylab("Observations") +
  labs(fill="Outcome") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype="dashed",colour="grey"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill=element_blank),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()      
  ) +
  ggtitle(paste("Counts for variable:",x[1,1]))