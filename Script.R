library(tidyverse)
library(ggplot2)
library(viridis)

d<-read.csv("common_comparisons",sep="\t", header=TRUE)
d$Gene<-factor(d$Gene,levels=unique(d$Gene))
d <- d %>% gather(key = "observation", value="value",-c(1,2))
empty_bar <- 0
nObsType <- nlevels(as.factor(d$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(d$group)*nObsType, ncol(d)) )
colnames(to_add) <- colnames(d)
to_add$group <- rep(levels(d$group), each=empty_bar*nObsType )
d <- rbind(d, to_add)
d <- d %>% arrange(group, Gene)
d$id <- rep( seq(1, nrow(d)/nObsType) , each=nObsType)
label_data <- d %>% group_by(id, Gene) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
base_data <- d %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
grid_data <- base_data
grid_data$end <- grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,] 

p <- ggplot(d) +      

  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), color="black",stat="identity", alpha=0.5) +
  scale_fill_manual(values=c("#b300b3","#ccff33","orange","#00ffff")) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start-5, yend = 0), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start-5, yend = 100), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start-5, yend = 200), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 300, xend = start-5, yend = 300), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 500, xend = start-5, yend = 500), colour = "black", alpha=1, size=0.05 , inherit.aes = FALSE ) +
  
  #ggplot2::annotate("text", x = rep(max(d$id),5), y = c(0, 100, 200, 300, 500), label = c("0", "100", "200", "300", "500") , color="black", size=5 , angle=0, fontface="bold", hjust=-3) +  
  ylim(-350,550) +
  theme_minimal() +
  theme(
    legend.position = c(0.5,0.5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm") 
  ) +
  coord_polar(start=0) +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=Gene, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle=label_data$angle, inherit.aes = FALSE )
p

