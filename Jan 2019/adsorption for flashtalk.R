library(readr)
adsorption <- read_csv("Postdoc-R/CSV Files/summary adsorption.csv")
#adsorption <- read_delim("Postdoc-R/CSV Files/summary adsorption.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE)

library(tidyr)
adsorption<- separate(adsorption, reservoir, into = paste("group", 1:2, sep = ""))
adsorption[is.na(adsorption)] <- c("lith")

cols <- c("virus", "group1", "group2", "type")
adsorption[cols] <- lapply(adsorption[cols], factor)
adsorption$reservoir <- as.factor(paste(adsorption$group1, adsorption$group2, sep='-'))
adsorption$group <- adsorption$group2
adsorption$group2 <- NULL

library(plotly)
library(ggplot2)

ggplotly(ggplot(data=adsorption, aes(x=group,y = Cd, color=type)) + geom_boxplot() + geom_point()) 

source("theme_Publication2.R")
source ("resizewin.R")

resize.win(8,6)

adsorption$group <- factor (adsorption$group,levels= c("naked", "calcified", "lith"))

require (Rmisc)
ads.sum <- summarySE (adsorption, measurevar = "Cd", groupvars = "group" )
adsorption$Cd_d <- adsorption$Cd*24*60

ggplot(data=adsorption %>% filter(group==c("naked", "calcified") & type=="fast"), 
       aes(x=group,y = Cd_d)) + geom_boxplot(size=1) + #change to group for other plots
  geom_point(size=5) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=5),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm")) +
  labs(y = expression("adsorption coefficient " ~(mL~day^-1)))+
  theme(legend.title = element_blank(), axis.title.x = element_blank())



