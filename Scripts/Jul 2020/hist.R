ggplot (data=NAdata_merge, aes(x=log10(abundance), color=depthf, fill=depthf)) +
  geom_bar(aes (y = ..count../sum(..count..)), alpha=0.25) +
  #geom_density(alpha=.2, position="stack", color="black", size=0.5) + 
  #geom_density(alpha=.2, size=1, position= "identity", aes (y = ..count../sum(..count..))) +
  scale_x_continuous(n.breaks = 6) +
  theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) +   facet_grid(depthf~entity)

a <- ggplot (data=NAdata_merge %>% filter (depthf=="<200 m") %>% filter (entity=="Li"), aes(x=log10(abundance), color=depthf, fill=depthf)) +
   geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
   #geom_density(alpha=.2, position="identity", color="black", size=0.5) + 
  scale_x_continuous(n.breaks = 6) +
  theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) #+   facet_grid(depthf~entity)
ag <- ggplot_build(a)$data[[1]]


ggplot (data=NAdata_merge, aes(x=log10(abundance), fill=depthf)) + 
  geom_density(position = "identity", alpha=0.5, aes (y = ..count../sum(..count..)*10)) + scale_x_continuous(breaks=seq(1,6,1)) +
  theme_Publication2() + labs (x = expression(log[10]~"concentration"~~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit(2, "line")) + 
  facet_grid(~entity)

ggplot (data=NAdata_merge, aes(x=log10(abundance), color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  #geom_density() +
  #geom_density(alpha=.2, position="stack", color="black", size=0.5) + 
  geom_density(alpha=.2, color="black", size=0.5, position= "identity", aes (y = ..count../sum(..count..)*10)) +
  scale_x_continuous(n.breaks = 6) +
  theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1), y= "probability density") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) +   facet_grid(~entity)

ggplot (data=NAdata_merge, aes(x=log10(abundance), y=stat(density*0.1), color=depthf, fill=depthf)) +
  #geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  #geom_density() +
  #geom_density(alpha=.2, position="stack", color="black", size=0.5) + 
  #geom_density(alpha=.2, color="black", size=0.5, position= "identity", aes (y = binwidth*..density..)) +
  geom_histogram(binwidth=0.1, alpha=0.2) +
  geom_density (alpha=0.2,  position="stack") + 
  scale_x_continuous(n.breaks = 6) +
  theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1), y= "probability density") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) +   facet_grid(~entity)


ggplot (data=NAdata_merge %>% filter (entity=="Cc") %>% filter (depthf=="<200 m"), aes(x=log10(abundance), y=stat(density*0.05), color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  #geom_density() +
  #geom_density(alpha=.2, position="stack", color="black", size=0.5) + 
  #geom_density(alpha=.2, color="black", size=0.5, position= "identity", aes (y = binwidth*..density..)) +
  geom_histogram(binwidth=0.05, alpha=0.2) +
  geom_density (alpha=0.2,  position="stack") + 
  scale_x_continuous(n.breaks = 6) +
  theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm"), axis.title.y = element_blank()) #+   facet_grid(~depthf)

shelf<- NAdata_merge %>% filter (depthf=="<200 m")



ggplot (data=NAdata_merge, aes(x=log10(abundance), color=depthf, fill=depthf)) +
  #geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(aes (y=..count../sum(..count..)), alpha=0.2) +
  #geom_density(alpha=.2, position="stack", color="black", size=0.5) + 
  #geom_density(alpha=.2, color="black", size=0.5, position= "identity", aes (y = ..count../sum(..count..)*10)) +
  scale_x_continuous(n.breaks = 6) +
  theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1), y= "probability density") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) + facet_grid(~entity)


xyplot(~ log10(abundance) | entity, group = depthf, data = NAdata_merge, auto.key = TRUE)


library(scales) # ! important
library(ggplot2)
ggplot(NAdata_merge, aes(x=log10(abundance), fill=depthf)) +
  stat_bin(aes(y=..density..),  binwidth=0.1, color="white", alpha=0.2) +
  geom_line(stat="density", size = 1) +
  scale_y_continuous() + facet_grid(~entity) +
  theme_classic()
