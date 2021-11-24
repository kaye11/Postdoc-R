##this is to make a histogram of probability infections vs host concentration
#always use the comb data set
#for 30m

infprob_abundance30 <- NAdata.betas_comb_30m_cn %>% filter (step=="abundance")
infprob_abundance30 <- rename (infprob_abundance30, c("abundance" = "combvalue"))

infprob_inf30 <- NAdata.betas_comb_30m_cn %>% filter (step=="infcomb")
infprob_inf30 <- rename (infprob_inf30, c("infcomb" = "combvalue"))

infprob <- left_join ((infprob_abundance30 %>% select (c(lat, lon, depthf, abundance))), (infprob_inf30 %>% select (c(lat, lon, depthf, infcomb))))

#calculate infection probability
infprob$infprob <- infprob$infcomb*100
infprob$infprob2 <- infprob$infprob
infprob$infprob2[infprob$infprob2 == 0] <- NA

resize.win(9,6)
#make a bar chart
ggplot(data=infprob, aes (x=log10(abundance), y=log10(infprob2), color=depthf)) + geom_point(stat="identity") + theme_Publication() + 
  labs (x=expression(log[10]~"concentration"~mL^-1), y=expression(log[10]~"infection probability (%)")) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + theme (legend.title = element_blank())

ggplot(data=infprob, aes (x=log10(abundance), y=infprob2, color=depthf, fill=depthf)) +geom_point(stat="identity") +
  theme_Publication() + 
  labs (x=expression(log[10]~"concentration"~mL^-1), y=expression("infection probability (%)")) +
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) +
  theme (legend.title = element_blank()) + facet_grid(~depthf)

##infprob
infprob.shelf <- ggplot (NAdata.betas_comb_30m_cn %>% filter (depthf=="shelf/slope") %>% 
                           filter (step=="infcomb"), 
                         aes(x=(combvalue*100),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)))  + theme_Publication2() + 
  labs (x = "infection probability (%)") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

infprob.shelf <- ggplot_build(infprob.shelf)$data[[1]] %>% select (x, y)
infprob.shelf$depthf <- "shelf/slope"

infprob.open <- ggplot (NAdata.betas_comb_30m_cn %>% filter (depthf=="open ocean") %>% 
                          filter (step=="infcomb"), 
                        aes(x=(combvalue*100),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

infprob.open <- ggplot_build(infprob.open)$data[[1]] %>% select (x, y)
infprob.open$depthf <- "open ocean"

infprob_merged <- rbind (infprob.shelf, infprob.open)
infprob_merged$y[infprob_merged$y == 0] <- NA
infprob_merged$percentinf <- (10^(infprob_merged$x))*100


sum(infprob.shelf$y)

ggplot (data=infprob_merged, aes(x=log10(x),  y= y, color=depthf, fill=depthf)) + 
  geom_area(position = "identity", alpha=0.25) +
  geom_line(size=1) +
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) +
  theme_Publication2() + labs (x = "% population infected", y= "probability density") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

infprob_analysis <- Ehux_30m %>% filter (step=="infcomb")


ggplot (data=Ehux_30m %>% filter (step=="infcomb"), aes(x=(10^(x))*100, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values=c("#c51b8a")) + 
  scale_fill_manual (values=c("#c51b8a")) + 
  labs (x="%infection", y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "none")
