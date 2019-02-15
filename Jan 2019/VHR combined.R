library(readxl)
host <- read_excel("Postdoc-R/Exported Tables/allexp_sytox_minusvp.xlsx")

virbac <- read_excel("Postdoc-R/Exported Tables/allexp_virbac minus vp.xlsx")

#extract cell and viral count
cell <- host %>% filter(stain %in% c("countperml"))
ehv <- virbac %>% filter(cell %in% c("EhV"))

cell <- cell%>% arrange (factor(maingroup, c("still-control", "still-infected", 
                                             "turbulent-control", "turbulent-infected", 
                                             "still-viral particles", "turbulent-viral particles")))

ehv <- ehv%>% arrange (factor(maingroup, c("still-control", "still-infected", 
                                           "turbulent-control", "turbulent-infected", 
                                           "still-viral particles", "turbulent-viral particles")))

cell <-  cell[! cell$exp=="1st",  ]
ehv <- ehv[! ehv$exp=="1st",  ]

cell.ehv <- cbind(cell [c(6)], ehv [c(2:12, 14)])
cell.ehv$VH <- cell.ehv$count/cell.ehv$value
cell.ehv$VHdiv <- cell.ehv$VH/10^4

ggplotly(ggplot(data=cell.ehv, aes(x=time, y=VHdiv, colour=group2)) +geom_boxplot() + 
           facet_grid(exp~group1, scales="free")+ geom_point()+ theme_bw())

sum.all <- summarySE(cell.ehv.dropvp, measurevar = "VHdiv", 
                     groupvars = c("maingroup", "group1", "group2", "time", "exp"))


cell.ehv$supergroup2 <- factor(cell.ehv$supergroup2, 
                          levels = c("still-control-0 rpm",  "still-infected-0 rpm", "still-viralparticles-0 rpm", 
                                     "turbulent-control-5 rpm" , "turbulent-infected-5 rpm", 
                                     "turbulent-viralparticles-5 rpm",
                                     "turbulent-control-15 rpm", "turbulent-infected-15 rpm",
                                     "turbulent-viralparticles-15 rpm"))

##what I wanted
resize.win(8.5, 5.8)
ggplot(data=cell.ehv, aes(x=time, y=VHdiv, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), size=1.5, alpha=0.2) + 
  labs (y= expression("EhV:E. huxleyi" ~ scriptstyle (x) ~"10"^~4), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) + 
  theme_Publication2() + 
  theme(legend.key.width=unit(3,"line"), legend.title = element_blank()) +  
  guides(linetype=guide_legend(ncol=3), colour=guide_legend(ncol=3), shape=guide_legend(ncol=3), 
         fill=guide_legend(ncol=3) ) +
  theme(strip.text = element_blank(), legend.title=element_blank())
