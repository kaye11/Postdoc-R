##density plots
#shelf
Ehux.abundance1.shelf <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="abundance") %>% filter (depthf=="shelf/slope"), 
                            aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance1.shelf <- ggplot_build(Ehux.abundance1.shelf)$data[[1]] %>% select (x,y)
Ehux.abundance1.shelf$entity <- "E. huxleyi"
Ehux.abundance1.shelf$step <- "abundance"
Ehux.abundance1.shelf$depthf <- "shelf/slope"

Ehux.enccomb1.shelf <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="enccomb") %>% filter (depthf=="shelf/slope"),  
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb1.shelf <- ggplot_build(Ehux.enccomb1.shelf)$data[[1]] %>% select (x,y)
Ehux.enccomb1.shelf$entity <- "E. huxleyi"
Ehux.enccomb1.shelf$step <- "enccomb"
Ehux.enccomb1.shelf$depthf <- "shelf/slope"


Ehux.adscomb1.shelf <- ggplot (data=NAdata.betas_comb_1m_cn  %>% filter (step=="adscomb") %>% filter (depthf=="shelf/slope"), 
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb1.shelf <- ggplot_build(Ehux.adscomb1.shelf)$data[[1]] %>% select (x, y)
Ehux.adscomb1.shelf$entity <- "E. huxleyi"
Ehux.adscomb1.shelf$step <- "adscomb"
Ehux.adscomb1.shelf$depthf <- "shelf/slope"

Ehux.infcomb1.shelf <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="infcomb") %>% filter (depthf=="shelf/slope"),
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb1.shelf <- ggplot_build(Ehux.infcomb1.shelf)$data[[1]] %>% select (x, y)
Ehux.infcomb1.shelf$entity <- "E. huxleyi"
Ehux.infcomb1.shelf$step <- "infcomb"
Ehux.infcomb1.shelf$depthf <- "shelf/slope"

#open ocean
Ehux.abundance1.open <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="abundance") %>% filter (depthf=="open ocean"), 
                                 aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance1.open <- ggplot_build(Ehux.abundance1.open)$data[[1]] %>% select (x, y)
Ehux.abundance1.open$entity <- "E. huxleyi"
Ehux.abundance1.open$step <- "abundance"
Ehux.abundance1.open$depthf <- "open ocean"

Ehux.enccomb1.open <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="enccomb") %>% filter (depthf=="open ocean"),  
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb1.open <- ggplot_build(Ehux.enccomb1.open)$data[[1]] %>% select (x,y)
Ehux.enccomb1.open$entity <- "E. huxleyi"
Ehux.enccomb1.open$step <- "enccomb"
Ehux.enccomb1.open$depthf <- "open ocean"


Ehux.adscomb1.open <- ggplot (data=NAdata.betas_comb_1m_cn  %>% filter (step=="adscomb") %>% filter (depthf=="open ocean"), 
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb1.open <- ggplot_build(Ehux.adscomb1.open)$data[[1]] %>% select (x, y)
Ehux.adscomb1.open$entity <- "E. huxleyi"
Ehux.adscomb1.open$step <- "adscomb"
Ehux.adscomb1.open$depthf <- "open ocean"

Ehux.infcomb1.open <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="infcomb") %>% filter (depthf=="open ocean"),
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb1.open <- ggplot_build(Ehux.infcomb1.open)$data[[1]] %>% select (x, y)
Ehux.infcomb1.open$entity <- "E. huxleyi"
Ehux.infcomb1.open$step <- "infcomb"
Ehux.infcomb1.open$depthf <- "open ocean"

Ehux_1m_sep <- rbind (Ehux.abundance1.shelf, Ehux.enccomb1.shelf, Ehux.adscomb1.shelf, Ehux.infcomb1.shelf, 
                   Ehux.abundance1.open, Ehux.enccomb1.open, Ehux.adscomb1.open, Ehux.infcomb1.open)

Ehux_1m_sep$step <-  reorder.factor (Ehux_1m_sep$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 
Ehux_1m_sep$depthf <-  reorder.factor (Ehux_1m_sep$depthf, new.order = c("shelf/slope", "open ocean")) 

Ehux1_sep.png <- ggplot (data=Ehux_1m_sep, aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Ehux1_sep.png", width = 17, height = 6, dpi = 600, device="png", units="in")

##based on this, an infection step

forinfcomb <- Ehux_1m_sep %>% filter (step=="infcomb")
forinfcomb$percent <- (10^(forinfcomb$x))*100

nrow(forinfcomb[forinfcomb$percent<1, ]) #99
nrow(forinfcomb[forinfcomb$percent>1, ]) #84

resize.win (6, 6)

ggplot (data=Ehux_1m_sep %>% filter (step=="infcomb"), aes(x=log10((10^x)*100), y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  scale_x_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  labs (x=expression("% population infected "~d^-1), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom")

