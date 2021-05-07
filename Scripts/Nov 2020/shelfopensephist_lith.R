##density plots
#shelf
lith.abundance30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="abundance") %>% 
                                    filter (depthf=="shelf/slope"), 
                                  aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
lith.abundance30.shelf <- ggplot_build(lith.abundance30.shelf)$data[[1]] %>% select (x,y)
lith.abundance30.shelf$entity <- "lith"
lith.abundance30.shelf$step <- "abundance"
lith.abundance30.shelf$depthf <- "shelf/slope"

lith.enccomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="enccomb") %>% filter (depthf=="shelf/slope"),  
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.enccomb30.shelf <- ggplot_build(lith.enccomb30.shelf)$data[[1]] %>% select (x,y)
lith.enccomb30.shelf$entity <- "lith"
lith.enccomb30.shelf$step <- "enccomb"
lith.enccomb30.shelf$depthf <- "shelf/slope"

lith.adscomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith")  %>% filter (step=="adscomb") %>% filter (depthf=="shelf/slope"), 
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.adscomb30.shelf <- ggplot_build(lith.adscomb30.shelf)$data[[1]] %>% select (x, y)
lith.adscomb30.shelf$entity <- "lith"
lith.adscomb30.shelf$step <- "adscomb"
lith.adscomb30.shelf$depthf <- "shelf/slope"

#open ocean

lith.abundance30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="abundance") %>% filter (depthf=="open ocean"), 
                                 aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
lith.abundance30.open <- ggplot_build(lith.abundance30.open)$data[[1]] %>% select (x, y)
lith.abundance30.open$entity <- "lith"
lith.abundance30.open$step <- "abundance"
lith.abundance30.open$depthf <- "open ocean"

lith.enccomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>%  filter (step=="enccomb") %>% 
                                 filter (depthf=="open ocean"),  
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.enccomb30.open <- ggplot_build(lith.enccomb30.open)$data[[1]] %>% select (x,y)
lith.enccomb30.open$entity <- "lith"
lith.enccomb30.open$step <- "enccomb"
lith.enccomb30.open$depthf <- "open ocean"

lith.adscomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith")  %>% filter (step=="adscomb") %>% filter (depthf=="open ocean"), 
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.adscomb30.open <- ggplot_build(lith.adscomb30.open)$data[[1]] %>% select (x, y)
lith.adscomb30.open$entity <- "lith"
lith.adscomb30.open$step <- "adscomb"
lith.adscomb30.open$depthf <- "open ocean"

lith_30m_sep <- rbind (lith.abundance30.shelf, lith.enccomb30.shelf, lith.adscomb30.shelf, 
                       lith.abundance30.open, lith.enccomb30.open, lith.adscomb30.open)

lith_30m_sep$step <-  reorder.factor (lith_30m_sep$step, new.order = c("abundance", 'enccomb', "adscomb")) 
lith_30m_sep$depthf <-  reorder.factor (lith_30m_sep$depthf, new.order = c("shelf/slope", "open ocean")) 

lith30_sep.png <- ggplot (data=lith_30m_sep, aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "lith30_sep.png", width = 13, height = 5, dpi = 600, device="png", units="in")

