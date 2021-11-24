##run in annotate
##vardi data

vardidata <- read.csv("D:/Users/karengb/CSV Files/vardi.csv")
source("inspack.R")

#get data frame
all <- read.csv("E:/Users/karengb/Postdoc-R/Exported Tables/beta_master8_allbetas.csv", sep=";")

#merge 3 disrates (10e-3, 10e-5, 10e-8)
calm <- all %>% filter (disrate %in% c ("1e-08")) %>% filter (group=="Cc") %>% select (beta_BM, beta_DS, disrate, beta_turb, beta_all)
calm$watcon <- "calm"

stormy <- all %>% filter (disrate %in% c ("0.001")) %>% filter (group=="Cc") %>% select (beta_BM, beta_DS, disrate, beta_turb, beta_all)
stormy$watcon <- "stormy"

moderate <- all %>% filter (disrate %in% c ("1e-05")) %>% filter (group=="Cc") %>% select (beta_BM, beta_DS, disrate, beta_turb, beta_all)
moderate$watcon <- "moderate"

calmstormy <- rbind (calm, stormy, moderate)

probs <- as.data.frame(list (watcon=c("calm", "moderate", "stormy"), virus = rep(c("high", "low"), 1, each=3), 
                             propvir= rep(c(0.33, 0.67), 1, each=3), 
                             ads= rep(c(0.63, 0.63), 1, each=3),  inf = rep(c(0.3, 0.06), 1, each=3)))

calmstormy.exp2 <- left_join (probs, calmstormy)

calmstormy.exp <- calmstormy.exp2[rep(seq_len(nrow(calmstormy.exp2)), 185), ]

vardi <- vardidata[rep(seq_len(nrow(vardidata)), 6), ]

vardisim <- cbind (vardi, calmstormy.exp)

#run simulation

#calculate encounters
vardisim$enc <- vardisim$beta_all*vardisim$virnum*vardisim$propvir 

#adsorption
vardisim$adstot <- vardisim$enc*vardisim$ads

#sucinf
vardisim$sucinf <- vardisim$enc*vardisim$ads*vardisim$inf

#get infection vs host
vardisim_inf <- reshape(vardisim %>% select (Days, Bag, virus, hostnum, watcon, sucinf, virnum), idvar=c("Days", "Bag", "virus", "hostnum", "virnum"), timevar="watcon", direction="wide")
vardisim_inf$parameter= "infections"

##combine high and low
vardi_high <- vardisim_inf %>% filter (virus=="high") 
vardi_low <- vardisim_inf %>% filter (virus=="low") 
vardi_low <- arrange(vardi_low, Days)
vardi_high <- arrange(vardi_high, Days)
vardi_comb <- vardi_high %>% select (c(Days, Bag, virus, hostnum, virnum))

vardi_comb$calm <- vardi_high$sucinf.calm + vardi_low$sucinf.calm
vardi_comb$moderate <- vardi_high$sucinf.moderate + vardi_low$sucinf.moderate
vardi_comb$stormy <- vardi_high$sucinf.stormy + vardi_low$sucinf.stormy

#melt data
vardi_comb.melt <- reshape2::melt(vardi_comb %>% select (c(Days, Bag, hostnum, virnum, calm, moderate, stormy)), 
                                    id.vars=c("Days", "Bag", "hostnum", "virnum"))

vardi_comb.melt.sum <- summarySE(data=vardi_comb.melt, measurevar = "value", group=c("Days", "variable"))


resize.win(8, 5)
ggplot(vardi_comb.melt %>% filter (Days %in% c("0", "5", "10")), aes (x=as.factor(Days), y=log10(value*100), group=Days, color=variable)) + geom_point(size=5, position=position_jitterdodge()) +
  theme_Publication2() +  scale_color_manual (values=c('#2b57a7', '#ffc4b4', '#b11346')) +
  labs (x="Days of incubation", y= expression ("% population infected "~d^-1)) + 
  theme (legend.title = element_blank(), legend.key.width = unit (1, "cm"), legend.position = "bottom",
         legend.direction = "horizontal") +   
  geom_hline(yintercept = log10(10), linetype="dashed") #+
  #scale_y_continuous(breaks = c(2, 1, 0, -1, -2, -3, -4),label = c(100, 10, 1, 0.1, 0.01, 0.001, 0.0001)) 

