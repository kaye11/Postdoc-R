##load latest beta master data file

source("inspack.R")

#thought experiment
#make data frame for sucinf 10% of pop is 3000
sucinf <- as.data.frame (list(sucinf = seq (1, 3000, 1)))
sucads <- as.data.frame (list(sucads = seq (1, 3000, 1)))

BM_select <- BM %>% select (group, group2, beta_BM) 
DS_select <- DS_pred %>% select (group, group2, beta_DS) 
betas <- merge (BM_select, DS_select)

#this expands data frames
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

#for sucinf
tex <- expand.grid.df(probs, sucinf)

texbetas <- merge(tex, betas)

texbetas$beta_all <- (texbetas$sucinf)/(texbetas$hostnum*texbetas$virnum*texbetas$propvir*texbetas$ads*texbetas$inf)
#can also remove hostnum for just entities

texbetas$beta_turb <- texbetas$beta_all-texbetas$beta_BM-texbetas$beta_DS

turb_constant <- BM %>% select (group2, rad)
turb_constant$constant <- (4.2*pi*((1/(v*100^2))^0.5)*(((turb_constant$rad+Rehv)*100)^3))*86400 

#add constant
texbetascons <- merge (texbetas, turb_constant)

texbetascons$disrate <- (texbetascons$beta_turb/texbetascons$constant)^2

#check calc
texbetascons$beta_turb_check <- (4.2*pi*((texbetascons$disrate/(v*100^2))^0.5)*(((texbetascons$rad+Rehv)*100)^3))*86400 

#it is the same!!!

texbetascons$group <- reorder.factor (texbetascons$group, new.order = c("Nc", "Cc", "Li"))
texbetascons$group2 <- reorder.factor (texbetascons$group2, new.order = c("Nc", "Cc-Mc", "Cc-Oc", "Li")) 

#prelim plot
resize.win(7,6)
ggplot(texbetascons %>% filter(!(group %in% c("Li"))) %>% filter (condition %in% c("open ocean")), aes(x=log10(disrate), y=log10(sucinf), color=group, fill=group)) + geom_point(size=2) + theme_Publication() + theme (legend.title = element_blank(), panel.spacing = unit(2, "lines")) + facet_grid(~virus) + labs (x = expression("dissipation rate "~(m^2~s^-3)), y=expression("log10 successful infection"~day^-1~mL^-1)) +  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +  geom_hline(yintercept = log10(1), linetype="dashed")


###for encounters repeat the same again but use sucads

#drop levels
#texbetascons.drop <- texbetascons %>% filter(!(group2 %in% c("Li-Hc", "Li-Mc")))

#encounters
texbetascons$E <- texbetascons$virnum*texbetascons$propvir*texbetascons$hostnum*texbetascons$beta_all
