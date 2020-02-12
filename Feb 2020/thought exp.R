#thought experiment
#maked data frame for sucinf
sucinf <- as.data.frame (seq (1, 10000, 1))
sucinf <- plyr::rename (sucinf, c ("seq(1, 10000, 1)" = "sucinf"))

BM_select <- BM %>% select (group2, beta_BM)
DS_select <- DS_pred %>% select (group2, beta_DS)
betas <- merge (BM_select, DS_select)

#this expands data frames
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

tex <- expand.grid.df(probs, sucinf)

texbetas <- merge(tex, betas)

texbetas$beta_all <- (texbetas$sucinf)/(texbetas$hostnum*texbetas$virnum*texbetas$propvir*texbetas$bioinf)

texbetas$beta_turb <- abs(texbetas$beta_all-texbetas$beta_BM-texbetas$beta_DS)

turb_constant <- BM %>% select (group2, rad)
turb_constant$constant <- (4.2*pi*((1/(v*100^2))^0.5)*(((turb_constant$rad+Rehv)*100)^3))*86400 

#add constant
texbetascons <- merge (texbetas, turb_constant)

texbetascons$disrate <- (texbetascons$beta_turb/texbetascons$constant)^2

#check calc
texbetascons$beta_turb_check <- (4.2*pi*((texbetascons$disrate/(v*100^2))^0.5)*(((texbetascons$rad+Rehv)*100)^3))*86400 
#it is the same!!!

#drop levels
texbetascons.drop <- texbetascons %>% filter(!(group2 %in% c("Li-Hc", "Li-Mc")))

#encounters
texbetascons.drop$E <- texbetascons.drop$virnum*texbetascons.drop$propvir*texbetascons.drop$hostnum*texbetascons.drop$beta_all

ggplot(texbetascons.drop, aes(x=log10(disrate), y=log10(E), color=group2)) + geom_point(position = position_dodge()) + facet_grid(virus~condition)
