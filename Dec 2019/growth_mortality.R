
#for calculating growth rates and mortality rates
growth_mortality <- read.csv("D:/Postdoc/theoretical/CJ data/growth_mortality.csv")

growth_mortality$mortality.rate <-  as.numeric(as.character(growth_mortality$mortality.rate))

source("inspack.R")

gm_sum <- summarySE (growth_mortality, measurevar = "cell.concentration", groupvars = c("experiment", "type", "time"))

ggplot(data=growth_mortality, aes(x=time/24, y=log10(cell.concentration), color=type, shape=type)) + geom_point(size=4) + geom_smooth() + facet_grid(~experiment)                                                

growth_mortality$growth.rate.r_day <- growth_mortality$growth.rate.r * 24
growth_mortality$mortality.rate_day <- growth_mortality$mortality.rate * 24

growthrate.sum <- summarySE (growth_mortality , measurevar = "growth.rate.r_day", groupvars = c("experiment", "type", "time"), na.rm=TRUE)

growthrate.sum.noexp <- summarySE (growth_mortality, measurevar = "growth.rate.r_day", groupvars = c("type", "time"), na.rm=TRUE)

mortrate.sum <- summarySE (growth_mortality, measurevar = "mortality.rate_day", groupvars = c("experiment", "type", "time"), na.rm=TRUE)

mortrate.sum.noexp <- summarySE (growth_mortality, measurevar = "mortality.rate_day", groupvars = c("type", "time"), na.rm=TRUE)

mortrate.sum_stat <- summarySE (growth_mortality %>% filter (experiment %in% c( "374 calcified and naked", "374 growth and adsorption" )), measurevar = "mortality.rate_day", groupvars = c("experiment", "type", "time"), na.rm=TRUE)

growthrate.sum_stat <- summarySE (growth_mortality %>% filter (experiment %in% c( "374 calcified and naked", "374 growth and adsorption" )), measurevar = "growth.rate.r_day", groupvars = c("experiment", "type", "time"), na.rm=TRUE)
