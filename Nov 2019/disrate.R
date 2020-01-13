#make the dissipation rate and wind speed script

source("inspack.R")

turb <- expand.grid(list (depth = (seq(-60, 0, 0.5)), wind = (seq(5, 35, 5))))

#constants
K= 0.4 #von Karman's constant
Cd <- 1.15*(10)^-3 #drag coefficient
Den_air <-  (1.225)*10^-3 #g/m3 at sea level and 15C
Den_CH2O= 1.025 #g/m3 density seawater at 18C #these two will even out so no need to convert

turb$U <- sqrt(Cd*((turb$wind)^2)*((Den_air/Den_CH2O)))
turb$disrate <- (turb$U^3)/(K*(abs(turb$depth)))
#turb$U2 <- sqrt(turb$wind/(Den_CH2O*1000000))
#turb$E2 <- (turb$U2^3)/(K*(abs(turb$depth)))

resize.win (6,6)
ggplot(turb, aes(x=disrate, y=depth, color=as.factor(wind))) + geom_line(size=1) + labs (x=expression("dissipation rate"~ (m^2~s^-3)), y="depth (m)") + 
  scale_x_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x, n=6),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="b") +
  theme_Publication2() + theme (legend.direction = "vertical", legend.position = c(0.80, 0.20), legend.key.width = unit(0.7, "cm")) + labs(color=expression("wind speed"~(m~s^-1))) + geom_hline(yintercept = (-30), linetype="dotted")
                                                                                                                                                                                 