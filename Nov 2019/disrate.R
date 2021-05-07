#make the dissipation rate and wind speed script

source("inspack.R")

turb <- expand.grid(list (depth = (seq(0, 50, 0.25)), wind = (seq(10, 50, 10))))

#constants
K= 0.4 #von Karman's constant
Cd <- 1.15*(10)^-3 #drag coefficient
Den_air <-  (1.212)*10^-3 #g/m3 at sea level and 18C #g/m3 at sea level and 15C
Den_CH2O= 1.025 #g/m3 density seawater at 18C #these two will even out so no need to convert

turb$U <- sqrt(Cd*((turb$wind)^2)*((Den_air/Den_CH2O)))
turb$disrate <- (turb$U^3)/(K*(abs(turb$depth)))
#turb$U2 <- sqrt(turb$wind/(Den_CH2O*1000000))
#turb$E2 <- (turb$U2^3)/(K*(abs(turb$depth)))

BlRed <- colorRampPalette((c('#00429d', '#2b57a7', '#426cb0', '#5681b9', '#6997c2', '#7daeca', '#93c4d2', '#abdad9', '#caefdf', '#ffe2ca', '#ffc4b4', '#ffa59e', '#f98689', '#ed6976', '#dd4c65', '#ca2f55', '#b11346', '#93003a')))

resize.win (6,6)
ggplot(turb, aes(x=disrate, y=depth, color=as.factor(wind))) + geom_line(size=1) + labs (x=expression("dissipation rate"~ (m^2~s^-3)), y="depth (m)") + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n=6),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="b") +
  theme_Publication2() + theme (legend.direction = "vertical", legend.position = c(0.82, 0.25), legend.key.width = unit(0.7, "cm")) + labs(color=expression("wind speed"~(m~s^-1))) + geom_hline(yintercept =30, linetype="dotted")

resize.win (8,5)
ggplot(turb, aes(x=log10(disrate), y=depth, color=as.factor(wind))) + geom_line(size=1) + 
  labs (x=expression(log[10]~epsilon~(m^2~s^-3)),  y="depth (m)", 
        color=expression("wind speed"~(m~s^-1))) + 
  scale_x_continuous(breaks = c(seq(-8, -2, 1)), position="top") + 
  scale_y_continuous(trans="reverse") +
  theme_Publication() + theme (legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.7, "cm")) + geom_hline(yintercept = 30, linetype="dotted") + scale_color_manual (values= c ("#00429d",'#5681b9', '#ffc4b4','#ed6976', '#93003a' ))

