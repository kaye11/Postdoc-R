a <- infcomb_1m %>% filter (depthf=="open ocean")
b <- infcomb_1m %>% filter (depthf=="shelf/slope")

xx <- a$x  
yy <- a$y  

f <- approxfun(xx, yy)
C <- integrate(f, min(xx), max(xx))$value

p.unscaled <- integrate(f, -1, max(xx))$value #-1 is 10%
p.unscaled / C*100

p.unscaled <- integrate(f, min(xx), -0.9999999)$value
p.unscaled / C*100


#liths
c <- lith.adscomb1.open
d <- lith.adscomb1.shelf

xx <- c$x  
yy <- c$y  

f <- approxfun(xx, yy)
C <- integrate(f, min(xx), max(xx))$value

p.unscaled <- integrate(f, 0, max(xx))$value #1 is 100%
p.unscaled / C*100
