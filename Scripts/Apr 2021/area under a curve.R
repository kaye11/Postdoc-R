a <- infcomb_30m %>% filter (depthf=="open ocean")
b <- infcomb_30m %>% filter (depthf=="shelf/slope")

xx <- a$x  
yy <- a$y  

f <- approxfun(xx, yy)
C <- integrate(f, min(xx), max(xx))$value

p.unscaled <- integrate(f, -1, max(xx))$value #-1 is 10%
p.unscaled / C*100

p.unscaled <- integrate(f, min(xx), -0.9999999)$value
p.unscaled / C*100

#31m open ocean - 0.25%
#1m shelf/slope - 5.1%

#30m open ocean - 0.18%
#30m shelf/slope -3.66%


#liths
c <- lith.adscomb1.open
d <- lith.adscomb1.shelf

xx <- d$x  
yy <- d$y  

f <- approxfun(xx, yy)
C <- integrate(f, min(xx), max(xx))$value

p.unscaled <- integrate(f, 0, max(xx))$value #0 is 100%
p.unscaled / C*100

#abundance
e <- Cc.shelf
g <- Cc.open

xx <- g$x  
yy <- g$y  

f <- approxfun(xx, yy)
C <- integrate(f, min(xx), max(xx))$value

#p.unscaled <- integrate(f, 2.7, max(xx))$value #change 
p.unscaled <- integrate(f, 3, max(xx))$value #change ##this is filtered by max y of 0.04 up
p.unscaled / C*100

##encounters, 10^0=1=100% of pop
a <- Ehux.adscomb30.open
b <- Ehux.adscomb30.shelf

xx <- b$x  
yy <- b$y  

f <- approxfun(xx, yy)
C <- integrate(f, min(xx), max(xx))$value

p.unscaled <- integrate(f, 0, max(xx))$value 
p.unscaled / C*100

##if from prob density
xx <- e$x  
yy <- sqrt(e$y)+1  
f <- approxfun(xx, yy)
C <- integrate(f, min(yy), max(yy))$value

p.unscaled <- integrate(f, 1.2, max(yy))$value #0.02 max= sqrt(0.02)+1=1.14
p.unscaled / C*100

xx <- g$x  
yy <- sqrt(g$y)+1  
f <- approxfun(xx, yy)
C <- integrate(f, min(yy), max(yy))$value

p.unscaled <- integrate(f, 1.2, max(yy))$value #0.02 max= sqrt(0.02)+1=1.14
p.unscaled / C*100

data$sqrt <- sqrt(data$y)+1
