##for ben K. 

disrate <- rep_len (c (1 %o% 10^(seq(-8,-2, 0.5))), length.out=26)
calc <- rep_len(c("calcified"), length.out=13)
naked <- rep_len(c("naked"), length.out=13)
group <- c(calc, naked)
hostnum <- rep_len (c (1 %o% 10^(seq(0,6, 0.5))), length.out=26)
virnum <- hostnum*10
turb <- as.data.frame(cbind(disrate, group))

v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

turb$rad <- case_when(
  turb$group =="naked" ~ 1.8E-6,
  turb$group =="calcified" ~ 2.3E-6,
  TRUE ~ as.numeric(turb$group)
)
#turb <- mutate(turb, rad = ifelse(group == "naked" ,  1.8E-6,  2.3E-6)) #in m

turb$disrate <- as.numeric(as.character(turb$disrate))

turb$beta_d <- (4.2*pi*((turb$disrate/(v*100^2))^0.5)*(((turb$rad+Rehv)*100)^3))*86400 

#check encounters

#use TK, in cm3 s
turb$E_turb_HV <- (turb$beta_d*hostnum*virnum) #E calculated with Virus and Host (10:1 MOI)
turb$E_turb_V <- (turb$beta_d*virnum) #E calculated with virus only
turb$E_turb_H <- (turb$beta_d*hostnum)

source("theme_Publication.R")
ggplot(data = turb, aes(x = disrate, y = beta_d, color=group)) + geom_point(size =5) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  theme_Publication() +
  labs(y = expression(beta~("predicted encounters " ~cm^3~day^-1)), 
       x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank())


library(scales)
