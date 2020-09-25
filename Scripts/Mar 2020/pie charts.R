source ("inspack.R")

library(readr)
sum_host <- read_csv("Postdoc-R/CSV Files/sum host.csv")

###FIELD
##encounters
ggplot(sum_host %>% filter (condition=="field" & watcon=="calm"), aes(x = "", y = encprop_h, fill = enc_h)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#808000", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(virus~group2)  #+ geom_text (aes(label=encprop_h))

ggplot(sum_host %>% filter (condition=="field" & watcon=="stormy"), aes(x = "", y = encprop_h, fill = enc_h)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#808000", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(virus~group2)  

#adsorption
ggplot(sum_host %>% filter (condition=="field" & watcon=="calm"), aes(x = "", y = adsprop_h, fill = ads_h)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#008080", "#808000")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(virus~group2) + geom_text (aes(label=adsprop_h))

#infection
ggplot(sum_host %>% filter (condition=="field" & watcon=="calm"), aes(x = "", y = infprop_h, fill = inf_h)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#800080", "#008080")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(virus~group2) + geom_text (aes(label=infprop_h))

#infection, alles
ggplot(sum_host %>% filter (condition=="field" & watcon=="stormy"), aes(x = "", y = infprop_all_h, fill = inf_h_all)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#800080", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(virus~group2) #+ geom_text (aes(label=infprop_all_h))

#entitieswith adsorbed EhVs
ggplot(sum_host %>% filter (condition=="field" & watcon=="stormy"), aes(x = "", y = adsprop_h_allentity, fill = ads_h)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#FFFFFF", "#008080")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(virus~group2) #+ geom_text (aes(label=adsprop_h_allentity))
  q#adsorbed all

