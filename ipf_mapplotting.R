require(tidyverse)
library(rgeos)
require(ggthemes)
theme_set(theme_tufte())

require(extrafont)

load("south_asia_map.RData")

ipfmap = read.csv("ipf_for_map.csv")
ipfmap$STATE = as.character(ipfmap$STATE)

datastate = ipfmap %>% 
  group_by(STATE) %>% summarize(samplesize = n())

dataregion = ipfmap %>%
  group_by(Region) %>% summarize(samplesize = n())

dist = ipfmap %>% distinct(STATE,Region)

ffullmap = fortify(fullmap, region = c("ST_NM"))
ffullmapcount = left_join(ffullmap,datastate,by = c('id' = "STATE"))
ffullmap = left_join(ffullmap,dist,by = c('id' = "STATE"))
#ffullmap = ffullmap %>% filter(!is.na(Region))

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 6

cols1 = cols[c(1:ns)]
bks1 = c("Sri Lanka","South","Central","North","East","Himalayas")
lbs1 = c("Sri Lanka","South","Central","North","East","Himalayas")

plotindiamap = ggplot() +
  geom_polygon(data = ffullmap, aes(x=long, y=lat,group=group,fill=Region), colour = NA)+  
  geom_polygon(data = fullmap, aes(x=long, y=lat, group=group), colour = "black", fill = NA)+ 
  geom_polygon(data = fullmapcountries, aes(x=long, y=lat, group=group), colour = "black", fill = NA, size = 1)+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  scale_fill_manual(breaks = bks1, values = cols1, labels = lbs1, na.value = "white") +
  coord_map()

ggp = plotindiamap +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 1))


n1 = "Fig. 1.png"

print(ggp)
ggsave(file=n1, units="in", width=8, height=10)




###################################### plot sample sizes

bks1 = c(3,10,30,100,300,800)

plotindiamap = ggplot() +
  geom_polygon(data = ffullmapcount, aes(x=long, y=lat,group=group,fill=samplesize), colour = "black")+  
  geom_polygon(data = fullmapcountries, aes(x=long, y=lat, group=group), colour = "black", fill = NA, size = 1)+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  scale_fill_gradient(breaks = bks1, trans = "log", na.value = "white") +
  coord_map()

ggp = plotindiamap +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 1))


n1 = "Fig. 2.png"

print(ggp)
ggsave(file=n1, units="in", width=8, height=10)
