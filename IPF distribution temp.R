ipf = read.csv("ipf.csv")
ipfmap = read.csv("ipf_for_map.csv")

require(tidyverse)
require(raster)
require(ggsci)

indiatif = brick("IndiaDEM-Colour.tif")
indiatif = as.data.frame(indiatif, xy = TRUE)
indiatif$IndiaDEM.Colour.1 = indiatif$IndiaDEM.Colour.1/255
indiatif$IndiaDEM.Colour.2 = indiatif$IndiaDEM.Colour.2/255
indiatif$IndiaDEM.Colour.3 = indiatif$IndiaDEM.Colour.3/255
names(indiatif)[3:5] = c("r","g","b")
indiatif$codes = rgb(indiatif$r,indiatif$g,indiatif$b)
indiatif = indiatif %>% mutate(codes = replace(codes, codes == "#000000", NA))

ggp = ggplot() +
  #geom_polygon(data = filtercountry, aes(x=long, y=lat, group=group), 
  #                                 colour = "black", fill = "grey")+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_raster(data = indiatif , aes(x = x, y = y, fill = codes),
                                    alpha = 0.4) +
  
  geom_point(data = ipfmap, aes(x = LONGITUDE, y = LATITUDE, col = morph), size = 1, alpha = 1)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.position = "none")+
  coord_quickmap()


print(ggp)
ggsave(file="IPF_distribution_summer", units="in", width=10, height=7, bg = "transparent")
dev.off()