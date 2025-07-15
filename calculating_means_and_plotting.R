library(tidyverse)
library(ggthemes)
library(Hmisc)
theme_set(theme_tufte())

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

ipf = read.csv("ipf_for_map.csv")
ipf$quart = ipf$month
ipf = ipf %>%
  mutate(quart = replace(quart, quart %in% c(6,7,8), "Summer (Breeding)")) %>%
  mutate(quart = replace(quart, quart %in% c(9,10,11), "Autumn")) %>%
  mutate(quart = replace(quart, quart %in% c(12,1,2), "Winter")) %>%
  mutate(quart = replace(quart, quart %in% c(3,4,5), "Spring"))

ipf$region = factor(ipf$region, levels = c("Sri Lanka","South","Central","North","East","Himalayas"))
ipf$quart = factor(ipf$quart, levels = c("Summer (Breeding)","Autumn","Winter","Spring"))

ipfall = ipf

ipf1 = ipfall %>% filter(morph == "I")
ipf1 = ipf1 %>% group_by(region,quart) %>% dplyr::summarize(toti = n()) %>% ungroup
ipf2 = ipfall %>% filter(morph == "R")
ipf2 = ipf2 %>% group_by(region,quart) %>% dplyr::summarize(totr = n()) %>% ungroup
ipf3 = ipfall %>% filter(morph == "W")
ipf3 = ipf3 %>% group_by(region,quart) %>% dplyr::summarize(totw = n()) %>% ungroup

ipf = ipfall %>% group_by(region,quart) %>% dplyr::summarize(tot = n()) %>% ungroup

ipf1 = left_join(ipf,ipf1)
ipf1$freq = ipf1$toti/ipf1$tot
ipf1$toti[is.na(ipf1$toti)] = 0
ipf1$cil = binconf(x=ipf1$toti,n=ipf1$tot)[,2]
ipf1$cir = binconf(x=ipf1$toti,n=ipf1$tot)[,3]
ipf1 = ipf1 %>% dplyr::select(region,quart,freq,cil,cir,tot)

ipf2 = left_join(ipf,ipf2)
ipf2$freq = ipf2$totr/ipf2$tot
ipf2$totr[is.na(ipf2$totr)] = 0
ipf2$cil = binconf(x=ipf2$totr,n=ipf2$tot)[,2]
ipf2$cir = binconf(x=ipf2$totr,n=ipf2$tot)[,3]
ipf2 = ipf2 %>% dplyr::select(region,quart,freq,cil,cir,tot)

ipf3 = left_join(ipf,ipf3)
ipf3$freq = ipf3$totw/ipf3$tot
ipf3$totw[is.na(ipf3$totw)] = 0
ipf3$cil = binconf(x=ipf3$totw,n=ipf3$tot)[,2]
ipf3$cir = binconf(x=ipf3$totw,n=ipf3$tot)[,3]
ipf3 = ipf3 %>% dplyr::select(region,quart,freq,cil,cir,tot)

ipf1$morph = "Intermediate"
ipf2$morph = "Rufous"
ipf3$morph = "White"

ipf = rbind(ipf1,ipf2,ipf3)
ipf$freq[is.na(ipf$freq)] = 0
ipf$freq[ipf$tot<=10] = NA
ipf$cil[ipf$tot<=10] = NA
ipf$cir[ipf$tot<=10] = NA

pd = position_dodge(0.4)

ggp = ggplot(data = ipf[ipf$morph != "Intermediate",], aes(x = region, y = freq, col = morph)) +
  facet_wrap(. ~ quart,scale="free", nrow = 2, ncol = 2) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.2, position = pd) +
  scale_colour_manual(breaks = c("Rufous","White"),
                      values = c("#a63e27","white"),
                      labels = c("Rufous morph","White morph")) +
  xlab("geographic region") +
  ylab("proportion of individuals of each morph")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(panel.background = element_rect(fill = "#d0d9a4", colour = NA)) +
  theme(plot.background = element_rect(fill = "#d0d9a4", colour = NA)) +
  scale_y_continuous(breaks = c(0,0.25,0.50,0.75,1), labels = c(0,0.25,0.50,0.75,1), limits = c(0,1)) +
  theme(legend.position = "bottom")

#theme(panel.background = element_rect(fill = "#e0eaae", colour = NA)) +
#theme(plot.background = element_rect(fill = "#e0eaae", colour = NA)) +


png('Fig. 3.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = ipf[ipf$morph == "Intermediate",], aes(x = region, y = freq)) +
  facet_wrap(. ~ quart,scale="free", nrow = 2, ncol = 2) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.2, colour = "white") +
  geom_point(size = 4, colour = "#a63e27") +
  xlab("geographic region") +
  ylab("proportion of individuals of intermediate morph")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(panel.background = element_rect(fill = "#d0d9a4", colour = NA)) +
  theme(plot.background = element_rect(fill = "#d0d9a4", colour = NA)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4), labels = c(0,0.1,0.2,0.3,0.4), limits = c(0,0.4)) +
  theme(legend.position = "bottom")

#theme(panel.background = element_rect(fill = "#e0eaae", colour = NA)) +
#theme(plot.background = element_rect(fill = "#e0eaae", colour = NA)) +


png('Fig. 4a.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


#####################################################################

ipf = read.csv("ipf_for_map.csv")
ipf$quart = ipf$month
ipf = ipf %>%
  mutate(quart = replace(quart, quart %in% c(6,7,8), "Summer (Breeding)")) %>%
  mutate(quart = replace(quart, quart %in% c(9,10,11), "Autumn")) %>%
  mutate(quart = replace(quart, quart %in% c(12,1,2), "Winter")) %>%
  mutate(quart = replace(quart, quart %in% c(3,4,5), "Spring"))

ipf$region = factor(ipf$region, levels = c("Sri Lanka","South","Central","North","East","Himalayas"))
ipf$quart = factor(ipf$quart, levels = c("Summer (Breeding)","Autumn","Winter","Spring"))

ipfall = ipf

ipf1 = ipfall %>% filter(morph == "I")
ipf1 = ipf1 %>% group_by(quart) %>% dplyr::summarize(toti = n()) %>% ungroup
ipf = ipfall %>% group_by(quart) %>% dplyr::summarize(tot = n()) %>% ungroup

ipf1 = left_join(ipf,ipf1)
ipf1$freq = ipf1$toti/ipf1$tot
ipf1$toti[is.na(ipf1$toti)] = 0
ipf1$cil = binconf(x=ipf1$toti,n=ipf1$tot)[,2]
ipf1$cir = binconf(x=ipf1$toti,n=ipf1$tot)[,3]
ipf1 = ipf1 %>% dplyr::select(quart,freq,cil,cir,tot)


ipf = ipf1
ipf$freq[is.na(ipf$freq)] = 0
ipf$freq[ipf$tot<=10] = NA
ipf$cil[ipf$tot<=10] = NA
ipf$cir[ipf$tot<=10] = NA


ggp = ggplot(data = ipf, aes(x = quart, y = freq)) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.2, colour = "white") +
  geom_point(size = 4, colour = "#a63e27") +
  xlab("geographic region") +
  ylab("proportion of individuals of intermediate morph")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(panel.background = element_rect(fill = "#d0d9a4", colour = NA)) +
  theme(plot.background = element_rect(fill = "#d0d9a4", colour = NA)) +
  scale_y_continuous(breaks = c(0,0.05,0.1), labels = c(0,0.05,0.1), limits = c(0,0.1)) +
  theme(legend.position = "bottom")

#theme(panel.background = element_rect(fill = "#e0eaae", colour = NA)) +
#theme(plot.background = element_rect(fill = "#e0eaae", colour = NA)) +


png('Fig. 4.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()
