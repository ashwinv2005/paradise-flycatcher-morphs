library(tidyverse)
library(ggthemes)
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

ipf = read.csv("ipf.csv")
ipf1 = ipf %>% dplyr::select(region,quart,ruf,binconf.r_lower,binconf.r_upper)
names(ipf1)[3:5] = c("freq","cil","cir")
ipf2 = ipf %>% dplyr::select(region,quart,whi,binconf.w_lower,binconf.w_upper)
names(ipf2)[3:5] = c("freq","cil","cir")
ipf1$morph = "Rufous"
ipf2$morph = "White"
ipf = rbind(ipf1,ipf2)
ipf$quart = as.character(ipf$quart)

ipf = ipf %>%
  mutate(quart = replace(quart, quart == "Jun-Aug", "Summer (Breeding)")) %>%
  mutate(quart = replace(quart, quart == "Sep-Nov", "Autumn")) %>%
  mutate(quart = replace(quart, quart == "Dec-Feb", "Winter")) %>%
  mutate(quart = replace(quart, quart == "Mar-May", "Spring"))

ipf$region = factor(ipf$region, levels = c("Sri Lanka","South","Central","North","East","Himalayas"))
ipf$quart = factor(ipf$quart, levels = c("Summer (Breeding)","Autumn","Winter","Spring"))

pd = position_dodge(0.2)

ggp = ggplot(data = ipf, aes(x = region, y = freq, col = morph)) +
  facet_wrap(. ~ quart, scale="free", nrow = 2, ncol = 2) +
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
  theme(legend.position = "bottom")

#theme(panel.background = element_rect(fill = "#e0eaae", colour = NA)) +
#theme(plot.background = element_rect(fill = "#e0eaae", colour = NA)) +


png('Fig. 3.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

