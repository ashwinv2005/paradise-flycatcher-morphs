library(tidyverse)
library(sf)
library(Hmisc)
library(extrafont)

load("base_map.RData")

map_to_use_sf = map_to_use_sf %>%
  mutate(REGION = case_when(REGION == "East" ~ "North",
                            TRUE ~ REGION))

data = read.csv("data_morphs.csv") %>%
  mutate(REGION = case_when(REGION == "East" ~ "North",
                            TRUE ~ REGION))
data_full = read.csv("data_sampling_eBird.csv") %>%
  mutate(REGION = case_when(REGION == "East" ~ "North",
                            TRUE ~ REGION))
data_ipf = read.csv("data_ipf_eBird.csv") %>%
  mutate(REGION = case_when(REGION == "East" ~ "North",
                            TRUE ~ REGION))

white_total = data %>%
  filter(morph == "W") %>%
  group_by(REGION,month,morph) %>%
  reframe(count = n())

rufous_total = data %>%
  filter(morph == "R") %>%
  group_by(REGION,month,morph) %>%
  reframe(count = n())
  
intermediate_total = data %>%
  filter(morph == "I") %>%
  group_by(month,morph) %>%
  reframe(count = n()) %>%
  mutate(morph = "Intermediate")

ipf_total = data_ipf %>%
  group_by(REGION,month) %>%
  reframe(total_ipf = n())

eBird_total = data_full %>%
  group_by(REGION,month) %>%
  reframe(total = n()) %>%
  left_join(ipf_total) %>%
  mutate(total_ipf = case_when(is.na(total_ipf) ~ 0,
                           TRUE ~ as.numeric(total_ipf))) %>%
  mutate(freq_ipf = total_ipf/total) %>%
  mutate(low = quantile(freq_ipf,0.25)[1],
         high = quantile(freq_ipf,0.75)[1]) %>%
  mutate(freq_ipf = case_when(freq_ipf < mean(low) ~ low,
                              freq_ipf > mean(high) ~ high,
                              TRUE ~ freq_ipf))

white_stats = eBird_total %>%
  left_join(white_total) %>%
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ as.numeric(count))) %>%
  mutate(morph = "White")

rufous_stats = eBird_total %>%
  left_join(rufous_total) %>%
  mutate(count = case_when(is.na(count) ~ 0,
                            TRUE ~ as.numeric(count))) %>%
  mutate(morph = "Rufous")

morph_stats = white_stats %>%
  bind_rows(rufous_stats) %>%
  #mutate(fort_cont = (month*2)-2+fort) %>%
  rowwise() %>%
  mutate(cil = binconf(count, total, 0.05)[2],
         prop = count/total,
         ciu = binconf(count, total, 0.05)[3]) %>%
  mutate(cil = cil/freq_ipf,
         prop = prop/freq_ipf,
         ciu = ciu/freq_ipf) %>%
  group_by(REGION) %>%
  mutate(max_freq = max(ciu)) %>% ungroup() %>%
  mutate(cil = cil/max_freq,
         prop = prop/max_freq,
         ciu = ciu/max_freq) %>%
  mutate(cil = case_when(cil < 0 ~ 0,
                           TRUE ~ as.numeric(cil)))
  
bks = c("Himalaya","North","Central","South","Sri Lanka")
lbs = c("Himalaya","North\nIndia","Central\nIndia",
        "South\nIndia","Sri Lanka")

morph_stats$REGION = factor(morph_stats$REGION,
                            levels = bks)

ggp = ggplot(data = morph_stats %>% filter(!REGION %in% c("East"))) +
  facet_wrap(~ REGION,
             scales = "free_y",
             ncol = 1,
             strip.position = "right",
             labeller = labeller(REGION = c(North="North\nIndia",
                                            Central = "Central\nIndia",
                                            South="South\nIndia"))) +
  geom_line(aes(x = month, y = prop, col = morph), linewidth = 2) +
  geom_ribbon(aes(x = month, ymin = cil, ymax = ciu, col = morph, 
                  fill = morph), alpha = 0.2, linetype = 0) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  ylab("Standardized colour morph frequency")


plot_morphs = ggp +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 16), 
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(angle = 90, size = 22), 
    axis.text.y = element_text(size = 16)
    ) +
  theme(text=element_text(family="Gill Sans", face = 'bold')) +
  theme(
    strip.text.y = element_text(size = 15, face = 'bold.italic'),
    strip.background = element_rect(fill = "#d0d9a4", colour = NA)
  ) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(1,12),
    breaks = seq(1,12),
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov","Dec")
    ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#d0d9a4", colour = NA),
    plot.background = element_rect(fill = "#d0d9a4", colour = NA)
    ) +
  scale_y_continuous(expand = c(0,0), breaks = c(0), labels = c(0), limits = c(0,1)) +
  scale_colour_manual(breaks = c("White","Rufous"), values = c("white","#a63e27")) +
  scale_fill_manual(breaks = c("White","Rufous"), values = c("white","#a63e27")) +
  theme(
    legend.background = element_rect(fill = "#d0d9a4", colour = NA),
    legend.key = element_rect(colour = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
    )

n = "plot_morphs.png"

print(plot_morphs)
ggsave(file=n, units="in", width=7, height=10)

# plot intermediate

morph_total = data %>%
  group_by(month) %>%
  reframe(total = n())

intermediate_stats = morph_total %>%
  left_join(intermediate_total) %>%
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ as.numeric(count))) %>%
  mutate(morph = "Intermediate")

morph_stats_int = intermediate_stats %>%
  rowwise() %>%
  mutate(cil = binconf(count, total, 0.05)[2],
         prop = count/total,
         ciu = binconf(count, total, 0.05)[3]) %>%
  mutate(cil = case_when(cil < 0 ~ 0,
                         TRUE ~ as.numeric(cil)))

ggp = ggplot(data = morph_stats_int) +
  geom_line(aes(x = month, y = prop), linewidth = 2) +
  geom_ribbon(aes(x = month, ymin = cil, ymax = ciu),
              alpha = 0.2, linetype = 0) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  ylab("Relative frequency of intermediate morph")


plot_intermediate = ggp +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 12), 
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(angle = 90, size = 16), 
    axis.text.y = element_text(size = 12)
  ) +
  theme(text=element_text(family="Gill Sans", face = 'bold')) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(0.9,12.3),
    breaks = seq(1,12),
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.02))

n = "plot_intermediate.png"

print(plot_intermediate)
ggsave(file=n, units="in", width=8, height=6)

# ratings

bootst = function(vec) {
  mn = numeric(1000)
  
  for (i in 1:1000)
  {
    temp = sample(vec, replace = T)
    mn[i] = mean(temp)
  }
  
  lci = quantile(mn,0.025)
  med = median(mn)
  rci = quantile(mn,0.975)
  
  res = c(lci,med,rci)
  
  return(res)
}

ratings = data %>%
  filter(!is.na(rating)) %>%
  group_by(month) %>%
  reframe(
    n = n(),
    cil = bootst(rating)[1],
    mean = bootst(rating)[2],
    ciu = bootst(rating)[3],
    max_rat = max(rating)) %>%
  mutate(cil = case_when(n < 10 ~ NA,
                         TRUE ~ as.numeric(cil)),
         mean = case_when(n < 10 ~ NA,
                         TRUE ~ as.numeric(mean)),
         ciu = case_when(n < 10 ~ NA,
                         TRUE ~ as.numeric(ciu)))

ggp = ggplot(data = ratings) +
  geom_point(aes(x = month, y = mean), size = 2) +
  geom_errorbar(aes(x = month, ymin = cil, ymax = ciu), width = 0.2) +
  ylab("Mean extent of white on rufous (1-10)")

plot_rating = ggp +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 12), 
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(angle = 90, size = 16), 
    axis.text.y = element_text(size = 12)
  ) +
  theme(text=element_text(family="Gill Sans", face = 'bold')) +
  scale_x_continuous(
    expand = c(0.02,0),
    limits = c(0.9,12.1),
    breaks = seq(1,12),
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  scale_y_continuous(expand = c(0,0.1), limits = c(0,10), breaks = seq(0,10,1))

n = "plot_rating.png"

print(plot_rating)
ggsave(file=n, units="in", width=8, height=6)

## plot maps

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 5

cols1 = cols[c(1:ns)]

regionmap = ggplot() +
  geom_sf(data = map_to_use_sf, aes(fill=REGION), colour = "black")+  
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans")) +
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
  theme(legend.title = element_blank(), legend.text = element_text(size = 16),
        legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(breaks = bks, values = cols1, labels = lbs, na.value = "white") +
  coord_sf()

n = "regionmap.png"

print(regionmap)
ggsave(file=n, units="in", width=8, height=10)

map_to_use_sf_breeding = map_to_use_sf %>%
  filter(!is.na(REGION)) %>%
  mutate(breeding = case_when(REGION %in% c("South","Sri Lanka") ~ "Rufous",
                            TRUE ~ NA)) %>%
  mutate(breeding = case_when(REGION %in% c("Himalaya") ~ "White",
                              TRUE ~ breeding)) %>%
  mutate(breeding = case_when(!REGION %in% c("Himalaya","South","Sri Lanka") ~ "Both",
                              TRUE ~ breeding)) %>%
  mutate(breeding = factor(breeding, levels = c("White","Both","Rufous")))

breedingmap = ggplot() +
  geom_sf(data = map_to_use_sf_breeding, aes(fill=breeding), colour = "black") +  
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans")) +
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
        panel.background = element_rect(fill = "#d0d9a4", colour = NA),
        plot.background = element_rect(fill = "#d0d9a4", colour = NA))+
  scale_fill_manual(values = c("white","#D29F93","#a63e27"), na.value = "white") +
  theme(
    legend.background = element_rect(fill = "#d0d9a4", colour = NA),
    legend.key = element_rect(colour = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 30),
    legend.position = "bottom"
  ) +
  coord_sf()

n = "breedingmap.png"

print(breedingmap)
ggsave(file=n, units="in", width=7, height=10)