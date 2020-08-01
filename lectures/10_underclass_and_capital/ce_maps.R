library(tidyverse)
library(sf)
library(ggtext)
library(extrafont)
ttf_import("./ignore/fonts")
loadfonts(device = "win")

load("C:/Users/cclan/OneDrive/Ross Projects/Sampson and Raudenbush Replication/data/derived/SNCS_all_WEISB.RData")
st_erase <- function(x, y) {
  
  st_difference(x, st_make_valid(st_union(st_combine(y))))
}
kc_water <- tigris::area_water("WA", "King", class = "sf") %>% st_transform(3689)
sncs_nowater <- SNCS_all_WEISB %>% 
  st_transform(3689) %>%
  st_erase(kc_water)

ce_crime_seattle <- sncs_nowater %>% 
  select(`Collective Efficacy` = EB_INF_nc_2000, 
         `Violent Crime\n(Reverse Coded)` = Violent_Crime_Rate_0305,
         geometry) %>%
  mutate(`Violent Crime\n(Reverse Coded)` = -log(`Violent Crime\n(Reverse Coded)`)) %>%
  mutate(across(-geometry, ~scale(.))) %>%
  gather("measure", "value", -geometry) %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "white", lwd=NA) + 
  facet_wrap(~measure) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  theme(legend.position = "none", 
        strip.background = element_blank(), 
        strip.text = element_text(size = 26, family = "Quattrocento", color = "#342c5c"))
ggsave("./lectures/10_underclass_and_capital/img/ce_crime_seattle.png", plot = ce_crime_seattle, units = "in")

ce_mailed_seattle <- sncs_nowater %>% 
  select(`Collective Efficacy` = EB_INF_nc_2000, 
         `Mailed Letters` = Any_Mailed_2000,
         geometry) %>%
  mutate(across(-geometry, ~scale(.))) %>%
  gather("measure", "value", -geometry) %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "white", lwd=NA) + 
  facet_wrap(~measure) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  theme(legend.position = "none", 
        strip.background = element_blank(), 
        strip.text = element_text(size = 26, family = "Quattrocento", color = "#342c5c"))
ggsave("./lectures/10_underclass_and_capital/img/ce_mailed_seattle.png", plot = ce_mailed_seattle, units = "in")
