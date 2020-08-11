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

glimpse(SNCS_indiv)

full_data <- haven::read_spss("C:/Users/cclan/OneDrive/Ross Projects/Sampson and Raudenbush Replication/data/raw/full data plus resids.sav")

sncs_codes <- full_data %>%
  select(`Percent Black` = pblack,
         `Violent Crime Rate` = lvcr0204,
         `Police Inefficacy` = pig_ebir,
         `Codes of Violence` = code_ebir,
         tract) %>%
  mutate(TRACT_2000 = str_pad(tract, 6, "left", "0")) %>%
  left_join(SNCS_all_WEISB %>% select(TRACT_2000, geometry), by = "TRACT_2000") %>%
  st_as_sf()


kc_water <- tigris::area_water("WA", "King", class = "sf") %>% st_transform(3689)
sncs_nowater <- sncs_codes %>% 
  st_transform(3689) %>%
  st_erase(kc_water)

sncs_nowater %>% 
  st_drop_geometry() %>%
  select(`Percent\nBlack`=`Percent Black`,
         `Codes of\nViolence`=`Codes of Violence`,
         `Violent\nCrime Rate`=`Violent Crime Rate`,
         `Police\nInefficacy`=`Police Inefficacy`) %>%
  cor(., use = "pairwise.complete")
  
codes_crime_seattle <- sncs_nowater %>% 
  select(`Percent\nBlack`=`Percent Black`,
         `Codes of\nViolence`=`Codes of Violence`,
         `Violent\nCrime Rate`=`Violent Crime Rate`,
         `Police\nInefficacy`=`Police Inefficacy`,
         geometry) %>%
  mutate(across(-geometry, ~scale(.))) %>%
  gather("measure", "value", -geometry) %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "white", lwd=NA) + 
  facet_wrap(~measure, ncol=4) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  theme(legend.position = "none", 
        strip.background = element_blank(), 
        strip.text = element_text(size = 26, family = "Quattrocento", color = "#342c5c"))

ggsave("./lectures/12_da_cots/img/codes_crime_seattle.png", plot = codes_crime_seattle, units = "in")
