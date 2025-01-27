library(auk)
library(ggplot2)
library(dplyr)
library(sf)
library(stringr)
df <- read.csv("MyEBirdData.csv")
states <- ebird_states %>%
  filter(country %in% c("China", "Taiwan"))

df.se <- df %>%
  filter(State.Province %in% states$state_code) %>%
  group_by(State.Province) %>%
  summarise(species=n_distinct(Common.Name)) %>%
  mutate(state_code=State.Province)

df.se <- left_join(df.se, states) %>%
  mutate(PINYIN_NAM=state) %>%
  select(PINYIN_NAM, species) %>%
  mutate(PINYIN_NAM = if_else(PINYIN_NAM == "Nei Mongol", "Neimenggu", PINYIN_NAM))

map <- st_read("shapefile/省级行政区.shp")
map <- st_drop_geometry(map) %>%
  select(PINYIN_NAM, NAME)
map.se <- left_join(map, df.se) %>%
  mutate(CNAME=NAME)

maptest <- st_read("shapefile/china_map/中国省级地图GS（2019）1719号.geojson")
maptest$CNAME <- str_sub(maptest$CNAME, 1,3)
jdx <- st_read("shapefile/china_map/九段线GS（2019）1719号.geojson")
library(fuzzyjoin)
maptest.se <- stringdist_left_join(maptest, map.se, by = 'CNAME',  method="lcs", max_dist=2.9) %>%
  select(species, geometry, NAME)

ggplot() +
  geom_sf(data = st_as_sf(maptest.se), aes(fill = species)) +
  geom_sf(data=jdx, linewidth=.2)+
  scale_fill_gradient(na.value = "white", low = "#ADD8E6", high = "#4169E1") + 
  labs(fill=NULL)+
  theme_minimal()+
  theme(element_text(family = "STHeiti"))

