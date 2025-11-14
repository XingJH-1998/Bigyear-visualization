library(httr)       
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(purrr)
library(tibble)


user_id <- "NjYxNjU0MA"   # 你要查询的 id
regions <- c("CN", "HK", "MO", "TW")   # 大陆、香港、澳门、台湾


prov_dict <- c(
  "CN-11" = "北京市","CN-12" = "天津市","CN-13" = "河北省","CN-14" = "山西省","CN-15" = "内蒙古自治区",
  "CN-21" = "辽宁省","CN-22" = "吉林省","CN-23" = "黑龙江省","CN-31" = "上海市","CN-32" = "江苏省",
  "CN-33" = "浙江省","CN-34" = "安徽省","CN-35" = "福建省","CN-36" = "江西省","CN-37" = "山东省",
  "CN-41" = "河南省","CN-42" = "湖北省","CN-43" = "湖南省","CN-44" = "广东省","CN-45" = "广西壮族自治区",
  "CN-46" = "海南省","CN-50" = "重庆市","CN-51" = "四川省","CN-52" = "贵州省","CN-53" = "云南省",
  "CN-54" = "西藏自治区","CN-61" = "陕西省","CN-62" = "甘肃省","CN-63" = "青海省","CN-64" = "宁夏回族自治区",
  "CN-65" = "新疆维吾尔自治区",
  "TW" = "台湾省","HK" = "香港特别行政区","MO" = "澳门特别行政区"
)


provinces_all <- c(
  "北京市","天津市","河北省","山西省","内蒙古自治区",
  "辽宁省","吉林省","黑龙江省","上海市","江苏省",
  "浙江省","安徽省","福建省","江西省","山东省",
  "河南省","湖北省","湖南省","广东省","广西壮族自治区",
  "海南省","重庆市","四川省","贵州省","云南省",
  "西藏自治区","陕西省","甘肃省","青海省","宁夏回族自治区",
  "新疆维吾尔自治区","台湾省","香港特别行政区","澳门特别行政区"
)

# -------------------- 3) 查询函数：按区域抓该用户省级物种数 --------------------

fetch_region_counts <- function(user_id, region_code) {
  url <- paste0("https://ebird.org/prof/count/species?username=", user_id, "&r=", region_code)
  resp <- httr::GET(
    url,
    httr::add_headers(`User-Agent` = "R-ebird-map/1.0 (contact: tangerinw0w0@163.com)")
  )
  httr::stop_for_status(resp)
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  
  dat_df <- tryCatch(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.data.frame(dat_df) && ncol(dat_df) >= 2) {
    out <- tibble(
      code = as.character(dat_df[[1]]),
      n    = as.integer(dat_df[[2]])
    ) %>% filter(!is.na(code), !is.na(n))
  } else {
    dat_list <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) list())
    if (length(dat_list) == 0) {
      message("Empty response for region: ", region_code)
      return(tibble(code = character(), n = integer()))
    }
    dat_list <- map(dat_list, as.list)
    dat_list <- keep(dat_list, ~ length(.x) >= 2 && !is.null(.x[[1]]) && !is.null(.x[[2]]))
    if (length(dat_list) == 0) {
      message("No valid items parsed for region: ", region_code)
      return(tibble(code = character(), n = integer()))
    }
    out <- tibble(
      code = map_chr(dat_list, ~ as.character(.x[[1]])),
      n    = map_int(dat_list, ~ as.integer(.x[[2]]))
    ) %>% filter(!is.na(code), !is.na(n))
  }
  
  # —— 仅对 TW 做“省级合并” —— #
  if (identical(region_code, "TW")) {
    if (any(out$code == "TW")) {
      out <- out %>% filter(code == "TW")
    }
  }
  
  out
}


all_counts <- purrr::map_dfr(regions, ~fetch_region_counts(user_id, .x))
all_counts <- all_counts %>%
  mutate(province = dplyr::case_when(
    str_starts(code, "CN-") ~ prov_dict[code],
    code %in% c("TW","TW-") ~ prov_dict["TW"],
    code %in% c("HK","HK-") ~ prov_dict["HK"],
    code %in% c("MO","MO-") ~ prov_dict["MO"],
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(province)) %>%
  group_by(province) %>%
  summarise(species = sum(as.integer(n)), .groups = "drop")


df_prov <- all_counts %>%
  tidyr::complete(province = provinces_all, fill = list(species = 0L)) %>%
  arrange(match(province, provinces_all))

species_status <- ebird_taxonomy %>%
  select(species_code, scientific_name, category, report_as)
df_tw <- read.csv("/Users/tangerine/Downloads/MyEBirdData.csv") %>%
  mutate(Countrycode= str_sub(State.Province,1,2)) %>%
  filter(Countrycode=="TW") %>%
  left_join(species_status, by = c("Scientific.Name" = "scientific_name")) %>%
  select(Scientific.Name, species_code, category, report_as) 
df_prov[df_prov$province=="台湾省",]$species <- length(unique(df_tw$Scientific.Name))

# bird_cnt <- 800  # 设置图注上限
max_val <- max(df_prov$species, na.rm = TRUE)

map_geo <- sf::st_read("shapefile/china_map/中国省级地图GS（2019）1719号.geojson",
                       quiet = TRUE)
stopifnot("CNAME" %in% names(map_geo))  # CNAME 为中文全称列

map_joined <- map_geo %>%
  dplyr::select(CNAME, geometry) %>%
  dplyr::left_join(df_prov, by = c("CNAME" = "province")) %>%
  dplyr::mutate(label_val = ifelse(species > 0, species, ""))

jdx <- sf::st_read("shapefile/china_map/九段线GS（2019）1719号.geojson",
                   quiet = TRUE)


n_steps <- 100
ylgn_100 <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGn"))(n_steps)
breaks_vec <- seq(0, max_val, length.out = n_steps + 1)

p <- ggplot() +
  geom_sf(data = map_joined,
          aes(fill = species),
          color = "#EEEEEE",
          linewidth = 0.3) +
  geom_sf(data = jdx, linewidth = 0.25, color = "black") +
  scale_fill_gradientn(
    colours = c("white", "#ADD8E6", "#4169E1"),
    values = scales::rescale(c(0, 1, max_val)),  
    limits = c(0, max_val),
    na.value = "white"
  ) +
  geom_sf_text(data = map_joined,
               aes(label = label_val),
               family = "STHeiti",
               size = 3,
               color = "black") +
  coord_sf() +
  theme_minimal(base_family = "STHeiti") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )
p
