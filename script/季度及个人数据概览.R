library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(openxlsx)
library(viridis)
library(forcats)
library(tidytext)
showtext::showtext.auto()

df <- read.csv("MyEBirdData.csv")
subid <- readRDS("matched_submissionID.rda")
ebird_taxonomy <- read.csv("ebird_taxonomy.csv")
INCLUDE_OBSERVERS <- c("Allen Xue","Ao Wang","Brook Wang","Fishing Cat","Hanyang Ye","Haru Z","Jiahua Xing","Mengshuai Ge","Raven X", "Tianhao Zhang", "Tianhao Zhao", "Tunwu Kuo", "WEICHI LI",  "Wenxuan Zhang", "XC Bu", "Xiaoyan Yu", "Xiaoyu Yan", "Xingyu Li", "YIRAN WANG", "Yeyuan Cao", "Zhuofei Lu", "Zongzhuang Liu", "jingyao wu", "shuyi Lin", "yanguiyu hao", "小 鸥", "智健 梁", "茹晖 徐", "铭俊 黄", "马 文辉", "Weed S", "白尾 海雕", "Yixiao Wang", "Yinan Wu", "David Chen", "Richard Zhang","wanda yang","Junyang Zhao", "Siyao Xu", "Bing Dong", "Chengyi Liu")
EXCLUDE_OBSERVERS <- c("Big Year", "Anonymous eBirder")

# —— 1) 回并并筛选到物种级 —— 
df_with_obs <- df %>%
  filter(Count>0) %>%
  left_join(subid, by = c("Submission.ID" = "ID")) %>%
  left_join(ebird_taxonomy, by = c("Scientific.Name" = "SCI_NAME")) %>%
  filter(CATEGORY %in% c("domestic", "issf", "species"))

tax_replace <- ebird_taxonomy %>%
  select(
    SPECIES_CODE, TAXON_ORDER, CATEGORY, TAXON_CONCEPT_ID, PRIMARY_COM_NAME, 
    SCI_NAME, ORDER,  FAMILY, SPECIES_GROUP, REPORT_AS
  )

df_with_obs2 <- df_with_obs %>%
  left_join(
    tax_replace,
    by = c("REPORT_AS" = "SPECIES_CODE"),
    suffix = c("", ".rep")
  )

df_with_obs2 <- df_with_obs2 %>%
  mutate(
    Scientific.Name   = coalesce(SCI_NAME, Scientific.Name),
    PRIMARY_COM_NAME  = coalesce(PRIMARY_COM_NAME.rep, PRIMARY_COM_NAME),
    ORDER             = coalesce(ORDER.rep, ORDER),
    FAMILY            = coalesce(FAMILY.rep, FAMILY),
    TAXON_ORDER       = coalesce(TAXON_ORDER.rep, TAXON_ORDER),
    CATEGORY          = coalesce(CATEGORY.rep, CATEGORY),
    TAXON_CONCEPT_ID  = coalesce(TAXON_CONCEPT_ID.rep, TAXON_CONCEPT_ID),
    SPECIES_GROUP     = coalesce(SPECIES_GROUP.rep, SPECIES_GROUP),
    REPORT_AS         = coalesce(REPORT_AS.rep, REPORT_AS)
  ) %>%
  select(-ends_with(".rep"))

# —— 2) 拆分观测者为长表，并清洗 —— 
df_long <- df_with_obs2 %>%
  mutate(Observers = strsplit(Observers, ",\\s*")) %>%  # 注意这里用 Observers_list
  unnest(Observers) %>%
  filter(Observers %in% INCLUDE_OBSERVERS) %>%
  mutate(Observers = trimws(Observers)) %>%
  filter(!(Observers %in% EXCLUDE_OBSERVERS),
         !is.na(Observers), nzchar(Observers))

df_long[df_long$Country %in% c("TW", "HK"),]$Country <- "CN"
df_long[df_long$Country %in% c("XX"),]$Country <- "ZA"


df.1st <- df_long %>% 
  dplyr::mutate(Year=year(Date), Month=month(Date)) %>%
  filter(Month %in% c(1:3)) %>%
  select(Common.Name, State.Province, Location, Date, Observers)

df.2st <- df_long %>% 
  dplyr::mutate(Year=year(Date), Month=month(Date)) %>%
  filter(Month %in% c(4:6)) %>%
  select(Common.Name, State.Province, Location, Date, Observers) %>%
  arrange(Observers, Location)

df.3st <- df_long %>% 
  dplyr::mutate(Year=year(Date), Month=month(Date)) %>%
  filter(Month %in% c(7:9)) %>%
  select(Common.Name, State.Province, Location, Date, Observers) %>%
  arrange(Observers, Location)

df.4st <- df_long %>% 
  dplyr::mutate(Year=year(Date), Month=month(Date)) %>%
  filter(Month %in% c(10:12)) %>%
  select(Common.Name,Scientific.Name,  State.Province, Location, Date, Observers) %>%
  arrange(Observers, Location)

df.gms <- df.4st %>% filter(Observers=="Mengshuai Ge")


df.tony <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Ao Wang")

df.spidey <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Xingyu Li")

df.raven <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Raven X")

df.alpha <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="铭俊 黄")

df.bxc <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="XC Bu")

df.lzz <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Zongzhuang Liu")

df.wyx <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Brook Wang")

df.yxy <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Xiaoyu Yan")

df.xxl <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Hanyang Ye")

dddd <- df_long %>%
  filter(Observers=="Hanyang Ye") %>% 
  select(Common.Name, Location, Date, Observers, Country, State.Province) %>% 
  arrange(Date)

dddd2 <- dddd %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date)
  )

observer_year_country <- dddd2 %>%
  group_by(Observers, Year, Country) %>%
  summarise(
    start_date = min(Date, na.rm = TRUE),
    end_date   = max(Date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

df.ou <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="小 鸥")

df.lzf <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Zhuofei Lu")

df.gms <- df_long %>%
  select(Common.Name, Location, Date, Observers, State.Province) %>%
  filter(Observers=="Mengshuai Ge") %>%
  filter(Date>"2025-10-01")
length(unique(df.gms$Common.Name))

df.lcy <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Chengyi Liu")


df.weed <- df_long %>%
  select(Common.Name, Location, Date, Observers) %>%
  filter(Observers=="Weed S")
