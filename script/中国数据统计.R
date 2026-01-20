#中国数据统计

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(openxlsx)
showtext::showtext_auto()

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

# —— 3) 提取“国家”代码（State.Province 的前2个字符）—— 
df_long <- df_long %>%
  mutate(Country = substr(State.Province, 1, 2) |> toupper()) %>%
  filter(!is.na(Country), nzchar(Country))

df.china <- df_long %>% filter(Country %in% c("CN","TW", "HK"))
df2 <- df.china %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    YearMonth = floor_date(Date, "month")
  )
length(unique(df2$Common.Name))
length(unique(df2$Scientific.Name))

monthly_species <- df2 %>%
  distinct(YearMonth, Scientific.Name) %>%
  count(YearMonth, name = "n_species_month")

monthly_cum_species <- df2 %>%
  arrange(Date) %>%
  distinct(YearMonth, Scientific.Name) %>%
  group_by(YearMonth) %>%
  summarise(
    cum_species = n_distinct(
      Scientific.Name[
        YearMonth <= cur_group()$YearMonth
      ]
    ),
    .groups = "drop"
  )

first_month <- df2 %>%
  group_by(Scientific.Name) %>%
  summarise(
    first_month = min(YearMonth),
    .groups = "drop"
  )

monthly_cum_species <- first_month %>%
  count(first_month, name = "new_species") %>%
  arrange(first_month) %>%
  mutate(cum_species = cumsum(new_species)) %>%
  rename(YearMonth = first_month)

plot_df <- left_join(
  monthly_species,
  monthly_cum_species,
  by = "YearMonth"
)

scale_factor <- 1.5

ggplot(plot_df, aes(x = YearMonth)) +
  # 柱状图
  geom_col(
    aes(y = n_species_month * scale_factor),
    fill = "#ADD8E6",
    width = 25
  ) +
  
  # 柱状图数字标签：当月鸟种数
  geom_text(
    aes(
      y = n_species_month * scale_factor,
      label = n_species_month
    ),
    vjust = -0.3,
    size = 3
  ) +
  
  # 折线图
  geom_line(
    aes(y = cum_species),color="#4169E1",
    linewidth = 1
  ) +
  geom_point(
    aes(y = cum_species),
    size = 2
  ) +
  
  # 折线图数字标签：当月新增物种数
  geom_text(
    aes(
      y = cum_species,
      label = paste0("+", new_species)
    ),
    vjust = -1,
    size = 3
  ) +
  
  scale_y_continuous(
    name = "当年内累计记录鸟种数",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "当月记录鸟种数"
    )
  ) +
  
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "1 month"
  ) +
  
  labs(
    x = NULL,
    title = "每月鸟种数与加新数"
  ) +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

name_map <- df.china %>%
  filter(!is.na(Common.Name)) %>%
  group_by(Scientific.Name) %>%
  summarise(
    Common.Name.std = Common.Name[which.min(nchar(Common.Name))],
    .groups = "drop"
  )

df.china <- df.china %>%
  select(-Common.Name) %>%
  left_join(name_map, by = "Scientific.Name") %>%
  rename(Common.Name = Common.Name.std)

unique_species <- df.china %>%
  distinct(Common.Name, Observers) %>%
  count(Common.Name, name = "n_observer") %>%
  filter(n_observer == 1) %>%
  select(Common.Name)
df_unique <- df.china %>%
  semi_join(unique_species, by = "Common.Name")

observer_unique_species_table <- df_unique %>%
  group_by(Observers, Common.Name) %>%
  summarise(
    Provinces = paste(sort(unique(State.Province)), collapse = ", "),
    n_records = n(),
    first_date = min(as.Date(Date)),
    .groups = "drop"
  ) %>%
  arrange(Observers, first_date)

observer_unique_species_list <- observer_unique_species_table %>%
  group_by(Observers) %>%
  summarise(
    n_unique_species = n(),
    Species_list = paste(
      Common.Name,
      collapse = "；"
    ),
    .groups = "drop"
  )

plot_df <- observer_unique_species_list %>%
  arrange(n_unique_species) %>%
  mutate(Observers = factor(Observers, levels = Observers))

ggplot(plot_df,
       aes(x = n_unique_species, y = Observers)) +
  geom_col(fill = "grey60") +
  geom_text(
    aes(label = n_unique_species),
    hjust = -0.1,
    size = 3.5
  ) +
  labs(
    x = "独有物种数",
    y = "",
    title = "团队观鸟大年：独有物种贡献榜"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank()
  )

write.xlsx(plot_df,"队伍唯一鸟种.xlsx")

observer_province_checklists <- df.china %>%
  distinct(Submission.ID, Observers, State.Province) %>%
  count(
    Observers,
    State.Province,
    name = "n_checklists"
  )

top_local <- observer_province_checklists %>%
  group_by(State.Province) %>%
  slice_max(n_checklists, n = 1, with_ties = FALSE) %>%
  ungroup()


