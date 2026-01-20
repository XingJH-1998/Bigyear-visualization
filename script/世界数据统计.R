#世界数据统计

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(openxlsx)
library(viridis)
library(forcats)
library(tidytext)
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

df_long[df_long$Country %in% c("TW", "HK"),]$Country <- "CN"
df_long[df_long$Country %in% c("XX"),]$Country <- "ZA"


# 1️⃣ 每个观测者看到的鸟种数
observer_species <- df_long %>%
  group_by(Observers) %>%
  summarise(
    n_species = n_distinct(Scientific.Name),
    .groups = "drop"
  ) %>%
  arrange(desc(n_species)) %>%
  slice_head(n= 8)

# 2️⃣ 每个观测者去过的国家数
observer_countries <- df_long %>%
  group_by(Observers) %>%
  summarise(
    n_countries = n_distinct(Country),
    .groups = "drop"
  ) %>%
  arrange(desc(n_countries)) %>%
  slice_head(n= 8)

# 3️⃣ 每个国家记录的鸟种数
country_species <- df_long %>%
  group_by(Country) %>%
  summarise(
    n_species = n_distinct(Scientific.Name),
    .groups = "drop"
  ) %>%
  arrange(desc(n_species)) %>%
  slice_head(n= 8)


# Step 1：找出只被一个观测者记录的物种
unique_species <- df_long %>%
  distinct(Scientific.Name, Observers) %>%  # 去重观测者-物种组合
  count(Scientific.Name, name = "n_observer") %>%  # 每个物种被多少观测者记录
  filter(n_observer == 1) %>%
  select(Scientific.Name)

# Step 2：保留这些独占物种的记录
df_unique <- df_long %>%
  semi_join(unique_species, by = "Scientific.Name")

# Step 3：统计每个观测者独占物种数
observer_unique_counts <- df_unique %>%
  group_by(Observers) %>%
  summarise(
    n_unique_species = n_distinct(Scientific.Name),
    .groups = "drop"
  ) %>%
  arrange(desc(n_unique_species)) %>%
  slice_head(n=8)




# 假设 df_long$Observers 是字符串，可能有多个名字用逗号分隔
df_long_clean <- df_long %>%
  # 先拆分多观察者
  separate_rows(Observers, sep = ",\\s*") %>%
  # 保证去掉空格
  mutate(Observers = trimws(Observers),
         Date = as.Date(Date)) %>%   # 确保 Date 是 Date 类型
  # 去重，每个观察者每一天只算一次
  distinct(Observers, Date)

# 统计每个观察者的观鸟天数
observer_days <- df_long_clean %>%
  group_by(Observers) %>%
  summarise(Days = n(), .groups = "drop") %>%
  arrange(desc(Days)) %>%
  slice_head(n=8)

p1 <- ggplot(observer_species, aes(x = n_species, y = reorder(Observers, n_species))) +
  geom_col(fill = "#ADD8E6") +
  geom_text(aes(label = n_species), hjust = 1, size = 5) +
  labs(
    x = "鸟种数",
    y=NULL,
    title = "每位队员的鸟种数"
  ) +
  theme_minimal()

p2 <- ggplot(observer_countries, aes(x = n_countries, y = reorder(Observers, n_countries))) +
  geom_col(fill = "#3CB371") +
  geom_text(aes(label = n_countries), hjust = 1, size = 5) +
  labs(
    x = "观鸟到访国家数",
    y=NULL,
    title = "每位队员观鸟到访的国家数"
  ) +
  theme_minimal()

p3 <- ggplot(observer_unique_counts, aes(x = n_unique_species, y = reorder(Observers, n_unique_species))) +
  geom_col(fill = "#FFDEAD") +
  geom_text(aes(label = n_unique_species), hjust = 1, size = 5) +
  labs(
    x = "唯一鸟种数",
    y = NULL,
    title = "每位队员的唯一鸟种数"
  ) +
  theme_minimal()

# 绘图
p4 <- ggplot(observer_days, aes(x = reorder(Observers, Days), y = Days)) +
  geom_col(fill = "#E9967A") +
  geom_text(aes(label = Days), hjust = 1, size = 5) +
  coord_flip() +  # 横向柱状图
  labs(x = NULL, y = "观鸟天数", title = "每位队员的观鸟天数") +
  theme_minimal()

ggpubr::ggarrange(p1,p2,p3,p4,ncol=2, nrow=2, align = "hv")

# 1️⃣ 补全地区信息
df_long_region <- df_long %>%
  mutate(Region = case_when(
    # 欧洲
    Country %in% c("DK","GB","SE","NL","CH","FR","DE","LT","LV","IT","BE","MC","VA") ~ "欧洲",
    # 非洲
    Country %in% c("TZ","KE","ZA","BW","ZM","EG","MW","ZW") ~ "非洲",
    # 北美
    Country %in% c("US","MX","CR") ~ "北美洲",
    # 南美
    Country %in% c("CL", "PE") ~ "南美洲",
    # 东南亚
    Country %in% c("TH","ID","MY","PH","SG","VN","KH","LK") ~ "东南亚",
    # 大洋洲
    Country %in% c("AU","NZ") ~ "大洋洲",
    # 其他 / 未分类
    TRUE ~ "Other"
  ))

# 1. 数据预处理
plot_data <- df_long_region %>%
  # 去除缺失值（以防万一）
  filter(!is.na(Region), !is.na(Observers)) %>%
  filter(Region!="Other") %>%
  
  # 第一步：计算每个Region里每个Observer看到的鸟种数量（去重）
  group_by(Region, Observers) %>%
  summarise(
    n_species = n_distinct(Scientific.Name), # 统计不同鸟种的数量
    .groups = "drop"
  ) %>%
  
  # 第二步：每个Region只保留前7名
  group_by(Region) %>%
  slice_max(order_by = n_species, n = 7, with_ties = FALSE) %>% # with_ties=FALSE保证严格只取前7个
  ungroup() %>%
  
  # 第三步：关键步骤，在每个Region内部对Observer进行重新排序
  mutate(
    # reorder_within 会创建一个结合了Region的临时因子，用于正确排序
    Observers_ordered = reorder_within(Observers, n_species, Region)
  )

gms <- df_long_region %>% filter(Observers=="Mengshuai Ge") %>% filter(
  Region=="大洋洲"
)
length(unique(gms$Common.Name))
length(unique(gms$Scientific.Name))

lzf <- df_long_region %>% filter(Observers=="Zhuofei Lu") %>% filter(
  Region=="欧洲"
)
length(unique)


# 2. 绘图
ggplot(plot_data, aes(x = n_species, y = Observers_ordered, fill = Region)) +
  # 画柱状图
  geom_col(show.legend = FALSE, width = 0.8) +
  
  # 添加数值标签（显示在柱子末端）
  geom_text(aes(label = n_species), hjust = -0.2, size = 3.5, color = "black") +
  
  # 分面：按Region分面，scales = "free_y" 允许每个面的Y轴独立
  facet_wrap(~Region, scales = "free", ncol = 2) + 
  
  # 修正Y轴标签：去除reorder_within产生的后缀
  scale_y_reordered() +
  
  # 配色：使用 viridis 配色方案，专业且对色盲友好
  scale_fill_viridis_d(option = "D", alpha = 0.85) +
  
  # 调整坐标轴范围，给数值标签留出空间
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  # 主题设置
  labs(
    x = "鸟种数",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(), # 去掉Y轴网格线，让画面更干净
    strip.text = element_text(face = "bold", size = 12), # 加粗分面标题
    axis.text.y = element_text(color = "grey30"),
    plot.title = element_text(face = "bold", margin = margin(b = 10))
  )


