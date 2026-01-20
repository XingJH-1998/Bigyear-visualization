#世界累积鸟种数动图
library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)
library(transformr)
library(data.table)
showtext::showtext.auto()

df <- read.csv("MyEBirdData.csv")
subid <- readRDS("matched_submissionID.rda")
ebird_taxonomy <- read.csv("ebird_taxonomy.csv")
INCLUDE_OBSERVERS <- c("Allen Xue","Ao Wang","Brook Wang","Fishing Cat","Hanyang Ye","Haru Z","Jiahua Xing","Mengshuai Ge","Raven X", "Tianhao Zhang", "Tianhao Zhao", "Tunwu Kuo", "WEICHI LI",  "Wenxuan Zhang", "XC Bu", "Xiaoyan Yu", "Xiaoyu Yan", "Xingyu Li", "YIRAN WANG", "Yeyuan Cao", "Zhuofei Lu", "Zongzhuang Liu", "jingyao wu", "shuyi Lin", "yanguiyu hao", "小 鸥", "智健 梁", "茹晖 徐", "铭俊 黄", "马 文辉", "Weed S", "白尾 海雕", "Yixiao Wang", "Yinan Wu", "David Chen", "Richard Zhang","wanda yang","Junyang Zhao", "Siyao Xu", "Bing Dong", "Chengyi Liu")
EXCLUDE_OBSERVERS <- c("Big Year", "Anonymous eBirder")

# —— 1) 回并并筛选到物种级 —— 
df_with_obs <- df %>%
  mutate(Year=year(Date)) %>%
  filter(Year=="2025") %>%
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

clean_df <- df_long %>%
  mutate(
    # 确保有一个标准的2位国家代码列
    Country_Code = substr(State.Province, 1, 2), 
    Date_Parsed = ymd(Date),
    Month = floor_date(Date_Parsed, "month")
  )

# 2. 计算每个国家、每个月的新增鸟种
monthly_birds <- clean_df %>%
  group_by(Country_Code, Month) %>%
  summarise(Species_List = list(unique(Scientific.Name)), .groups = "drop")

# 3. 准备时间轴和国家列表
all_months <- seq(min(clean_df$Month), max(clean_df$Month), by = "month")

# 获取世界地图 (只包含有 geometry 的国家)
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a2_eh, geometry) %>%
  filter(!is.na(iso_a2_eh)) %>%
  mutate(iso_a2=iso_a2_eh)
world_map$iso_a2[world_map$iso_a2=="TW"] <- "CN"

# --- 关键步骤：构建 "国家 x 时间" 的完整骨架 ---
# 这样确保即使某个月某国家没有鸟，它依然存在于数据中（用来显示灰色）
full_grid <- expand_grid(
  iso_a2 = unique(world_map$iso_a2), # 世界所有国家
  Month = all_months                 # 所有时间点
)

# 4. 合并数据并计算累积值
plot_data <- full_grid %>%
  # 连入观测数据
  left_join(monthly_birds, by = c("iso_a2" = "Country_Code", "Month")) %>%
  
  # 处理累积逻辑
  arrange(iso_a2, Month) %>%
  group_by(iso_a2) %>%
  mutate(
    # 将当月鸟种列表里的 NULL 换成空字符向量，防止报错
    Species_List = map(Species_List, ~ if(is.null(.)) character(0) else .),
    # 累积去重合并 (Union)
    Cumulative_List = accumulate(Species_List, union),
    # 计算数量
    Count = map_int(Cumulative_List, length)
  ) %>%
  ungroup() %>%
  # *** 核心技巧 ***：
  # 如果 Count 为 0，说明从开始到现在都没见过鸟，我们要让它保持 NA，
  # 这样 ggplot 就会把它画成灰色。如果有鸟了，就保留数值。
  mutate(Count = if_else(Count == 0, NA_integer_, Count)) %>%
  select(iso_a2, Month, Count)

# 5. 最后将地理信息关联回去
# 注意：这会让数据变大 (Geometry x Months)，但对于国家级地图是完全能跑得动的
map_animation_data <- world_map %>%
  right_join(plot_data, by = "iso_a2")

start_month <- min(map_animation_data$Month) %m-% months(1)

start_frame <- map_animation_data %>%
  filter(Month == min(Month)) %>%   # 拿一份几何结构
  mutate(
    Month = start_month,
    Count = NA_real_
  )

map_animation_data2 <- bind_rows(
  start_frame,
  map_animation_data
)

p  <- ggplot(map_animation_data) +
  # 绘制地图
  geom_sf(aes(fill = Count), color = "white", size = 0.05) +
  
  # 设置颜色映射
  # na.value = "grey90" 是关键：没有数据的国家显示浅灰
  scale_fill_gradient(
    low = "#fde0dd",     # 很浅的红（接近白）
    high = "#a50f15",    # 深红
    na.value = "grey90",
    name = "鸟种数"
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5
    )
  )+
  # 设置地图投影 (可选，Mollweide投影适合世界地图)
  coord_sf(crs = "+proj=moll") +
  
  theme_void() + # 去掉坐标轴和背景
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "grey50")
  ) +
  
  # 动态标签
  labs(
    title = "全球累计观测鸟种数",
    subtitle = " 时间: {format(frame_time, '%Y-%m')}" # 格式化时间显示
  )

# 添加动画过渡
anim <- p + 
  transition_time(Month) +
  ease_aes('linear')

# --- 第三步：渲染输出 ---
# 建议先用低分辨率测试，确认效果后再调高 fps 和 res
animate(
  anim, 
  nframes = 60, # 帧数设为月份数的5倍，保证流畅
  fps = 10, 
  width = 1000, height = 700, res = 100,
  renderer = gifski_renderer(),
  end_pause = 20,
  start_pause = 10,
)

# 保存
anim_save("bird_accumulation_map_0117.gif")




