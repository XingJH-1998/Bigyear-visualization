library(tidyverse)
showtext::showtext.auto()
df <- read.csv("MyEBirdData.csv")
subid <- readRDS("matched_submissionID.rda")
INCLUDE_OBSERVERS <- c("Allen Xue","Ao Wang","Brook Wang","Fishing Cat","Hanyang Ye","Haru Z","Jiahua Xing","Mengshuai Ge","Raven X", "Tianhao Zhang", "Tianhao Zhao", "Tunwu Kuo", "WEICHI LI",  "Wenxuan Zhang", "XC Bu", "Xiaoyan Yu", "Xiaoyu Yan", "Xingyu Li", "YIRAN WANG", "Yeyuan Cao", "Zhuofei Lu", "Zongzhuang Liu", "jingyao wu", "shuyi Lin", "yanguiyu hao", "小 鸥", "智健 梁", "茹晖 徐", "铭俊 黄", "马 文辉", "Weed S", "白尾 海雕", "Yixiao Wang", "Yinan Wu", "David Chen", "Richard Zhang","wanda yang","Junyang Zhao", "Siyao Xu", "Bing Dong", "Chengyi Liu")
EXCLUDE_OBSERVERS <- c("Big Year", "Anonymous eBirder")

# —— 1) 回并并筛选到物种级 —— 
df_with_obs <- df %>%
  left_join(subid, by = c("Submission.ID" = "ID")) %>%
  left_join(ebird_taxonomy, by = c("Scientific.Name" = "scientific_name")) %>%
  filter(category == "species")

# —— 2) 拆分观测者为长表，并清洗 —— 
df_long <- df_with_obs %>%
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

library(lubridate)
df2 <- df.china %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    YearMonth = floor_date(Date, "month")
  )

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


df_time_fixed <- df_long %>%
  # 第一步：把空字符串 "" 替换为 NA，防止报错
  mutate(Time_Clean = ifelse(Time == "" | Time == "null", NA, Time)) %>%
  # 过滤掉没有时间的记录
  filter(!is.na(Time_Clean)) %>%
  mutate(
    # 第二步：使用 parse_date_time 解析 "HM p" 格式 (Hour:Minute AM/PM)
    # orders = "IM p" 代表: I=12小时制, M=分钟, p=AM/PM
    Time_Obj = parse_date_time(Time_Clean, orders = "IM p"),
    
    # 第三步：转化为 24小时制的“小数小时”，方便比大小
    # 例如：01:30 PM -> 13.5
    Hour_Decimal = hour(Time_Obj) + minute(Time_Obj)/60
  )

# 检查一下转换是否正确（调试用）
head(df_time_fixed %>% select(Time, Time_Obj, Hour_Decimal), 10)

time_extremes <- df_time_fixed %>%
  group_by(Observers) %>%
  summarise(
    # 最早的开始时间 (最小的小数小时)
    Earliest_Start_Decimal = min(Hour_Decimal, na.rm = TRUE),
    # 最晚的开始时间 (最大的小数小时)
    Latest_Start_Decimal = max(Hour_Decimal, na.rm = TRUE),
    # 为了展示好看，顺便把原字符串也取出来
    Earliest_Time_Str = Time[which.min(Hour_Decimal)],
    Latest_Time_Str = Time[which.max(Hour_Decimal)],
    .groups = "drop"
  )

# --- A. 卷王之王 (The Early Bird) Top 5 ---
early_birds <- time_extremes %>%
  arrange(Earliest_Start_Decimal) %>% # 升序，越小越早
  slice_head(n = 5)

print("=== Early Bird Top 5 ===")
print(early_birds %>% select(Observers, Earliest_Time_Str))

# --- B. 夜猫子 (The Night Owl) Top 5 ---
night_owls <- time_extremes %>%
  arrange(desc(Latest_Start_Decimal)) %>% # 降序，越大越晚
  slice_head(n = 5)

print("=== Night Owl Top 5 ===")
print(night_owls %>% select(Observers, Latest_Time_Str))

# 也可以画一个“观鸟活跃时间分布图” (比如每个人喜欢在几点出门)
p_active_time <- ggplot(df_time_fixed, aes(x = Hour_Decimal, y = reorder(Observers, Hour_Decimal, FUN = median))) +
  # 半透明的抖动点，展示时间分布密度
  geom_jitter(height = 0.2, alpha = 0.4, color = "steelblue", size = 1) + 
  # 标记凌晨和深夜的界限 (例如 6点前和 18点后)
  geom_vline(xintercept = c(6, 18), linetype = "dashed", color = "gray") +
  scale_x_continuous(
    breaks = seq(0, 24, 3), 
    labels = function(x) paste0(x, ":00")
  ) +
  labs(
    title = "Birder Activity Clock",
    subtitle = "Points represent start times of checklists (24h format)",
    x = "Time of Day (24h)",
    y = "Observer"
  ) +
  theme_minimal()

print(p_active_time)

distance_stats <- df_long %>%
  # 必须去重 Submission.ID，因为一个清单里有很多物种，距离是一样的
  distinct(Submission.ID, Observers, Distance.Traveled..km.) %>%
  group_by(Observers) %>%
  summarise(total_km = sum(Distance.Traveled..km., na.rm = TRUE)) %>%
  arrange(desc(total_km)) %>%
  slice_head(n = top_n)




media_stats <- df_long %>%
  filter(ML.Catalog.Numbers == TRUE) %>%
  group_by(Observers) %>%
  summarise(n_media_species = n_distinct(Scientific.Name)) %>% # 或者 n() 看素材总数
  arrange(desc(n_media_species))



family_stats <- df_long %>%
  group_by(Observers) %>%
  summarise(n_families = n_distinct(family)) %>%
  arrange(desc(n_families))




# 按日期排序
team_cumulative <- df_long %>%
  arrange(Date) %>%
  select(Date, Scientific.Name) %>%
  # 只要每一天出现的物种
  distinct(Date, Scientific.Name) %>%
  # 找出每个物种全队第一次看到的日期
  group_by(Scientific.Name) %>%
  summarise(First_Sighting = min(Date)) %>%
  ungroup() %>%
  arrange(First_Sighting) %>%
  mutate(
    Team_Total = row_number() # 累计值
  )

# 绘图
p_curve <- ggplot(team_cumulative, aes(x = as.Date(First_Sighting), y = Team_Total)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_area(fill = "steelblue", alpha = 0.2) +
  theme_minimal() +
  labs(title = "Team Cumulative Life List 2025", x = "Date", y = "Total Species")



library(ggplot2)
library(patchwork) # 拼图神器

# 1. 定义统一的颜色映射
all_observers <- unique(observer_stats$Observers)
# 创建自定义调色板 (如果人数多，可以用 RColorBrewer 或 viridis)
obs_colors <- setNames(scales::hue_pal()(length(all_observers)), all_observers)

# 2. 封装一个绘图函数，减少重复代码
plot_bar <- function(data, x_var, y_var, title, y_lab) {
  ggplot(data, aes(x = reorder(.data[[x_var]], .data[[y_var]]), 
                   y = .data[[y_var]], 
                   fill = .data[[x_var]])) + # 增加 fill
    geom_col(show.legend = FALSE) + # 不显示图例，因为名字在轴上
    coord_flip() +
    scale_fill_manual(values = obs_colors) + # 应用统一颜色
    labs(x = NULL, y = y_lab, title = title) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank() # 去掉多余的横线
    )
}

# 3. 生成图形
p1 <- plot_bar(top_species, "Observers", "n_species", "Total Species", "Count")
p2 <- plot_bar(top_unique, "Observers", "n_unique_species", "Unique Findings", "Count")
p3 <- plot_bar(top_records, "Observers", "n_records", "Total Checklists", "Count")
p4 <- plot_bar(top_countries, "Observers", "n_countries", "Countries Visited", "Count")

# 4. 组合 (Patchwork 语法)
# 布局：上面两张，下面两张，加一个总标题
final_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = '🐦 Team Birding Summary 202X',
    subtitle = 'Who 
    is the ultimate birder?',
    caption = 'Data source: eBird'
  )

print(final_plot)



library(ggwordcloud)

# 统计最常看到的鸟（按次数，不是按只数，避免一大群鸟占主导）
species_cloud_data <- df_long %>%
  count(Common.Name, sort = TRUE) %>%
  slice_head(n = 50)

ggplot(species_cloud_data, aes(label = Common.Name, size = n, color = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_gradient(low = "darkgreen", high = "red")

