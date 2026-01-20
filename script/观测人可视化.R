#library()

df <- read.csv("MyEBirdData.csv")
subid <- readRDS("matched_submissionID.rda")
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
  mutate(Observers = trimws(Observers)) %>%
  filter(!(Observers %in% EXCLUDE_OBSERVERS),
         !is.na(Observers), nzchar(Observers))

# —— 3) 提取“国家”代码（State.Province 的前2个字符）—— 
df_long <- df_long %>%
  mutate(Country = substr(State.Province, 1, 2) |> toupper()) %>%
  filter(!is.na(Country), nzchar(Country))

if(F)
{
  df.weed <- df_long %>%
    filter(Observers=="Weed S")
  table(df.weed$State.Province)
}

# —— 4) 三类统计 —— 
# 4.1 每人看了多少物种（以 Scientific.Name 去重）
species_per_observer <- df_long %>%
  group_by(Observers) %>%
  summarise(n_species = n_distinct(Scientific.Name), .groups = "drop")

# 4.2 每人传了多少条记录（以 Submission.ID 去重）
records_per_observer <- df_long %>%
  group_by(Observers) %>%
  summarise(n_records = n_distinct(Submission.ID), .groups = "drop")

# 4.3 “仅此人看到的物种”
#     先统计每个物种被几位（去重后）观测者见到
species_observers <- df_long %>%
  group_by(Scientific.Name) %>%
  summarise(n_observers = n_distinct(Observers), .groups = "drop")

#     仅被 1 位观测者看到的物种
unique_species_keys <- species_observers %>%
  filter(n_observers == 1) %>%
  pull(Scientific.Name)

#     每个观测者的“独享物种”数量（以 Scientific.Name 去重）
unique_species_per_observer <- df_long %>%
  filter(Scientific.Name %in% unique_species_keys) %>%
  group_by(Observers) %>%
  summarise(n_unique_species = n_distinct(Scientific.Name), .groups = "drop")

# —— 5) 汇总为一个统计表 —— 
observer_stats <- species_per_observer %>%
  left_join(unique_species_per_observer, by = "Observers") %>%
  left_join(records_per_observer, by = "Observers") %>%
  tidyr::replace_na(list(n_unique_species = 0L))

# —— 6) 取各指标 Top 5 并绘图 —— 
top_n <- 7

top_species <- observer_stats %>%
  arrange(desc(n_species)) %>% slice_head(n = top_n)

top_unique <- observer_stats %>%
  arrange(desc(n_unique_species)) %>% slice_head(n = top_n)

top_records <- observer_stats %>%
  arrange(desc(n_records)) %>% slice_head(n = top_n)

countries_per_observer <- df_long %>%
  group_by(Observers) %>%
  summarise(n_countries = n_distinct(Country), .groups = "drop")

top_countries <- countries_per_observer %>%
  arrange(desc(n_countries)) %>% slice_head(n = top_n)

# 图1：物种数 Top 5
p1 <- ggplot(top_species, aes(x = reorder(Observers, n_species), y = n_species)) +
  geom_col() +
  coord_flip() +
  labs(x = "Observer", y = "Number of species",
       title = sprintf("Top %s Observers by Number of Species", top_n)) +
  theme_minimal()

# 图2：独享物种数 Top 5
p2 <- ggplot(top_unique, aes(x = reorder(Observers, n_unique_species), y = n_unique_species)) +
  geom_col() +
  coord_flip() +
  labs(x = "Observer", y = "Number of unique-only species",
       title = sprintf("Top %s Number of Unique-only Species", top_n)) +
  theme_minimal()

# 图3：记录数 Top 5
p3 <- ggplot(top_records, aes(x = reorder(Observers, n_records), y = n_records)) +
  geom_col() +
  coord_flip() +
  labs(x = "Observer", y = "Number of records",
       title = sprintf("Top %s Number of Records", top_n)) +
  theme_minimal()

# 图4：国家数 Top 5
p4 <- ggplot(top_countries, aes(x = reorder(Observers, n_countries), y = n_countries)) +
  geom_col() +
  coord_flip() +
  labs(x = "Observer", y = "Number of countries",
       title = sprintf("Top %s Number of Countries", top_n)) +
  theme_minimal()

# 组合展示（2x2）
ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

