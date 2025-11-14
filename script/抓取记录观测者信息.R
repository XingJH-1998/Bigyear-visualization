library(dplyr)
library(purrr)
library(furrr)
library(httr)
library(rvest)
library(stringr)
library(auk)
library(ggpubr)
library(tidyr)
showtext::showtext_auto()

# —— 健壮抓取函数：错误返回 NA，不中断 —— 
getChecklistObserver_safe <- function(submissionID) {
  tryCatch({
    Sys.sleep(runif(1, 0.15, 0.5))  # 并发抖动
    url <- sprintf("https://ebird.org/checklist/%s", submissionID)
    
    resp <- httr::RETRY(
      "GET",
      url,
      httr::add_headers(`User-Agent` = "R-ebird-map/1.0 (contact: your_email@example.com)"),
      times = 4,
      pause_base = 1,
      pause_cap = 6,
      terminate_on = c(400, 403, 404, 410),  # 这些状态直接放弃
      httr::timeout(15)
    )
    
    if (httr::status_code(resp) != 200) return(NA_character_)
    
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    doc <- read_html(txt)
    vals <- html_text2(html_elements(doc, "[data-participant-userdisplayname]"))
    vals <- unique(trimws(vals[nzchar(vals)]))
    if (length(vals) == 0) NA_character_ else vals
  }, error = function(e) NA_character_)
}

# —— 读取数据与准备唯一 ID —— 
df <- read.csv("/Users/tangerine/Downloads/MyEBirdData0822.csv")
subid <- readRDS("/Users/tangerine/Documents/matched_submissionID.rda")
subid_new <- df %>% distinct(ID = Submission.ID) %>%
  filter(!(ID %in% subid$ID))

# —— 并行计划（控制并发数，避免打断/限流）——
workers <- max(1, min(4, future::availableCores() - 1))  # 稍微保守一点更稳
future::plan(future::multisession, workers = workers)

# —— 并行抓取 + 自动进度条 —— 
subid_new$Observers_list <- furrr::future_map(
  subid_new$ID,
  getChecklistObserver_safe,
  .options  = furrr::furrr_options(seed = TRUE),
  .progress = TRUE              # 关键：用 furrr 自带进度条，替代 with_progress
)

subid_new <- subid_new %>%
  mutate(Observers = map_chr(
    Observers_list,
    ~ if (is.character(.x) && length(.x) > 1) {
      paste(.x, collapse = ", ")
    } else if (length(.x) == 0 || (length(.x) == 1 && is.na(.x))) {
      NA_character_
    } else {
      .x[1]
    }
  )) %>%
  select(-Observers_list)

subid <- rbind(subid, subid_new)
saveRDS(subid, "matched_submissionID.rda")






