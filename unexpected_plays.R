# unexpected_plays.R
library(zoo)
library(dplyr)

precompute_unexpected_distributions <- function(window_size = 5, threshold = 0.8) {
  raw_data <- readRDS("pbp2014-2024.rds") %>%
    filter(play_type %in% c("run", "pass")) %>%
    group_by(game_id, drive) %>%
    mutate(
      run_ratio = rollapplyr(play_type == "run", width = window_size,
                             FUN = function(x) sum(x)/length(x),
                             fill = NA),
      pass_ratio = rollapplyr(play_type == "pass", width = window_size,
                              FUN = function(x) sum(x)/length(x),
                              fill = NA),
      unexpected_run = play_type == "run" & lag(pass_ratio) >= threshold,
      unexpected_pass = play_type == "pass" & lag(run_ratio) >= threshold
    ) %>%
    ungroup() %>%
    filter(unexpected_run | unexpected_pass) %>%
    select(-run_ratio, -pass_ratio)
  
  saveRDS(raw_data, "unexpected_plays_data.rds")
}

precompute_unexpected_distributions()
