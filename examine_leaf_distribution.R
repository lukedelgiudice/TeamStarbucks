library(ggplot2)
library(mclust)
library(dplyr)
library(purrr)

assign_player_position_analysis <- function(data, ref_data, probs) {
  data %>%
    mutate(player_position = case_when(
      play_call == "run" ~ ifelse(!is.na(rusher_player_id) & rusher_player_id != "", "hb", "qb"),
      play_call == "pass" ~ if_else(!is.na(pass_length) & pass_length == "deep", "wr", "te"),
      TRUE ~ "unknown"
    ))
}

examine_leaf_distribution <- function(play_call, player_position, red_zone = FALSE) {
  raw_data <- readRDS("pbp2014-2024.rds")
  
  position_ref_data <- raw_data %>%
    filter(play_type %in% c("run", "pass", "qb_kneel", "qb_spike"))
  
  position_probs <- list(
    run = position_ref_data %>%
      filter(play_type == "run", !is.na(rusher_player_id)) %>%
      count(position = ifelse(rusher_player_id != "", "hb", "qb")) %>%
      mutate(prob = n/sum(n)) %>%
      select(-n),
    
    pass = list(
      deep = c(wr = 0.8, te = 0.2),
      short = c(te = 0.5, wr = 0.3, hb = 0.2),
      middle = c(te = 0.7, wr = 0.3),
      sideline = c(wr = 0.7, te = 0.3),
      default = c(te = 0.4, wr = 0.4, hb = 0.2)
    )
  )
  
  regular_play_data <- position_ref_data %>%
    mutate(
      play_call = case_when(
        play_type %in% c("run", "qb_kneel", "qb_spike") ~ "run",
        play_type == "pass" ~ "pass"
      )
    ) %>%
    assign_player_position_analysis(ref_data = position_ref_data, probs = position_probs)
  
  subsetData <- regular_play_data %>%
    filter(play_call == play_call,
           player_position == player_position,
           fumble == 0)
  
  if (play_call == "pass") {
    subsetData <- subsetData %>%
      filter(interception == 0, incomplete_pass == 0)
  }
  
  if (red_zone) {
    subsetData <- subsetData %>% filter(yardline_100 <= 20)
  }
  
  else {
    subsetData <- subsetData %>% filter(yardline_100 > 20)
  }
  
  subsetData <- subsetData %>% filter(!is.na(yards_gained))
  
  if (nrow(subsetData) < 30) {
    message("too few observations, using play_call level data")
    
    subsetData <- regular_play_data %>%
      filter(play_call == play_call, fumble == 0)
    
    if (play_call == "pass") {
      subsetData <- subsetData %>% filter(interception == 0, incomplete_pass == 0)
    }
    
    subsetData <- subsetData %>% filter(!is.na(yards_gained))
  }
  
  if (nrow(subsetData) == 0) stop("no valid data for this leaf")
  
  p <- ggplot(subsetData, aes(x = yards_gained)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste("Yards Gained Distribution (", play_call, player_position,
                       ifelse(red_zone, "red zone )", "not red zone )")),
         x = "Yards Gained", y = "Density")
  print(p)
  
  fit <- Mclust(subsetData$yards_gained, G = 1:3, verbose = FALSE)
  print(summary(fit))
  
  return(fit)
}

# example
fit_leaf <- examine_leaf_distribution("run", "hb", red_zone = FALSE)
