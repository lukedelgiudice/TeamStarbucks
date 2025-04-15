# precompute_yards_distributions.R
library(dplyr)
library(mixtools)

precompute_yards_distributions <- function(raw_data) {
  # Preprocess data.
  play_data <- raw_data %>%
    filter(play_type %in% c("run", "pass")) %>%
    mutate(
      play_call = play_type,
      player_position = case_when(
        play_type == "run" & qb_scramble == 1 ~ "qb",
        play_type == "run" ~ "hb",
        play_type == "pass" & !is.na(pass_length) & pass_length == "deep" ~ "wr",
        play_type == "pass" & !is.na(pass_length) ~ "te",
        play_type == "pass" ~ "wr",
        TRUE ~ "unknown"
      ),
      pass_length = if_else(play_type == "pass", pass_length, NA_character_),
      yards_gained = coalesce(yards_gained, 0),
      red_zone = yardline_100 <= 20,
      valid_play = case_when(
        play_type == "run" ~ (fumble == 0 | is.na(fumble)),
        play_type == "pass" ~ ((interception == 0 | is.na(interception)) & complete_pass == 1)
      )
    ) %>%
    filter(valid_play)
  
  # Define categories.
  categories <- play_data %>%
    distinct(play_call, player_position, pass_length, red_zone) %>%
    mutate(unexpected = FALSE)
  
  fit_mixture <- function(data) {
    if (nrow(data) < 50) {
      return(list(mu = mean(data$yards_gained), sigma = sd(data$yards_gained), lambda = 1))
    }
    mix <- normalmixEM(data$yards_gained, k = 2, maxit = 100, epsilon = 1e-4)
    list(mu = mix$mu, sigma = mix$sigma, lambda = mix$lambda)
  }
  
  distributions <- list()
  for (i in 1:nrow(categories)) {
    cat <- categories[i, ]
    key <- with(cat, paste(play_call, player_position, pass_length, red_zone, unexpected, sep = "_"))
    subset <- play_data %>%
      filter(
        play_call == cat$play_call,
        player_position == cat$player_position,
        if (cat$play_call == "pass") pass_length == cat$pass_length else TRUE,
        red_zone == cat$red_zone
      )
    if (nrow(subset) > 0) {
      distributions[[key]] <- fit_mixture(subset)
    }
  }
  
  saveRDS(distributions, "yards_distributions.rds")
  return(distributions)
}

raw_data <- readRDS("pbp2014-2024.rds")
distributions <- precompute_yards_distributions(raw_data)
