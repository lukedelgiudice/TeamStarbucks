source("layer_player_position.R")
source("layer_run_pass.R")
source("layer_fumble.R")
source("layer_interception.R")
source("layer_incompletion.R")
source("layer_yards_gained.R")

library(dplyr)

# Placeholder functions (assumed to exist)
classify_drive_tendency <- function(play_history) {
  # Simplified for this example
  if (length(play_history) == 0) return("neutral")
  return("neutral")
}

get_expected_play_type <- function(down, ytg, fp) {
  # Naive implementation for unexpected plays
  if (down >= 3 && ytg >= 7) return("pass")
  return("run")
}

raw_data <- readRDS("pbp2014-2024.rds")

# Preprocess historical data
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
    red_zone = yardline_100 <= 20
  )

simulate_play <- function(down, ytg, fp, red_zone, play_history = list()) {
  # Handle unexpected plays
  drive_tendency <- classify_drive_tendency(play_history)
  expected_play <- get_expected_play_type(down, ytg, fp)
  unexpected_threshold <- 0.15
  unexpected_trigger <- runif(1) < unexpected_threshold
  
  if (unexpected_trigger && drive_tendency != "neutral") {
    play_call <- if (expected_play == "run") "pass" else "run"
    pass_length <- if (play_call == "pass") {
      if (red_zone) "short" else sample(c("short", "deep"), 1)
    } else NA_character_
    player_position <- if (play_call == "run") "hb" else if (pass_length == "deep") "wr" else "te"
    unexpected_flag <- TRUE
  } else {
    # Main decision tree
    player_position <- assign_player_position(down, ytg, fp, red_zone, play_data)
    play_call <- assign_play_type(player_position, down, ytg, fp, red_zone, play_data)
    pass_length <- if (play_call == "pass") {
      if (down == 3 && ytg > 10) "deep" else "short"
    } else NA_character_
    unexpected_flag <- FALSE
  }
  
  # Simulate turnovers and incompletions
  if (play_call == "run") {
    if (simulate_fumble(play_call, player_position, play_data)) {
      return(list(down = down, ytg = ytg, fp = fp, exit_drive = 1, event = "fumble", yards = 0, play_type = play_call))
    }
  } else if (play_call == "pass") {
    if (simulate_interception(play_call, pass_length, play_data)) {
      return(list(down = down, ytg = ytg, fp = fp, exit_drive = 1, event = "interception", yards = 0, play_type = play_call))
    }
    if (simulate_incompletion(play_call, pass_length, play_data)) {
      new_down <- down + 1
      exit_drive <- if (new_down > 4) 1 else 0
      event <- if (new_down > 4) "turnover_on_downs" else "incompletion"
      return(list(down = new_down, ytg = ytg, fp = fp, exit_drive = exit_drive, event = event, yards = 0, play_type = play_call))
    }
  }
  
  # Sample yards gained
  yards <- sample_yards_gained(play_call, player_position, pass_length, red_zone, unexpected_flag)
  max_yards <- 100 - fp
  yards <- min(yards, max_yards)
  
  # Update game state
  new_fp <- fp + yards
  new_ytg <- max(0, ytg - yards)
  new_down <- if (new_ytg == 0) 1 else (down + 1)
  new_ytg <- if (new_ytg == 0) 10 else new_ytg
  exit_drive <- 0
  event <- "play_complete"
  
  if (new_fp >= 100) {
    return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "td", yards = yards, play_type = play_call))
  }
  
  if (new_down > 4) {
    exit_drive <- 1
    event <- "turnover_on_downs"
  }
  
  list(
    down = new_down,
    ytg = new_ytg,
    fp = new_fp,
    exit_drive = exit_drive,
    event = event,
    yards = yards,
    play_type = play_call,
    unexpected = unexpected_flag
  )
}