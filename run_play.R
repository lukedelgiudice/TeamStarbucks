# run_play.R
# Source required layers and fourth–down prediction modules.
source("layer_player_position.R")   # Provides assign_player_position_drive()
source("layer_run_pass.R")           # Provides assign_play_type()
source("layer_fumble.R")             # Provides simulate_fumble()
source("layer_interception.R")       # Provides simulate_interception()
source("layer_incompletion.R")       # Provides simulate_incompletion()
source("layer_yards_gained.R")       # Provides sample_yards_gained()
source("predict_fourth_down.R")      # Provides predict_fourth_down()
source("predict_field_goal.R")       # Provides predict_field_goal()

library(dplyr)

# Simple helper functions
classify_drive_tendency <- function(play_history) {
  if (length(play_history) == 0) return("neutral")
  return("neutral")
}

get_expected_play_type <- function(down, ytg, fp) {
  if (down >= 3 && ytg >= 7) return("pass")
  return("run")
}

raw_data <- readRDS("pbp2014-2024.rds")

# Preprocess historical play data.
play_data <- raw_data %>%
  filter(play_type %in% c("run", "pass")) %>%
  mutate(
    play_call = play_type,
    player_position = case_when(
      play_type == "run" ~ if_else(qb_scramble == 1, "qb", "hb"),
      play_type == "pass" ~ if_else(!is.na(pass_length),
                                    if_else(pass_length == "deep", "wr", "te"),
                                    "wr"),
      TRUE ~ "unknown"
    ),
    red_zone = yardline_100 <= 20
  )

### Fourth-down simulation helper
simulate_fourth_down <- function(fp, ytg) {
  # Use predict_fourth_down() to get probabilities for FG, GFI, and PUNT.
  options_prob <- predict_fourth_down(fp, ytg)  # Should return a named vector with names: "FG", "GFI", "PUNT"
  option <- sample(names(options_prob), 1, prob = options_prob)
  
  if (option == "FG") {
    # Field goal attempt: use distance (100 - fp) as the yardline for prediction.
    prob_fg <- predict_field_goal(100 - fp)
    made <- runif(1) < prob_fg
    if (made) {
      return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "fg", yards = 0, play_type = "field_goal"))
    } else {
      return(list(down = 5, ytg = ytg, fp = fp, exit_drive = 1, event = "missed_fg", yards = 0, play_type = "field_goal"))
    }
  } else if (option == "PUNT") {
    return(list(down = 5, ytg = ytg, fp = fp, exit_drive = 1, event = "punt", yards = 0, play_type = "punt"))
  } else if (option == "GFI") {
    # Go for it: simulate a normal play on fourth down.
    new_state <- simulate_play_internal(4, ytg, fp, (100 - fp) <= 20)
    # If the play gains fewer yards than required, it's a turnover on downs.
    if (new_state$yards < ytg) {
      new_state$exit_drive <- 1
      new_state$event <- "turnover_on_downs"
      return(new_state)
    } else {
      # Success: reset down to 1 and yards-to-go.
      new_state$down <- 1
      new_state$ytg <- 10
      return(new_state)
    }
  }
}

### Internal simulation (used for downs 1-3 and fourth down GFI plays)
simulate_play_internal <- function(down, ytg, fp, red_zone, play_history = list()) {
  # Decide if an unexpected play is triggered.
  drive_tendency <- classify_drive_tendency(play_history)
  expected_play <- get_expected_play_type(down, ytg, fp)
  unexpected_threshold <- 0.15
  unexpected_trigger <- runif(1) < unexpected_threshold
  
  if (unexpected_trigger && drive_tendency != "neutral") {
    play_call <- if (expected_play == "run") "pass" else "run"
    pass_length <- if (play_call == "pass") {
      if (red_zone) "short" else sample(c("short", "deep"), 1)
    } else NA_character_
    player_position <- if (play_call == "run") "hb" else ifelse(pass_length == "deep", "wr", "te")
    unexpected_flag <- TRUE
  } else {
    player_position <- assign_player_position_drive(down, ytg, fp, red_zone, play_data)
    play_call <- assign_play_type(player_position, down, ytg, fp, red_zone, play_data)
    pass_length <- if (play_call == "pass") {
      if (down == 3 && ytg > 10) "deep" else "short"
    } else NA_character_
    unexpected_flag <- FALSE
  }
  
  # Simulate turnovers.
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
  
  # Sample yards gained using the precomputed mixture distributions.
  yards <- sample_yards_gained(play_call, player_position, pass_length, red_zone, unexpected_flag)
  max_yards <- 100 - fp
  yards <- min(yards, max_yards)
  
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

### Main simulation function – uses fourth down simulation when needed.
simulate_play <- function(down, ytg, fp, red_zone, play_history = list()) {
  if (down == 4) {
    return(simulate_fourth_down(fp, ytg))
  } else {
    return(simulate_play_internal(down, ytg, fp, red_zone, play_history))
  }
}

# Expose the simulate_play function for use by run_drive.R.
run_play <- simulate_play
