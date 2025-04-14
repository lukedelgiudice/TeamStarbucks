source("predict_field_goal.R")
source("predict_fourth_down.R")
source("layer_player_position.R")
source("layer_run_pass.R")
source("layer_fumble.R")
source("layer_interception.R")
source("layer_incompletion.R")
source("layer_yards_gained.R")

library(dplyr)
library(purrr)

raw_data <- readRDS("pbp2014-2024.rds")

position_ref_data <- raw_data %>%
  filter(play_type %in% c("run", "pass", "qb_kneel", "qb_spike"))

fourth_down_data <- raw_data %>%
  filter(play_type %in% c("punt", "field_goal"))

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
  assign_player_position(
    ref_data = position_ref_data,
    probs = position_probs
  )

simulate_fourth_down <- function(ytg, fp) {
  
  yardline_100 <- 100 - fp
  play_probs <- predict_fourth_down(yardline_100, ytg)
  
  final_probs <- c(
    FG = play_probs["FG"],
    GFI = play_probs["GFI"],
    PUNT = play_probs["PUNT"]
  )
  
  play_choice <- sample(
    c("fg", "gfi", "punt"),
    size = 1,
    prob = final_probs
  )
  
  if (play_choice == "fg") {
    kick_distance <- (100 - fp) + 10
    fg_prob <- predict_field_goal(kick_distance)
    
    if (runif(1) < fg_prob) {
      return(list(down = NA, ytg = NA, fp = 115, exit_drive = 1, event = "FG_made"))
    } else {
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "FG_missed"))
    }
  } else if (play_choice == "punt") {
    if (runif(1) < 0.9) {
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "punt"))
    } else {
      new_fp <- fp + 5
      if (new_fp >= 100) {
        return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "td"))
      }
      return(list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0, event = "punt_mishandled"))
    }
  } else if (play_choice == "gfi") { # switch this to instead use simulate_play and fix the 4th down checks in simulate_play
    conversion_prob <- ifelse(ytg <= 2, 0.7, 0.3)
    if (runif(1) < conversion_prob) {
      yg <- sample(ytg:15, 1)
      new_fp <- fp + yg
      if (new_fp >= 100) {
        return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "td"))
      }
      return(list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0, event = "gfi_success"))
    } else {
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "gfi_failure"))
    }
  }
}

simulate_play <- function(down, ytg, fp, red_zone, play_history = list()) {
  state <- list(down = down, ytg = ytg, fp = fp, red_zone = red_zone)
  
  exit_drive <- 0
  event <- "non_terminal"
  yards <- 0
  unexpected_flag <- FALSE
  
  unexpected_threshold <- 0.15 * (1 + 0.1*length(play_history))
  unexpected_trigger <- runif(1) < unexpected_threshold
  drive_tendency <- classify_drive_tendency(play_history)
  expected_play <- get_expected_play_type(down, ytg, fp)
  
  play <- NULL
  
  if (unexpected_trigger && drive_tendency != "neutral") {
    play_context <- case_when(
      expected_play == "run" && drive_tendency == "run_heavy" ~ "run_surprise",
      expected_play == "pass" && drive_tendency == "pass_heavy" ~ "pass_surprise",
      TRUE ~ "neutral"
    )
    
    if (play_context != "neutral") {
      unexpected_type <- switch(play_context,
                                "run_surprise" = "pass_deep",
                                "pass_surprise" = "run",
                                sample(c("run", "pass"), 1)
      )
      
      if (state$red_zone && unexpected_type == "pass_deep") {
        unexpected_type <- "pass_short"
        play$pass_length <- "short"
      }
      
      play <- tibble(
        play_type = unexpected_type,
        pass_location = ifelse(unexpected_type == "pass_deep", 
                               sample(c("left", "right"), 1), "middle"),
        pass_length = ifelse(unexpected_type == "pass_deep", "deep", "short")
      ) %>%
        assign_player_position(
          ref_data = position_ref_data,
          probs = position_probs
        ) %>%
        mutate(play_call = unexpected_type)
      
      unexpected_flag <- TRUE
    }
  }
  
  if (is.null(play)) {
    play <- tibble(
      play_type = ifelse(state$down < 4, "run", "pass"),
      pass_location = sample(c("left", "middle", "right"), 1),
      pass_length = sample(c("short", "deep"), 1)
    ) %>%
      assign_player_position(
        ref_data = position_ref_data,
        probs = position_probs
      ) %>%
      assign_run_pass(data = regular_play_data)
    
    unexpected_flag <- FALSE
  }
  
  play_call <- play$play_call
  player_position <- play$player_position
  
  if (simulate_fumble(play_call, player_position, regular_play_data)) {
    return(list(down = state$down, ytg = state$ytg, fp = state$fp, exit_drive = 1, event = "fumble", yards = 0))
  }
  
  if (play_call == "pass") {
    if (simulate_interception(play_call, regular_play_data)) {
      return(list(down = state$down, ytg = state$ytg, fp = state$fp, exit_drive = 1, event = "interception", yards = 0))
    }
    
    if (simulate_incompletion(play_call, regular_play_data)) {
      new_down <- state$down + 1
      return(list(down = new_down, ytg = state$ytg, fp = state$fp, exit_drive = 0, event = "incompletion", yards = 0))
    }
  }
  
  yards <- sample_yards_gained(play_call, player_position, state$red_zone, regular_play_data)
  yards <- max(0L, min(yards, 99L))
  
  max_yards <- 100 - state$fp
  yards <- min(yards, max_yards)
  
  if (state$red_zone) {
    yards <- min(yards, 20)
  }
  
  new_fp <- state$fp + yards
  
  new_ytg <- max(0, state$ytg - yards)
  new_down <- ifelse(new_ytg == 0, 1, state$down + 1)
  new_ytg <- ifelse(new_ytg == 0, 10, new_ytg)
  
  exit_drive <- 0
  event <- "non_terminal"
  
  if (state$down == 4 && new_ytg > 0) {
    exit_drive <- 1
    event <- "turnover_on_downs"
  }
  
  result <- list(
    down = new_down,
    ytg = new_ytg,
    fp = new_fp,
    exit_drive = exit_drive,
    event = event,
    yards = yards,
    play_type = play$play_call,
    unexpected = unexpected_flag
  )
  
  return(result)
}

run_play <- function(down, ytg, fp, red_zone = FALSE, play_history = list()) {
  if (down == 4) {
    result <- simulate_fourth_down(ytg, fp)
  } else {
    result <- simulate_play(down, ytg, fp, red_zone, play_history)
  }
  return(result)
}
