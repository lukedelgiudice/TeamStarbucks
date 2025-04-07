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
  
  # these should be improved, but unfortunately this data has no info on actual position
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
  fg_prob_base <- fourth_down_data %>%
    filter(!is.na(field_goal_attempt)) %>%
    summarize(prob = mean(field_goal_attempt == 1, na.rm = TRUE)) %>%
    pull(prob)
  
  punt_prob_base <- fourth_down_data %>%
    filter(!is.na(punt_attempt)) %>%
    summarize(prob = mean(punt_attempt == 1, na.rm = TRUE)) %>%
    pull(prob)
  
  gfi_prob_base <- 1 - (fg_prob_base + punt_prob_base)
  
  if (fp >= 65) {
    weights <- c(fg = 3.0, gfi = 2.5, punt = 0.1)
  }
  
  else if (fp >= 40) {
    weights <- c(fg = 1.5, gfi = 1.2, punt = 0.8)
  }
  
  else {
    weights <- c(fg = 0.5, gfi = 0.7, punt = 1.8)
  }
  
  adjusted_probs <- c(
    fg = fg_prob_base * weights["fg"],
    gfi = gfi_prob_base * weights["gfi"],
    punt = punt_prob_base * weights["punt"]
  )
  
  final_probs <- prop.table(adjusted_probs)
  
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
    } 
    
    else {
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "FG_missed"))
    }
  } 
  
  else if (play_choice == "punt") {
    if (runif(1) < 0.9) {
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "punt"))
    } 
    
    else {
      new_fp <- fp + 5
      
      if (new_fp >= 100) {
        return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "td"))
      }
      
      return(list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0, event = "punt_mishandled"))
    }
  } 
  
  else if (play_choice == "gfi") {
    conversion_prob <- ifelse(ytg <= 2, 0.7, 0.3)
    
    if (runif(1) < conversion_prob) {
      yg <- sample(ytg:15, 1)
      new_fp <- fp + yg
      
      if (new_fp >= 100) {
        return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "td"))
      }
      
      return(list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0, event = "gfi_success"))
    } 
    
    else {
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "gfi_failure"))
    }
  }
}

simulate_play <- function(down, ytg, fp, red_zone = FALSE) {
  state <- list(down = down, ytg = ytg, fp = fp, red_zone = red_zone)
  
  if (state$down == 4) {
    return(simulate_fourth_down(state$ytg, state$fp))
  }
  
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
  
  max_yards <- 100 - state$fp
  yards <- min(yards, max_yards)
  
  new_fp <- state$fp + yards
  if (new_fp >= 100) {
    return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "td", yards = yards))
  }
  
  new_ytg <- max(0, state$ytg - yards)
  new_down <- ifelse(new_ytg == 0, 1, state$down + 1)
  new_ytg <- ifelse(new_ytg == 0, 10, new_ytg)
  
  return(list(down = new_down, ytg = new_ytg, fp = new_fp, exit_drive = 0, event = "non_terminal", yards = yards))
}

run_play <- function(down, ytg, fp, red_zone = FALSE) {
  result <- simulate_play(down, ytg, fp, red_zone)
  return(result)
}

# test
init_state <- list(
  down = 1,
  ytg = 10,
  fp = 25,
  red_zone = FALSE
)
result <- run_play(init_state$down, init_state$ytg, init_state$fp, init_state$red_zone)
print(result)





# Additional time series modeling to decide when to perform unorthodox plays (ie: long pass on 3rd down)
# Answer question of whether teams start to adjust defense to specific playstyle that unorthodox plays could beat
# Subset to data where teams exhibit consistent playstyles and then make an "unexpected" play
# Decide which play based on historical trends within each game (looking at game specific data)
# Classify certain offense playstyles like short pass, long pass, etc
# 
# Need to remove the bias of unexpected plays which are actually expected, like passing on third and long
# as a common running team, this can be done by also factoring in the traditional non-time series probabilities that
# reflect what aggregate decision making to expect (this will include unorthodox plays but they should be out weighted)
# 
# The final outcome is an additional variable in the model which is waiting to activate an unorthodox play for +EP
# 
# Also need to consider the fact that unorthodox plays can not be used on repeat (at the very least need a cooldown)



