source("predict_field_goal.R")
source("predict_fourth_down.R")
source("layer_player_position.R")
source("layer_run_pass.R")
source("layer_fumble.R")
source("layer_interception.R")
source("layer_incompletion.R")
source("layer_yards_gained.R")

simulate_fourth_down <- function(ytg, fp) {
  fourth_down_probs <- predict_fourth_down(fp, ytg)
  play_types <- c("fg", "gfi", "punt")
  play_choice <- sample(play_types, size = 1, prob = fourth_down_probs)
  
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
  
  play <- data.frame(
    play_type = ifelse(state$down < 4, "run", "pass"),
    pass_location = sample(c("left", "middle", "right"), 1),
    pass_length = sample(c("short", "deep"), 1),
    rusher_player_id = "",
    passer_player_id = "",
    receiver_player_id = "",
    stringsAsFactors = FALSE
  )
  
  play <- assign_player_position(play)

  play <- assign_run_pass(play)
  play_call <- play$play_call
  player_position <- play$player_position
  
  if (simulate_fumble(play_call, player_position)) {
    return(list(down = state$down, ytg = state$ytg, fp = state$fp, exit_drive = 1, event = "fumble", yards = 0))
  }
  
  if (play_call == "pass") {
    if (simulate_interception(play_call)) {
      return(list(down = state$down, ytg = state$ytg, fp = state$fp, exit_drive = 1, event = "interception", yards = 0))
    }
    
    if (simulate_incompletion(play_call)) {
      new_down <- state$down + 1
      new_state <- list(down = new_down, ytg = state$ytg, fp = state$fp, exit_drive = 0)
      
      return(list(down = new_state$down, ytg = new_state$ytg, fp = new_state$fp, exit_drive = 0, event = "incompletion", yards = 0))
    }
  }
  
  yards <- sample_yards_gained(play_call, player_position, state$red_zone)
  new_fp <- state$fp + yards
  
  if (new_fp >= 100) {
    return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "td", yards = yards))
  }
  
  new_ytg <- max(0, state$ytg - yards)
  new_down <- ifelse(new_ytg == 0, 1, state$down + 1)
  
  if (new_ytg == 0) new_ytg <- 10
  
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



