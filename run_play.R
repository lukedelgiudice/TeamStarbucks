source("predict_field_goals.R")
source("predict_fourth_down.R")

early_downs <- function(down, ytg, fp) {
  # Turnover (B)
  turnover_prob <- 0.05  # UPDATE PROB
  if(runif(1) < turnover_prob) {
    return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "TO"))
  }
  
  # Simulate the play
  yg <- sample(0:15, 1, prob = rep(1/16, 16))
  new_fp <- fp + yg
  
  # Check if touchdown occurred (C)
  if(new_fp >= 100) {
    return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "TD"))
  }
  
  # Update ytd & d (D)
  new_ytg <- max(0, ytg - yg)
  new_down <- ifelse(new_ytg == 0, 1, down + 1)
  new_ytg <- ifelse(new_ytg == 0, 10, new_ytg)
  
  return(list(down = new_down, ytg = new_ytg, fp = new_fp, exit_drive = 0, event = "none"))
}

down_four <- function(down, ytg, fp) {
  
  # Call helper function to get the probabilities of the 3 options
  fourth_down_probs <- predict_fourth_down(fp, ytg)
  
  play_types <- c("FG", "GFI", "PUNT")
  play_choice <- sample(play_types, size = 1, prob = fourth_down_probs) # Use new probabilities from helper
  
  # Field goal
  if (play_choice == "FG") {
    # Kick distance
    kick_distance <- (100 - fp) + 10 # Add 10 for hold distance
    
    fg_prob <- predict_field_goal(kick_distance)
    
    success <- runif(1) < fg_prob
    
    if (success) {
      # Made field goal
      return(list(down = NA, ytg = NA, fp = 115, exit_drive = 1, event = "FG_made"))
    } 
    else {
      # Missed field goal
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "FG_missed"))
    }
  }
  
  # Punt
  else if (play_choice == "PUNT") {
    # Turnover on downs
    if (runif(1) < 0.9) {
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "PUNT"))
    } 
    
    # Abnormal punt
    else {
      new_fp <- fp + 5
      
      # Touchdown punt return
      if(new_fp >= 100) {
        return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "TD"))
      }
      
      # Mishandled punt
      return(list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0, event = "PUNT_mishandled"))
    }
  } 
  
  # Go for it
  else if (play_choice == "GFI") {
    # Conversion prob depends on ytg
    conversion_prob <- ifelse(ytg <= 2, 0.7, 0.3)
    
    success <- runif(1) < conversion_prob
    
    if (success) {
      # Successful conversion (simulate yards gained beyond required distance)
      yg <- sample(ytg:15, 1)
      new_fp <- fp + yg
      if (new_fp >= 100) {
        return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "TD"))
      } 
      else {
        return(list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0, event = "GFI_success"))
      }
    }
    
    # Unsuccessful conversion (turnover)
    else {
      return(list(down = NA, ytg = NA, fp = fp, exit_drive = 1, event = "GFI_failure"))
    }
  }
}

# Helper functions for each down (eventually adding play selection nuance)

down_one <- function(ytg, fp) {
  early_downs(1, ytg, fp)
}

down_two <- function(ytg, fp) {
  early_downs(2, ytg, fp)
}

down_three <- function(ytg, fp) {
  early_downs(3, ytg, fp)
}

down_four <- function(ytg, fp) {
  down_fourth(4, ytg, fp)
}

run_play <- function(down, ytg, fp) {
  
  # First down
  if (down == 1) {
    return(down_one(ytg, fp))
  } 
  
  # Second down
  else if (down == 2) {
    return(down_two(ytg, fp))
  } 
  
  # Third down
  else if (down == 3) {
    return(down_three(ytg, fp))
  } 
  
  # Fourth down
  else if (down == 4) {
    return(down_four(ytg, fp))
  }
}
