source("run_drive.R")
source("utils.R")
source("assign_player_position.R")

run_epoch <- function(down, ytg, fp, max_drives = 10) {
  team_status <- -1
  cumulative_drives <- 0
  
  if (!is.na(fp) && fp > 100) {
    no_score <- FALSE
  }
  
  else {
    no_score <- TRUE
  }
  
  cat("starting epoch simulation with state: down =", down, ", ytg =", ytg, ", fp =", fp, "\n")
  
  while(no_score && cumulative_drives < max_drives) {
    team_status <- team_status * -1
    cumulative_drives <- cumulative_drives + 1
    
    cat("drive", cumulative_drives, ": starting state of down =", down, ", ytg =", ytg, ", fp =", fp, ", team_status =", team_status, "\n")
    
    tmp_state <- run_drive(down, ytg, fp)
    
    if (!is.na(tmp_state$FP) && tmp_state$FP > 100) {
      down <- NA
      ytg <- NA
      fp <- tmp_state$FP
      no_score <- FALSE
      event <- tmp_state$event
      cat("drive ended with scoring event:", event, "\n")
    }
    
    else {
      down <- 1
      ytg <- 10
      fp <- tmp_state$FP
      no_score <- TRUE
      event <- tmp_state$event
      cat("drive ended with non-scoring event:", event, "\n")
    }
  }
  
  score <- team_status * compute_score(event)
  cat("final field position (fp):", fp, ", drives:", cumulative_drives, ", team_status:", team_status, ", score:", score, "\n")
  
  return(score)
}

# run_epoch(2, 7, 34)
