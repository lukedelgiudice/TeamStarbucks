# run_epoch.R
source("run_drive.R")
source("utils.R")  # Contains compute_score()

run_epoch <- function(down, ytg, fp, max_drives = 10) {
  # team_status: +1 indicates original offense eventually scores; -1 if opponent scores.
  team_status <- -1  # will flip immediately so that first drive uses team_status = 1.
  cumulative_drives <- 0
  
  if (!is.na(fp) && fp > 100) {
    no_score <- FALSE
  } else {
    no_score <- TRUE
  }
  
  cat("Starting epoch simulation with state: down =", down, ", ytg =", ytg, ", fp =", fp, "\n")
  
  while(no_score && cumulative_drives < max_drives) {
    team_status <- team_status * -1
    cumulative_drives <- cumulative_drives + 1
    
    cat("Drive", cumulative_drives, ": starting state: down =", down, ", ytg =", ytg, ", fp =", fp,
        ", team_status =", team_status, "\n")
    
    tmp_state <- run_drive(down, ytg, fp)
    
    if (!is.na(tmp_state$FP) && tmp_state$FP > 100) {
      down <- NA
      ytg <- NA
      fp <- tmp_state$FP
      no_score <- FALSE
      cat("Drive ended with scoring event:", tmp_state$event, "\n")
    } else {
      down <- 1      # always start new drive at 1st down
      ytg <- 10      # always 10 yards to go
      fp <- tmp_state$FP  # use the drive outcome field position
      no_score <- TRUE
      cat("Drive ended with non-scoring event:", tmp_state$event, "\n")
    }
  }
  
  score <- team_status * compute_score(fp)
  cat("Final field position (fp):", fp, ", drives:", cumulative_drives,
      ", team_status:", team_status, ", score:", score, "\n")
  
  return(score)
}

# Test run_epoch with an example state.
run_epoch(2, 7, 34)
