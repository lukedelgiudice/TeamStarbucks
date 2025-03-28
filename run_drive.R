source("run_play.R")

# The drive continues while new_state$exit_drive==0 (i.e. the drive remains in play)
run_drive <- function(D, YTG, FP){
  
  # Simulate a play given the current state.
  new_state <- run_play(D, YTG, FP)
  
  # Check the flag: if exit_drive == 0, the drive continues
  if(new_state$exit_drive == 0) {
    # if we should stay with the current drive, simply call it again with the
    # new state
    run_drive(new_state$down, new_state$ytg, new_state$fp)
  }
  
  else {
    # Otherwise, return the state (which might be a turnover, field goal, etc)
    list(D = new_state$down, YTG = new_state$ytg, FP = new_state$fp, event = new_state$event)
  }
}

# TEST: starting drive on first and 10 at 20 yard line
result <- run_drive(1, 10, 20)
print(result)
