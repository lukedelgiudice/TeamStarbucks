source("run_play.R")

run_drive <- function(D, YTG, FP){
  
  # get new state
  new_state <- run_play(D, YTG, FP)
  
  # Check if we should return the state or run a new play
  if(new_state$exit_drive==0){
    
    # if we should stay with the current drive, simply call it again with the
    # new state
    run_drive(new_state$D, new_state$YTG, new_state$FP)
    
  } else {
    
    # otherwise, return the current state to the run_epoch function
    list(D=new_state$D, YTG=new_state$YTG, FP=new_state$FP)
    
  }
  
}