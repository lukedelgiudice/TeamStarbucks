

run_play <- function(down, ytg, fp) {
  exit_drive = 0
  
  if (down < 4) {
    outcome = early_downs(down, ytg, fp)
    
    # touchdown
    if (outcome$fp == 105) {
      exit_drive = 1
    }
    
    # turnover -- return this in outcome (bool)
    else if (outcome$turnover) {
      exit_drive = 1
    }
    
    # no turnover / touchdown
    else {
      # do nothing?
    }
    
  }
  else if (down >= 4) {
    outcome = fourth_down(down, ytg, fp)
    
    # Made field goal
    if (outcome$fp == 115) {
      exit_drive = 1
    }
    
    # missed field goal / didn't successfully convert 4th down conversion attempt
    else if (outcome$fp < fp + ytg) {
      exit_drive = 1
    }
    
    # DO THIS INSTEAD #
    
    if (outcome$FIELDGOAL) {
      if (outcome$fp == 115) {
        # made field goal
      }
      else {
        # missed field goal
      }
    }
    
    # make bool flags that can be used for handling switches
    else if (outcome$PUNT) {
      if (outcome$SWITCH) {
        # downs switched
      }
      else {
        # team mishandled punt
      }
    }
    
    else if (outcome$GOFORIT) {
      if (outcome$fp < fp + ytg) {
        # unsuccessful (return state to epoch function to switch possession and proceed)
      }
      else {
        # successful, stay in the drive function and proceed from  (1, 10, FP)
      }
    }
    
  }
  
  # sample in 4th down function whether not to go for field goal
  # 2 or less go for it
  # for punts, divide field into chunks and then get samples within each (filter to those regions)
  
  # initialize team status to -1.  Why not 1, since the "original offense" runs
  # the first drive?  The while loop will always have to toggle the team in 
  # possession, regardless of if there was a score or not (I suppose it doesn't
  # _have_ to, but that is overly complicated).  So we will set it to -1 and 
  # then immediately switch it when we enter the while loop.
  
  # team_status <- -1 -- epoch handles team switching!
  
  
  
  outcome
  
  list(down=1, ytg=10, fp=new_fp, exit_drive) 
}