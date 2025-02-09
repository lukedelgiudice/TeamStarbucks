

run_play <- function(down, ytg, fp) {
  exit_drive = 0
  
  if (down < 4) {
    outcome = early_downs(down, ytg, fp)
    if (outcome$fp == 105) {
      exit_drive = 1
    }
  }
  else if (down >= 4) {
    outcome = fourth_down(down, ytg, fp)
    
    # Made field goal
    if (outcome$fp == 115) {
      
    }
  }
  
  # sample in 4th down function whether not to go for field goal
  # 2 or less go for it
  # for punts, divide field into chunks and then get samples within each (filter to those regions)
  
  outcome
  
  list(down=1, ytg=10, fp=new_fp, exit_drive) 
}