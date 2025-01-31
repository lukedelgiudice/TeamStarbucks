source("run_drive.R")

simulate_epoch <- function(down, ytg, fp, n) {
  points <- 0
  team <- 1
  drives <- 0
  
  while (drives < n) {
    drive_result <- run_drive(fp)
    
    if (drive_result$result == "Touchdown") {
      return(7 * team) # this didn't seem to work without keyword return (*ask*)
    } 
    else if (drive_result$result == "Field goal") {
      return(3 * team)
    }
    
    team <- team * -1
    fp <- sample(0:50, size = 1)
    drives <- drives + 1
  }
  
  0
}

# testing
# simulate_epoch(25)
