source("run_drive.R")

simulate_epoch <- function(down, ytg, fp, max_drives) {
  points <- 0
  team <- 1
  drives <- 0
  
  while (drives < max_drives) {
    drive <- run_drive(fp, down, ytg)
    fp <- drive$new_yd_line
    down <- drive$down
    ytg <- drive$ytg
    
    if (drive$result == "Touchdown") {
      return(7 * team) # this didn't seem to work without keyword return (*ask*)
    } else if (drive$result == "Field goal") {
      return(3 * team)
    }
    
    team <- team * -1
    drives <- drives + 1
  }
  
  return(0)
}

# testing
# simulate_epoch(25)
