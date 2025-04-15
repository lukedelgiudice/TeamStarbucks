# get_EP.R
source("run_epoch.R")

get_EP <- function(down, ytg, fp, max_drives, n) {
  total_points <- 0
  
  for (i in 1:n) {
    total_points <- total_points + run_epoch(down, ytg, fp, max_drives)
  }
  
  return(total_points / n)
}

# Testing: compute EP for initial state (e.g., 1st & 10 at fp = 63)
get_EP(1, 10, 63, 10, 10)
