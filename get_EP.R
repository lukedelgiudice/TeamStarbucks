source("run_epoch.R")

get_EP <- function(down, ytg, fp, max_drives, n) {
  
  total_points <- 0
  
  for (i in 1:n) {
    total_points <- total_points + simulate_epoch(down, ytg, fp, max_drives)
  }
  
  return(total_points / n)
}

# testing
get_EP(1, 10, 63, 15, 1000)
