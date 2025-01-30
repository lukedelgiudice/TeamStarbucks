simulate_epoch <- function(down, ytg, fp) {
  points <- 0
  while (down <= 4) {
    play_result <- run_play(down, ytg, fp)
    
    if (play_result$turnover) {
      return(0) 
    }
    
    if (play_result$touchdown) {
      return(7) 
    }
    
    if (play_result$field_goal) {
      return(3)  
    }
    
    down <- play_result$down
    ytg <- play_result$ytg
    fp <- play_result$fp
  }
  points
}
