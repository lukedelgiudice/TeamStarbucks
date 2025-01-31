source("drive_score")

run_drive <- function(yd_line, down, ytg) {
  yardage_gained <- sample(0:100, size = 1)
  
  new_yd_line <- yd_line + yardage_gained
  new_ytg <- ytg - yardage_gained
  
  if (new_ytg <= 0) {
    down <- 1
    new_ytg <- 10
  } 
  else {
    down <- down + 1
  }
  
  result <- drive_result(new_yd_line)
  
  return(list(new_yd_line = new_yd_line, down = down, ytg = new_ytg, result = result))
}
