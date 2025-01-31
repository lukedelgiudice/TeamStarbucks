source("drive_score")

run_drive <- function(yd_line) {
  yardage_gained <- sample(0:100, size = 1)
  
  new_yd_line <- yd_line + yardage_gained
  
  result <- drive_result(new_yd_line)
  
  return(list(yardage_gained = yardage_gained, new_yd_line = new_yd_line, result = result))
}
