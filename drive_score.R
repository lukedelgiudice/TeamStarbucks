drive_result <- function(yd_line) {
  result <-NA
  
  if (yd_line <= 100) {
    result <-"No score"
  } else if (yd_line > 100 && yd_line <= 110) {
    result <-"Touchdown"
  } else if (yd_line > 110 && yd_line <= 120) {
    result <-"Field goal"
  } else {
    result <-"Error"
  }
}
