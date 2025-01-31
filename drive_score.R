drive_result <- function(yardage) {
  result <-NA
  
  if (yardage <= 100) {
    result <-"No score"
  } else if (yardage > 100 && yardage <= 110) {
    result <-"Touchdown"
  } else if (yardage > 110 && yardage <= 120) {
    result <-"Field goal"
  } else {
    result <-"Error"
  }
}