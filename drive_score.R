drive_result <- function(yd_line) {
  if (yd_line <= 100) {
    return("No score")
  } else if (yd_line > 100 && yd_line <= 110) {
    return("Touchdown")
  } else if (yd_line > 110 && yd_line <= 120) {
    return("Field goal")
  } else {
    return("Field goal")
  }
}
