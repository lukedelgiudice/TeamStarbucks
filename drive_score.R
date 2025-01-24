score_drive <- function(yardage) {
  if (yardage <= 100) {
    print("No score")
    return(0)
  } else if (yardage > 100 && yardage <= 110) {
    print("Touchdown")
    return(1)
  } else if (yardage > 110 && yardage <= 120) {
    print("Field goal")
    return(2)
  } else {
    print("Error")
    return(NA)
  }
}