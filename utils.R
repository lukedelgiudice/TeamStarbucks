compute_score <- function(fp) {
  if (is.na(fp)) {
    return(0)
  }
  
  else if (fp >= 105) {
    return(7)
  }
  
  else if (fp >= 80) {
    return(3)
  }
  
  else {
    return(0)
  }
}