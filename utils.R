compute_score <- function(event) {
  if (is.na(event)) {
    return(0)
  }
  
  else if (event == "td") {
    return(7)
  }
  
  else if (event == "fg") {
    return(3)
  }
  
  else {
    return(0)
  }
}