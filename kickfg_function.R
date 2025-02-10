#Accounts for most rigid scenario but should be expanded to consider factors such as time left, point differential, and range

kickfg <- function(down, ytg, fp) {
  if (down == 4 && ytg < 2 && fp <= 95 && fp >= 70) {
    return(TRUE)  
  #THis can be expanded to further distinguish between kicking punt or go for it
    } else
    return(FALSE)  
  }


