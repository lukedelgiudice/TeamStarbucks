#Accounts for most rigid scenario but should be expanded to consider factors such as time left, point differential, and range while potentially ignoring ytg

kickfg <- function(down, ytg, fp) {
  if (down == 4 && ytg < 2 && fp <= 95 && fp >= 70) {
    return(TRUE)  
  #THis can be expanded to further distinguish between kicking punt or go for it
    } else
    return(FALSE)  
  }

#Find percentages at what rate field goals are blocked/missed vs percentage of conversions of low ytg 4th down conversions to find when its worth to go for what

