# Function to simulate downs 1-3
early_downs <- function(D, YTG, FP) {
  YG <- sample(0:15, 1, prob = c(rep(1/16, 16))) # Placeholder probability distribution for sampling gained yards
  new_FP <- FP + YG 
  new_YTG <- max(0, YTG - YG)
  new_D <- ifelse(new_YTG == 0, 1, D + 1) # Reset to 1st down if achieved, else increment
  list(D = new_D, YTG = ifelse(new_YTG == 0, 10, new_YTG), FP = new_FP, exit_drive = 0)
}