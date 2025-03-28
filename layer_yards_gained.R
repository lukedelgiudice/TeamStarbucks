library(mclust)

sample_yards_gained <- function(play_call, player_position, red_zone = FALSE) {
  data <- readRDS("pbp2014-2024.rds")
  data <- assign_player_position(data)
  
  subsetData <- data[data$play_type == play_call & data$player_position == player_position, ]

  if (red_zone) {
    subsetData <- subsetData[subsetData$yardline_100 <= 20, ]
  } 
  
  else {
    subsetData <- subsetData[subsetData$yardline_100 > 20, ]
  }
  
  subsetData <- subsetData[!is.na(subsetData$yards_gained), ]
  
  if(nrow(subsetData) < 30){
    subsetData <- data[data$play_type == play_call, ]
    subsetData <- subsetData[!is.na(subsetData$yards_gained), ]
  }
  
  if(nrow(subsetData) == 0){
    if(play_call == "run"){
      return(max(0, round(rnorm(1, 3, 2))))
    } 
    
    else {
      return(max(0, round(rnorm(1, 7, 3))))
    }
  }
  
  fit <- try(Mclust(subsetData$yards_gained, G = 1:3, verbose = FALSE), silent = TRUE)
  
  if(inherits(fit, "try-error")) {
    m <- mean(subsetData$yards_gained, na.rm = TRUE)
    s <- sd(subsetData$yards_gained, na.rm = TRUE)
    
    return(max(0, round(rnorm(1, m, s))))
  }
  
  params <- fit$parameters
  G <- fit$G
  comp <- sample(1:G, size = 1, prob = params$pro)
  mu <- params$mean[comp]
  sigma <- sqrt(params$variance$sigmasq[comp])
  
  yg <- round(rnorm(1, mean = mu, sd = sigma))
  return(max(0, yg))
}


