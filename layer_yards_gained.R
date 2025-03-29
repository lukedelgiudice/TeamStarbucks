library(mclust)

sample_yards_gained <- function(play_call, player_position, red_zone, play_data) {
  if (red_zone) {
    subsetData <- play_data[play_data$play_call == play_call & 
                              play_data$player_position == player_position &
                              play_data$yardline_100 <= 20, ]
  } else {
    subsetData <- play_data[play_data$play_call == play_call & 
                              play_data$player_position == player_position &
                              play_data$yardline_100 > 20, ]
  }
  
  subsetData <- subsetData[!is.na(subsetData$yards_gained), ]
  
  if(nrow(subsetData) < 30) {
    subsetData <- data[data$play_call == play_call, ]
    subsetData <- subsetData[!is.na(subsetData$yards_gained), ]
  }
  
  if(nrow(subsetData) == 0) {
    return(if(play_call == "run") max(0, round(rnorm(1, 3, 2))) else max(0, round(rnorm(1, 7, 3))))
  }
  
  fit <- try(Mclust(subsetData$yards_gained, G = 1:3, verbose = FALSE), silent = TRUE)
  
  if(inherits(fit, "try-error")) {
    m <- mean(subsetData$yards_gained, na.rm = TRUE)
    s <- sd(subsetData$yards_gained, na.rm = TRUE)
    return(max(0, round(rnorm(1, m, s))))
  }
  
  params <- fit$parameters
  comp <- sample(1:fit$G, 1, prob = params$pro)
  yg <- round(rnorm(1, params$mean[comp], sqrt(params$variance$sigmasq[comp])))
  max(0, yg)
}
