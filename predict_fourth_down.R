# predict_fourth_down.R
library(nnet)

# Check if cached model exists
cache_file <- "multinom_model.rds"
if (file.exists(cache_file)) {
  multinom_model <- readRDS(cache_file)
  cat("Loaded cached multinom_model from", cache_file, "\n")
} else {
  # Read raw data and build the fourth down data subset.
  footballData <- readRDS("pbp2014-2024.rds")
  fourthDownData <- footballData[footballData$down == 4 & 
                                   footballData$play_type %in% c("pass", "run", "field_goal", "punt"), ]
  
  fourthDownData$fourth_down_options <- factor(
    if_else(fourthDownData$play_type == "field_goal", "FG", 
            if_else(fourthDownData$play_type == "punt", "PUNT", "GFI")),
    levels = c("FG", "GFI", "PUNT")
  )
  
  # Optimize the multinomial regression model.
  multinom_model <- multinom(fourth_down_options ~ yardline_100 + ydstogo, data = fourthDownData)
  
  # Save the model to disk for future use.
  saveRDS(multinom_model, file = cache_file)
  cat("Saved multinom_model to", cache_file, "\n")
}

predict_fourth_down <- function(fp, ytg) {
  # Note: we assume that the input fp is on the same scale as used in the model (i.e. yardline_100 = 100 - fp).
  predictions <- predict(multinom_model, 
                         newdata = data.frame(yardline_100 = fp, ydstogo = ytg), 
                         type = "probs")
  return(predictions)
}
