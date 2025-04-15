library(nnet)

cache_file <- "multinom_model.rds"

if (file.exists(cache_file)) {
  multinom_model <- readRDS(cache_file)
} else {
  footballData <- readRDS("pbp2014-2024.rds")
  fourthDownData <- footballData[footballData$down == 4 & footballData$play_type %in% c("pass", "run", "field_goal", "punt"), ]
  
  fourthDownData$fourth_down_options <- factor(
    if_else(fourthDownData$play_type == "field_goal", "FG", if_else(fourthDownData$play_type == "punt", "PUNT", "GFI")),
    levels = c("FG", "GFI", "PUNT")
  )
  
  multinom_model <- multinom(fourth_down_options ~ yardline_100 + ydstogo, data = fourthDownData)
  
  saveRDS(multinom_model, file = cache_file)
  cat("saved multinom_model to", cache_file, "\n")
}

predict_fourth_down <- function(fp, ytg) {
  predictions <- predict(multinom_model, newdata = data.frame(yardline_100 = 100 - fp, ydstogo = ytg), type = "probs")
  return(predictions)
}
