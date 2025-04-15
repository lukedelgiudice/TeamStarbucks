# predict_fourth_down.R
library(nnet)

footballData <- readRDS("pbp2014-2024.rds")
fourthDownData <- footballData[footballData$down == 4 & footballData$play_type %in% c("pass", "run", "field_goal", "punt"), ]

fourthDownData$fourth_down_options <- factor(
  if_else(fourthDownData$play_type == "field_goal", "FG", 
          if_else(fourthDownData$play_type == "punt", "PUNT", "GFI")),
  levels = c("FG", "GFI", "PUNT")
)

multinom_model <- multinom(fourth_down_options ~ yardline_100 + ydstogo, data = fourthDownData)

predict_fourth_down <- function(fp, ytg) {
  predictions <- predict(multinom_model, newdata = data.frame(yardline_100 = fp, ydstogo = ytg), type = "probs")
  predictions
}
