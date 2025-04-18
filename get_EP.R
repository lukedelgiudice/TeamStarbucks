source("unexpected_plays.R")
source("precompute_yards_distributions.R")
source("precompute_yards_distributions_unexpected.R")
source("run_epoch.R")

library(ggplot2)

get_EP <- function(down, ytg, fp, max_drives, n) {
  total_points <- 0
  
  for (i in 1:n) {
    total_points <- total_points + run_epoch(down, ytg, fp, max_drives)
  }
  
  return(total_points / n)
}

compare_ep_by_rate <- function(down, ytg, fp, max_drives, n, rates = seq(0, 0.25, by = 0.25)) {
  results <- data.frame(Rate = numeric(0), EP = numeric(0))
  
  for (r in rates) {
    assign("UNEXPECTED_RATE", r, envir = .GlobalEnv)
    ep_val <- get_EP(down, ytg, fp, max_drives, n)
    results <- rbind(results, data.frame(Rate = r, EP = ep_val))
    cat("Rate:", r, "EP:", ep_val, "\n")
  }
  
  return(results)
}

compare_ep_across_states <- function(game_states, max_drives, n, rates = seq(0, 0.25, by = 0.25)) {
  results <- data.frame(Rate = numeric(0), EP = numeric(0))
  
  for (r in rates) {
    assign("UNEXPECTED_RATE", r, envir = .GlobalEnv)
    ep_values <- sapply(game_states, function(state) {
      get_EP(state$down, state$ytg, state$fp, max_drives, n)
    })
    avg_ep <- mean(ep_values)
    results <- rbind(results, data.frame(Rate = r, EP = avg_ep))
    cat("Rate:", r, "Average EP:", avg_ep, "\n")
  }
  
  return(results)
}

# game_states <- list(
#   list(down = 1, ytg = 10, fp = 63),
#   list(down = 2, ytg = 7, fp = 34),
#   list(down = 1, ytg = 10, fp = 85)
# )
# 
# compare_ep_across_states(game_states, max_drives = 10, n = 15)
# 
# compare_ep_by_rate(1, 10, 63, 10, 5)
# 
# get_EP(1, 10, 63, 10, 5)
