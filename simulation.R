source("get_EP.R")

project_results <- function(max_drives = 10, n = 100, rates = seq(0, 0.2, by = 0.05)) {
  downs <- c(1, 2, 3)
  ytgs <- c(5, 10)
  fps <- seq(10, 90, by = 10)
  
  game_states_df <- expand.grid(down = downs, ytg = ytgs, fp = fps)
  game_states <- split(game_states_df, 1:nrow(game_states_df))
  game_states <- lapply(game_states, as.list)
  
  results <- compare_ep_across_states(game_states, max_drives, n, rates)
  
  p <- ggplot(results, aes(x = Rate, y = EP)) +
    geom_line() +
    geom_point() +
    labs(title = "Average Expected Points vs UNEXPECTED_RATE", x = "UNEXPECTED_RATE", y = "Average EP") +
    theme_minimal()
  print(p)
  
  return(results)
}

results <- project_results(max_drives = 10, n = 100)
print(results)