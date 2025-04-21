library(parallel)
library(ggplot2)

project_results <- function(max_drives = 10, n = 100, rates = seq(0, 0.2, by = 0.05)) {
  downs <- c(1, 2, 3)
  ytgs  <- c(5, 10)
  fps   <- seq(10, 90, by = 10)
  game_states_df <- expand.grid(down = downs, ytg  = ytgs, fp   = fps, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  game_states <- split(game_states_df, seq_len(nrow(game_states_df)))
  
  cores <- max(detectCores() - 1, 1)
  cl <- makeCluster(cores)
  
  clusterEvalQ(cl, {
    source("unexpected_plays.R")
    source("precompute_yards_distributions.R")
    source("precompute_yards_distributions_unexpected.R")
    source("run_epoch.R")
    source("get_EP.R")
  })
  
  clusterExport(cl, varlist = c("game_states", "max_drives", "n", "rates"), envir    = environment())
  
  tasks <- expand.grid(idx  = seq_along(game_states), rate = rates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  
  ep_list <- parLapply(cl, seq_len(nrow(tasks)), function(i) {
    state <- game_states[[ tasks$idx[i] ]]
    r     <- tasks$rate[i]
    assign("UNEXPECTED_RATE", r, envir = .GlobalEnv)
    get_EP(state$down, state$ytg, state$fp, max_drives, n)
  })
  
  stopCluster(cl)
  
  tasks$EP <- unlist(ep_list)
  
  results <- aggregate(EP ~ rate, data = tasks, FUN = mean)
  names(results) <- c("Rate", "EP")
  
  p <- ggplot(results, aes(x = Rate, y = EP)) +
    geom_line() +
    geom_point() +
    labs(title = "Average Expected Points vs UNEXPECTED_RATE",
         x     = "UNEXPECTED_RATE",
         y     = "Average EP") +
    theme_minimal()
  print(p)
  
  invisible(results)
}

results <- project_results(max_drives = 10, n = 100)
print(results)
