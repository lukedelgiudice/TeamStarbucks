source("run_play.R")

run_drive <- function(D, YTG, FP, play_history = list()) {
  red_zone <- (100 - FP) <= 20
  
  new_state <- run_play(D, YTG, FP, red_zone, play_history)
  
  updated_history <- append(play_history, list(
    list(
      down = new_state$down,
      ytg = new_state$ytg,
      fp = new_state$fp,
      play_type = new_state$play_type,
      event = new_state$event
    )
  ))
  
  if (new_state$exit_drive == 0) {
    run_drive(new_state$down, new_state$ytg, new_state$fp, updated_history)
  } else {
    list(
      D = new_state$down,
      YTG = new_state$ytg,
      FP = new_state$fp,
      event = new_state$event,
      history = updated_history
    )
  }
}

classify_drive_tendency <- function(play_history) {
  if (length(play_history) < 3) return("neutral")
  
  n_plays <- min(5, length(play_history))
  weights <- rev(seq(0.5, 1, length.out = n_plays))
  play_types <- tail(sapply(play_history, function(x) x$play_type), n_plays)
  
  run_score <- sum(weights[play_types == "run"])/sum(weights)
  pass_score <- sum(weights[play_types == "pass"])/sum(weights)
  
  case_when(
    run_score > 0.65 ~ "run_heavy",
    pass_score > 0.65 ~ "pass_heavy",
    TRUE ~ "balanced"
  )
}

get_expected_play_type <- function(down, ytg, fp) {
  base_exp <- case_when(
    down == 3 && ytg > 5 ~ "pass",
    ytg <= 2 ~ "run",
    down == 4 ~ "pass",
    fp >= 75 ~ "pass",
    TRUE ~ "run"
  )
  
  if (fp >= 80) {
    if (ytg <= 3) return("run")
    return("pass_short")
  }
  base_exp
}



precompute_unexpected_distributions()

result <- run_drive(1, 10, 20)
print(result)

result <- run_drive(1, 10, 85)
print(result)


unexpected_data <- readRDS("unexpected_plays_data.rds")
ggplot(unexpected_data, aes(x = yards_gained)) +
  geom_density(aes(fill = play_type), alpha = 0.5) +
  facet_wrap(~unexpected_type)


