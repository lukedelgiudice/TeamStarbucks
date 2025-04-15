# run_drive.R
source("run_play.R")

run_drive <- function(D, YTG, FP, play_history = list()) {
  # Determine red zone from yardline_100 = 100 - FP.
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

# Example runs:
result <- run_drive(1, 10, 20)
print(result)

result <- run_drive(1, 10, 85)
print(result)
