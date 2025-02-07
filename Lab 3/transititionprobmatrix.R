transition_matrix <- function(game_data) {
  teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
  n <- length(teams)
  loss_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(teams, teams))
  
  for (i in 1:nrow(game_data)) {
    if (gamedata$Visiting_Score >= game_data$Home_Score) {
      loss_matrix[game_data$Home_Team[i], game_data$Visiting_Team[i]] <- 
        loss_matrix[game_data$Home_Team[i], game_data$Visiting_Team[i]] + 1
    } 
    else {
      loss_matrix[game_data$Visiting_Team[i], game_data$Home_Team[i]] <- 
        loss_matrix[game_data$Visiting_Team[i], game_data$Home_Team[i]] + 1
    }
  }
}
