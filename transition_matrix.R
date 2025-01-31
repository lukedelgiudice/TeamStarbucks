
game_data <- readRDS("data.rds")


teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
N <- length(teams)
team_index <- setNames(1:N, teams)

# Create the empty matrix, each cell representing a matchup
loss_matrix <- matrix(0, nrow = N, ncol = N, dimnames = list(teams, teams))


for (i in 1:nrow(game_data)) {
  # Read in the visiting team, visiting score, home team, home score
  visiting_team <- game_data$Visiting_Team[i]
  home_team <- game_data$Home_Team[i]
  visiting_score <- game_data$Visiting_Score[i]
  home_score <- game_data$Home_Score[i]
  
  # If the home score is greater than the home score, add one to the the visiting team, home team cell
  if (visiting_score < home_score) {
    loss_matrix[team_index[visiting_team], team_index[home_team]] <- 
      loss_matrix[team_index[visiting_team], team_index[home_team]] + 1
  } 
  # If the visiting score is greater than the home score, add one to the the home team, visiting team cell
  else if (home_score < visiting_score) {
    loss_matrix[team_index[home_team], team_index[visiting_team]] <- 
      loss_matrix[team_index[home_team], team_index[visiting_team]] + 1
  }
}

# Make sure the rows sum to one
transition_matrix <- loss_matrix / rowSums(loss_matrix)

