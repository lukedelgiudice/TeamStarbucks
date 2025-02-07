transition_matrix <- function(game_data) {
  # Get list of unique teams
  teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
  n <- length(teams)
  
  # Initialize a loss matrix with zero counts
  loss_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(teams, teams))
  
  # Process each game (add a loss for the losing team against the winning team)
  for (i in 1:nrow(game_data)) {
    visiting_team <- game_data$Visiting_Team[i]
    home_team <- game_data$Home_Team[i]
    visiting_score <- game_data$Visiting_Score[i]
    home_score <- game_data$Home_Score[i]
    
    # Home team won
    if (visiting_score < home_score) {
      loss_matrix[visiting_team, home_team] <- loss_matrix[visiting_team, home_team] + 1
    } 
    
    # Away team won
    else {
      loss_matrix[home_team, visiting_team] <- loss_matrix[home_team, visiting_team] + 1
    }
  }
  
  # Get the row sums
  row_sums <- rowSums(loss_matrix)
  loss_matrix <- sweep(loss_matrix, 1, row_sums, FUN = "/")
  
  # Replace divisions by 0 with 0
  loss_matrix[is.na(loss_matrix)] <- 0
  
  loss_matrix
}

#game_data <- readRDS("data.rds")
#tra <- transition_matrix(game_data)
#print(tra)

