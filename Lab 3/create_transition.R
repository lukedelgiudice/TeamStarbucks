if (!require(wordspace)) {
  install.packages("wordspace", dependencies = TRUE)
  library(wordspace)
}

construct <- function(df, years = NULL) {
  # Filter by year if specified
  if (!is.null(years)) {
    df <- df[df$season %in% years, ]
  }
  
  # Initialize matrix and other variables
  teams <- unique(c(df$Visiting_Team, df$Home_Team))
  N <- length(teams)
  team_index <- setNames(1:N, teams)
  transition <- matrix(0, nrow = N, ncol = N, dimnames = list(teams, teams))
  
  # Populate transition matrix (loop through all games)
  for (i in 1:nrow(df)) {
    visiting_team <- df$Visiting_Team[i]
    home_team <- df$Home_Team[i]
    visiting_score <- df$Visiting_Score[i]
    home_score <- df$Home_Score[i]
    
    # Home team wins
    if (visiting_score < home_score) {
      transition[team_index[visiting_team], team_index[home_team]] <- 
        transition[team_index[visiting_team], team_index[home_team]] + 1
    } 
    
    # Away team wins
    else {
      transition[team_index[home_team], team_index[visiting_team]] <- 
        transition[team_index[home_team], team_index[visiting_team]] + 1
    }
  }
  
  # Normalize each row so that the entries sum to one
  row_sums <- rowSums(transition)
  transition <- sweep(transition, 1, row_sums, FUN = "/")
  
  # Replace any na values (if a team never lost) with 0
  transition[is.na(transition)] <- 0
  
  transition
}

