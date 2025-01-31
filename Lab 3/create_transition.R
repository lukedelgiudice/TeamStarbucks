if(!require(wordspace)){
  install.packages("wordspace", dependencies=TRUE)
  library(wordspace)
}

construct <- function(df, years) {
  games <- readRDS("C:/Users/luked/Repositories/TeamStarbucks/data.rds")
  
  teams <- unique(c(games$Visiting_Team, games$Home_Team))
  N <- length(teams)
  team_index <- setNames(1:N, teams)
  transition <- matrix(0, nrow = N, ncol = N, dimnames = list(teams, teams))
  
  for (i in 1:nrow(games)) {
    # get team 1 and team 2
    visiting_team <- games$Visiting_Team[i]
    home_team <- games$Home_Team[i]
    visiting_score <- games$Visiting_Score[i]
    home_score <- games$Home_Score[i]
    
    if (visiting_score < home_score) {
      transition[team_index[visiting_team], team_index[home_team]] <- 
        transition[team_index[visiting_team], team_index[home_team]] + 1
    }
    
    else {
      transition[team_index[home_team], team_index[visiting_team]] <- 
        transition[team_index[home_team], team_index[visiting_team]] + 1
    }
  }

  normalize.rows(transition)
}

