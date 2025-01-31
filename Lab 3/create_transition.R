
construct <- function(df, years) {
  teams <- 4 # fix this to parse for teams
  transition <- matrix(0, nrow = teams, ncol = teams)
  
  games <- readRDS("C:/Users/luked/Repositories/TeamStarbucks/data.rds")
  
  
  for (i in 1:nrow(games)) {
    # get team 1 and team 2
    games[i]
    
    if (p1 > p2) {
      transition[t1][t2] = transition[t1][t2] + 1
    }
    
  }
  
  # normalize
}