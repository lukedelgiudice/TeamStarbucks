# Load necessary libraries
library(dplyr)

# Load the transition matrix from Part 1
source("create_transition.R")  # Ensure transition_matrix is loaded

# Number of teams
num_teams <- nrow(transition_matrix)

# Gold Coin Simulation
set.seed(42)  # Ensure reproducibility
num_steps <- 20000  # Total transitions
burn_in <- 1000  # Burn-in period

# Start with a randomly selected team
current_team <- sample(1:num_teams, 1)
coin_counts <- numeric(num_teams)

for (step in 1:num_steps) {
  # Choose next team based on transition probabilities
  current_team <- sample(1:num_teams, 1, prob = transition_matrix[current_team, ])
  
  # Only count occurrences after burn-in period
  if (step > burn_in) {
    coin_counts[current_team] <- coin_counts[current_team] + 1}
}

# Normalize the coin counts to get probabilities
gold_coin_ranking <- coin_counts / sum(coin_counts)

gold_coin_results <- data.frame(Team = rownames(transition_matrix), PageRank_Score = gold_coin_ranking)
gold_coin_results <- gold_coin_results %>% arrange(desc(PageRank_Score))

gold_coin_results