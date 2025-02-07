source("create_transition.R")

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read data and construct the transition matrix
game_data <- readRDS("data.rds")
transition_matrix <- construct(game_data, NULL)

# Number of teams
num_teams <- nrow(transition_matrix)


# SCENARIO 1: Overall Rankings (all years)

# Gold Coin Simulation (METHOD 1)
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

# Markov Steady-State (METHOD 2)

# Start with uniform distribution
b <- rep(1 / num_teams, num_teams)

for (i in 1:10000) {
  b <- b %*% transition_matrix
}

steady_state_ranking <- as.vector(b)
steady_state_results <- data.frame(Team = rownames(transition_matrix), PageRank_Score = steady_state_ranking)
steady_state_results <- steady_state_results %>% arrange(desc(PageRank_Score))

steady_state_results

# The steady-state vector (after 10,000 iterations -- and commented out as requested):
# steady_state_ranking


# SCENARIO 2: Seasonal Rankings
# **** QUESTION: how do team rankings change from season to season? ****

seasons <- sort(unique(game_data$season))
yearly_rankings <- list()

for (yr in seasons) {
  # Construct the transition matrix for the current season
  tm <- construct(game_data, years = yr)
  
  # Check the number of teams in this season (should be the same overall)
  num_teams_yr <- nrow(tm)
  
  # Compute the steady state using the iterative method
  b <- rep(1 / num_teams_yr, num_teams_yr)
  num_iterations <- 10000
  for (i in 1:num_iterations) {
    b <- b %*% tm
  }
  
  # Save the ranking for the current season
  ranking_yr <- data.frame(Team = rownames(tm), PageRank_Score = as.vector(b))
  ranking_yr <- ranking_yr[order(-ranking_yr$PageRank_Score), ]
  yearly_rankings[[as.character(yr)]] <- ranking_yr
}

# Combine rankings for plot
combined_rankings <- do.call(rbind, lapply(names(yearly_rankings), function(season_str) {
  df <- yearly_rankings[[season_str]]
  df <- df %>%
    arrange(desc(PageRank_Score)) %>% # descending order (best team at top)
    mutate(rank = row_number(), # assign rank based on ordering
           season = as.integer(season_str))
  return(df)
}))

head(combined_rankings)

# Create a plot where each team's ranking over the years is shown
p <- ggplot(combined_rankings, aes(x = season, y = rank, group = Team, color = Team)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_reverse(breaks = 1:max(combined_rankings$rank)) + # use scale_y_reverse() to have rank 1 at the top.
  labs(title = "Yearly MLB Team Rankings",
       x = "Season",
       y = "Ranking (1 = Best)") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

p

# This plot was difficult to interpret, so I took a more analytical approach:

# Get the mean/standard deviation/range of ranks for each team
team_fluctuations <- combined_rankings %>%
  group_by(Team) %>%
  summarise(mean_rank = mean(rank),
            sd_rank = sd(rank),
            min_rank = min(rank),
            max_rank = max(rank),
            range = max_rank - min_rank)

team_fluctuations

# Get the average yearly change in rankings for each team
rank_differences <- combined_rankings %>%
  arrange(Team, season) %>%
  group_by(Team) %>%
  mutate(lead_rank = lead(rank), # get the vector offset by 1 index for difference
         abs_diff = abs(lead_rank - rank)) %>%
  summarise(avg_change = mean(abs_diff, na.rm = TRUE))

rank_differences

# Merge these
team_summary <- team_fluctuations %>%
  inner_join(rank_differences, by = "Team")

team_summary

min(team_summary$avg_change)
max(team_summary$avg_change)
mean(team_summary$avg_change)

# Based on this new summary, it's clear that MLB teams fluctuate greatly from year to year,
# with as large as 10.6 ranks changed on average between seasons, and the mean average
# fluctuation being roughly 7 ranks changed between seasons across every team.

