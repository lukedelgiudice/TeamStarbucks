library(dplyr)
library(purrr)

assign_player_position <- function(df, ref_data, probs) {
  df %>%
    mutate(
      player_position = case_when(
        play_type %in% c("qb_kneel", "qb_spike") ~ "qb",
        play_type == "run" ~ sample(
          probs$run$position, 
          n(), 
          replace = TRUE, 
          prob = probs$run$prob
        ),
        play_type == "pass" ~ {
          pass_cat <- case_when(
            !is.na(pass_length) ~ pass_length,
            !is.na(pass_location) ~ ifelse(pass_location == "middle", "middle", "sideline"),
            TRUE ~ "default"
          )
          
          map_chr(pass_cat, function(cat) {
            switch(cat,
                   "deep" = sample(names(probs$pass$deep), 1, prob = probs$pass$deep),
                   "short" = sample(names(probs$pass$short), 1, prob = probs$pass$short),
                   "middle" = sample(names(probs$pass$middle), 1, prob = probs$pass$middle),
                   "sideline" = sample(names(probs$pass$sideline), 1, prob = probs$pass$sideline),
                   sample(names(probs$pass$default), 1, prob = probs$pass$default))
          })
        },
        TRUE ~ "unknown"
      )
    )
}
