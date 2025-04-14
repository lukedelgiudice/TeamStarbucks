library(mclust)

sample_yards_gained <- function(play_call, player_position, red_zone, play_data, 
                                unexpected_type = NA) {
  if (!is.logical(red_zone)) {
    stop("red_zone parameter must be logical (TRUE/FALSE)")
  }
  
  filter_data <- function(data) {
    data %>%
      filter(!is.na(yards_gained)) %>%
      mutate(yards_gained = as.numeric(yards_gained)) %>%
      filter(yards_gained >= 0, yards_gained <= 99)
  }
  
  if (!is.na(unexpected_type)) {
    unexpected_data <- readRDS("unexpected_plays_data.rds")
    subsetData <- unexpected_data %>%
      filter(
        case_when(
          unexpected_type == "pass_deep" ~ play_call == "pass" & pass_length == "deep",
          unexpected_type == "run" ~ play_call == "run",
          TRUE ~ FALSE
        ),
        between(yardline_100, 20, 80)
      ) %>% 
      filter_data()
  } else {
    yard_filter <- if(red_zone) quote(yardline_100 <= 20) else quote(yardline_100 > 20)
    
    subsetData <- play_data %>%
      filter(play_call == !!play_call,
             player_position == !!player_position,
             !!yard_filter) %>%
      filter(fumble == 0)
    
    if (play_call == "pass") {
      subsetData <- subsetData %>%
        filter(interception == 0, incomplete_pass == 0)
    }
    
    subsetData <- filter_data(subsetData)
  }
  
  get_fallback <- function() {
    val <- case_when(
      play_call == "run" ~ pmax(0, round(rnorm(1, 4.1, 2.1))),
      play_call == "pass" ~ pmax(0, round(rnorm(1, 6.8, 4.6)))
    )
    
    ifelse(is.na(val), 0L, val)
  }
  
  if (nrow(subsetData) < 5) return(get_fallback())
  
  safe_mclust <- function(data) {
    fit <- try(Mclust(data$yards_gained, G = 1:3, verbose = FALSE), silent = TRUE)
    
    if (inherits(fit, "try-error") || any(fit$parameters$variance$sigmasq <= 0)) {
      m <- mean(data$yards_gained, na.rm = TRUE) %>% replace_na(4.1)
      s <- sd(data$yards_gained, na.rm = TRUE) %>% replace_na(3.2) %>% pmax(0.1)
      return(list(mean = m, sd = s))
    }
    
    list(
      mean = fit$parameters$mean,
      sd = sqrt(pmax(fit$parameters$variance$sigmasq, 0.1)),
      pro = fit$parameters$pro
    )
  }
  
  params <- safe_mclust(subsetData)
  
  if (!is.null(params$pro)) {
    comp <- sample(seq_along(params$mean), 1, prob = params$pro)
    yg <- round(rnorm(1, params$mean[comp], params$sd[comp]))
  } else {
    yg <- round(rnorm(1, params$mean, params$sd))
  }
  
  pmax(0, pmin(yg, 99))
}