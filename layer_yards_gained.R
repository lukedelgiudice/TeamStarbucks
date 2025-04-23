sample_yards_gained <- (function() {
  distributions_expected <- NULL
  distributions_unexpected <- NULL
  
  function(play_call, player_position, pass_length, red_zone, unexpected) {
    if (is.null(distributions_expected)) {
      distributions_expected <<- readRDS("yards_distributions.rds")
    }
    if (is.null(distributions_unexpected)) {
      distributions_unexpected <<- readRDS("yards_distributions_unexpected.rds")
    }
    
    distributions <- if (unexpected) {
      distributions_unexpected
    }
    else {
      distributions_expected
    }
    
    key <- if (play_call == "pass") {
      paste(play_call, player_position, pass_length, red_zone, unexpected, sep = "_")
    }
    else {
      paste(play_call, player_position, NA, red_zone, unexpected, sep = "_")
    }
    
    dist <- distributions[[key]]
    
    if (is.null(dist)) {
      return(round(rnorm(1, mean = 5, sd = 5)))
    }
    
    component <- sample(seq_along(dist$lambda), 1, prob = dist$lambda)
    yards <- rnorm(1, mean = dist$mu[component], sd = dist$sigma[component])
    return(round(max(0, yards)))
  }
})()