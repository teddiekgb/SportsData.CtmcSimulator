# Load required packages
library(jsonlite)

SimulateInterval <- function(input, runs, until) {

  # build generator matrix
  input <- jsonlite::fromJSON(input)
  input.length <- length(input$gm)
  G.temp <- sapply(input, function (x) {length (x) <- input.length; return (x)})
  G <- matrix(unlist(G.temp, use.names=TRUE), ncol = input.length, byrow = TRUE, dimnames = list(dimnames(G.temp)[[1]], dimnames(G.temp)[[1]]))

  # create exit rate vector
  R <<- c(1:ncol(G))
  for (i in 1:ncol(G)) {
    R[i] <<- abs(G[i,i])
  }

  # create transition probability matrix
  P <<- sweep(G, 1, R, "/")
  P[P < 0] <<- 0

  totals.home <- c()
  totals.away <- c()
  totals <- c()

  # run simulations
  for (i in 1:runs) {

    # reset game clock and points tallies
    game.clock <- 0
    points.home <- 0
    points.away <- 0

    # starting state
    current.state <- 1

    # simulate steps through matrix until game clock expires
    while (game.clock <= until) {
      step <- SimulateStep(1, current.state)
      #game.clock <- game.clock + step$time.elapsed
      if (step$is.new.possession == 1) {
        game.clock <- game.clock + 1
      }
      points.home <- points.home + step$points.home
      points.away <- points.away + step$points.away
      current.state = step$current.state
    }

    # record results of this simulation
    totals.home <- c(totals.home, points.home)
    totals.away <- c(totals.away, points.away)
    totals <- c(totals, points.away+points.home)
  }

  game.lower <- qnorm(.40, mean = mean(totals), sd = sd(totals))
  game.upper <- (mean(totals) - game.lower) + mean(totals)
  return(jsonlite::toJSON(list("success" = 1, "reason" = "", "data" = list("game_lower" = game.lower, "game_upper" = game.upper, "game_mean" = mean(totals), "game_sd" = sd(totals), "home_mean" = mean(totals.home), "home_sd" = sd(totals.home), "away_mean" = mean(totals.away), "away_sd" = sd(totals.away))), auto_unbox = TRUE))

}

# function to simulate n steps of the transition matrix
SimulateStep = function(steps, current.state)
{
  points.away <- c()
  points.home <- c()
  is.new.possession <- 0

  # Make x steps through the markov chain
  for (i in 1:steps)
  {
    # cat("> Dist:", paste(round(c(trans[state,]), 2)), "\n");
    new.state <- sample(1:ncol(P), 1, prob=P[current.state,]);
    exit.time <- rexp(1, abs(R[current.state]));

    # check if step resulted in a new possession
    if (grepl('A', rownames(P)[current.state]) && grepl('B', rownames(P)[new.state])) {
      is.new.possession <- 1
    }
    if (grepl('B', rownames(P)[current.state]) && grepl('A', rownames(P)[new.state])) {
      is.new.possession <- 1
    }

    # check if new state resulted in points scored
    # if contains a number
    if (grepl('A', rownames(P)[new.state])) {
      if (grepl('1', rownames(P)[new.state])) {
        points.away <- c(points.away, 1)
      }
      if (grepl('2', rownames(P)[new.state])) {
        points.away <- c(points.away, 2)
      }
      if (grepl('3', rownames(P)[new.state])) {
        points.away <- c(points.away, 3)
      }
    }

    if (grepl('B', rownames(P)[new.state])) {
      if (grepl('1', rownames(P)[new.state])) {
        points.home <- c(points.home, 1)
      }
      if (grepl('2', rownames(P)[new.state])) {
        points.home <- c(points.home, 2)
      }
      if (grepl('3', rownames(P)[new.state])) {
        points.home <- c(points.home, 3)
      }
    }

    #cat("*", rownames(P)[state], "->", rownames(P)[newState], "in", exit_time, "seconds", "\n");

  }
  return(list("current.state" = new.state, "" = is.new.possession, "time.elapsed" = exit.time, "points.home" = if (!is.null(points.home)) sum(points.home) else 0, "points.away" = if (!is.null(points.away)) sum(points.away) else 0))
}
