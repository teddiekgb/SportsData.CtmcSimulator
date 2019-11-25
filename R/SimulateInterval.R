# Load required packages
# test comment
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
      game.clock <- game.clock + step$time.elapsed
      points.home <- points.home + step$points.home
      points.away <- points.away + step$points.away
      current.state = step$current.state
    }

    # record results of this simulation
    totals.home <- c(totals.home, points.home)
    totals.away <- c(totals.away, points.away)
    totals <- c(totals, points.away+points.home)
  }

  return(jsonlite::toJSON(list("success" = 1, "reason" = "", "data" = list("home_mean" = mean(totals.home), "home_sd" = sd(totals.home), "away_mean" = mean(totals.away), "away_sd" = sd(totals.away))), auto_unbox = TRUE))

}

# function to simulate n steps of the transition matrix
SimulateStep = function(steps, current.state)
{
  points.away <- c()
  points.home <- c()

  # Make x steps through the markov chain
  for (i in 1:steps)
  {
    # cat("> Dist:", paste(round(c(trans[state,]), 2)), "\n");
    new.state <- sample(1:ncol(P), 1, prob=P[current.state,]);
    exit.time <- rexp(1, abs(R[current.state]));
    # check if new state resulted in points scored

    # if contains _MADE
    if (grepl('_MADE', rownames(P)[new.state])) {
      # if contains _FT_ then add 1 point
      if (grepl('_FT_', rownames(P)[new.state])) {
        if (grepl('HOME_', rownames(P)[new.state])) {
          points.home <- c(points.home, 1)
        } else {
          points.away <- c(points.away, 1)
        }
      }
      # if contains _2PT_ then add 2 points
      if (grepl('_2PT_', rownames(P)[new.state])) {
        if (grepl('HOME_', rownames(P)[new.state])) {
          points.home <- c(points.home, 2)
        } else {
          points.away <- c(points.away, 2)
        }
      }
      # if contains _3PT_ then add 3 points
      if (grepl('_3PT_', rownames(P)[new.state])) {
        if (grepl('HOME_', rownames(P)[new.state])) {
          points.home <- c(points.home, 3)
        } else {
          points.away <- c(points.away, 3)
        }
      }
    }

    #cat("*", rownames(P)[state], "->", rownames(P)[newState], "in", exit_time, "seconds", "\n");

  }
  return(list("current.state" = new.state, "time.elapsed" = exit.time, "points.home" = if (!is.null(points.home)) sum(points.home) else 0, "points.away" = if (!is.null(points.away)) sum(points.away) else 0))
}
