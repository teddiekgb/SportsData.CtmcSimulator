\name{SimulateInterval}
\alias{SimulateInterval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SimulateInterval
%%  ~~function to do ... ~~
}
\description{Performs multiple simulations of an NBA basketball game (or some custom time interval) based on a supplied continuous time Markov-chain generator matrix.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SimulateInterval(input, runs, until)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{A conitinuous time Markov-chain generator matrix.
%%     ~~Describe \code{x} here~~
}
  \item{runs}{Number of times to simulate the game.
%%     ~~Describe \code{x} here~~
}
  \item{until}{Amount of game clock deciseconds to run each game simulation for.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
