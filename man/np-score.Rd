\name{NP.score}
\alias{NP.score}

\title{Calculate NP-tau score }
\description{
  Calculates the Net Probability score from a game table.
}
\usage{
  NP.score(grand.data, seconds=20)
}
\arguments{
  \item{grand.data}{A single data frame of all events to be considered.}
  \item{seconds}{The amount of time in seconds after the event with which we measure net goals.}
}

\value{
  A table containing the goal counts for each team following the particular events and zones, as well as the calculated net probability.
}
\author{A.C. Thomas <act@acthomas.ca>}
\references{Michael Schuckers and James Curro (2013) Total Hockey Rating (THoR): A comprehensive statistical rating of National Hockey League forwards and defensemen based upon all on-ice events. Sloan Sports Conference.
}

