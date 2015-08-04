\name{nhlscrapr-data}
\alias{quadsarray}
\alias{pushpull}
\alias{pushes}
\alias{pulls}
\alias{zone.adjust.prefab}
\alias{date201415}


\title{nhlscrapr: Included Data Sets}
\description{
  Data sets included with the nhlscrapr package.
}
\usage{
  quadsarray
  pushes
  pulls
  zone.adjust.prefab
  date201415
}
\format{
  quadsarray: a series of quadrilaterals that define distinct areas of
  the offensive zone.
  pushes, pulls: the possible transitions from one area subzone to
  another, constrained to be distance lengthening (push) or shortening
  (pull).
  zone.adjust.prefab: When fixing the rink biases, this is used to
  assign the transition probabilities. This can be calculated by the
  user if desired.
  date201415: A predownloaded version of the timing of games for the
  2014-2015 season.
  
  None of these is ever required for the user.
}
\author{A.C. Thomas <act@acthomas.ca>}
\keyword{datasets}
