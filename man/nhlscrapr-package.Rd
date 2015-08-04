\name{nhlscrapr-package}
\alias{nhlscrapr-package}
\alias{nhlscrapr}
\docType{package}
\title{
  nhlscrapr: Retrieve and process play-by-play game data from the NHL
  Real Time Scoring System
}
\description{
  This package contains routines for extracting play-by-play game data
  for regular-season and playoff NHL games, particularly for analyses
  that depend on which players are on the ice. 
}
\details{
  This package processes data files of the type expanded summary (ES)
  and play-by-play (PL) for all possible games between 2002 and 2013,
  visual shift charts (SCH and SCV) for games between 2002 and
  2007, and shot locations from 2008 until present. The purpose of this
  routine is to produce tables for players,
  games and on-ice events specifically to look at the impact of a player's 
  contribution to how events unfold. The examples below illustrate how 
  these tools can be used to produce these tables. 

  The command process.games() will download the appropriate game files
  and, for each game selected produce a table of events with all players on the ice
  and relevant information for each particular event, such as shot distance and type, 
  goals and assists, and the zone in which the event took place (relative to the home 
  team). 

  The command augment.game() takes a single game file and makes it more useful for 
  the purpose of statistical modelling. It replaces player names with a unique player 
  ID number, separates goaltenders from skaters, and obtains the event interval time. 
  This routine is called for each game in the table by merge.to.mega.file() as it combines 
  all games into one single R object, saving this object to disk along with the game table 
  and unique player roster.

  A user only need run one command, compile.all.games(), in order to get
  everything from the site, though this may take hours to download and compile.

}
\author{
A.C. Thomas <act@acthomas.ca>
}
\keyword{ package }

\examples{

\dontrun{


  #What are all games that can/should be downloaded?
  #valid=FALSE implies previously screened problems.
  #Just a subset for testing.
  game.table <- full.game.database()[301:303,]

  #Get the details for these 20 games.
  print(game.table)

  #Takes HTML files and (possibly) GIF images and produces event and player tables for each game.
  process.games (game.table)

  #Give me a single game record.
  sample.game <- retrieve.game (season="20022003", gcode="20301")

  #Augment all games and put them all in one big database.
  compile.all.games (output.file="mynhlscrapes.RData")


  #################################################################
  # Extras:


  #Process games in parallel!
  library(doMC)
  registerDoMC(4)

  res <- foreach (kk=1:dim(game.table)[1]) \%dopar\%
    \{
      message (paste(kk, game.table[kk,1], game.table$gcode[kk]))
      item <- process.single.game(
                game.table[kk,1],
                game.table$gcode[kk],
                save.to.file=TRUE)
    \}
}

}
