\name{process.games}
\alias{process.games}
\alias{process.single.game}
\alias{retrieve.game}
\alias{compile.all.games}
\title{Given downloaded NHL games, produce event tables with players on ice.}
\description{
  Produces game tables from the NHL source files.
}
\usage{
  process.single.game (season="20122013",
                       gcode="20001",
                       rdata.folder="nhlr-data",
                       override.download=FALSE,
                       save.to.file=TRUE,
                       ...)
  process.games       (games=full.game.database(),
                       rdata.folder="nhlr-data",
                       override.download=FALSE)
  retrieve.game       (season="20122013",
                       gcode="20001",
                       rdata.folder="nhlr-data",
                       force=TRUE,
                       ...)
  compile.all.games   (rdata.folder="nhlr-data",
                       output.folder="source-data",
                       new.game.table=NULL,
                       seasons=NULL,
                       verbose=FALSE,
                       override.days.back=NULL,
                       date.check=FALSE,
                       ...)

}
\arguments{
  \item{season}{A character string for the two years specifying an NHL
    season.}
  \item{gcode}{The five-digit ID number for a particular NHL game.}
  \item{rdata.folder}{The location within the current directory to
  which to save the downloaded files. Will be created if it does not
  exist.}
  \item{output.folder}{The location within the current directory where
    compiled files will be saved. Will be created if it does not
  exist.}
  \item{games, new.game.table}{A game database, such as the one produced by
    full.game.database().}
  \item{override.download}{Re-download the game files whether or not
    they are currently locally available.}
  \item{save.to.file}{Save the game object to file.}
  \item{force}{If TRUE, reprocesses the single game file.}
  \item{seasons}{If selected, defaults to a game table with only these
    seasons, of the form "20142015".}
  \item{date.check}{Try and download files without known dates.}
  \item{override.days.back}{Force download for those days previous to
    this one.}
  \item{verbose}{Print extra output information.}
  \item{...}{Arguments to pass to download.single.game().}
  
}
\details{
  This group of functions takes the downloaded HTML, image and JSON
  files and produces an event table for each game, particularly focusing
  on the players on the ice during each event. 
}
\value{
  process.single.game() produces an object called game.info that
  contains several pieces of information, primarily the data.frame
  playbyplay, with each column representing an event. Columns are as
  follows:
  event: The event number as recorded in the game.
  period, seconds: The period of the game and the elapsed time in seconds.
  a1,...,a6: The numbers and names of the away team players on the ice.
  h1,...,h6: The numbers and names of the home team players on the ice.
  ev.team: The team that registered the event in question.
  ev.player.1, ev.player.2, ev.player.3: The players involved in the event.
  distance: The distance in feet from the relevant goal for a shot on goal, missed shot or goal.
  type: Further information on the event, either shot type or penalty type.
  homezone: The zone in which the event took place, from the
    perspective of the home team.
  xcoord, ycoord: If available, the (x,y) location of a shot on
    goal or hit.

  process.games() runs this routine and saves all game files to disk.

  retrieve.game() retrieves the processed file from disk if it exists, and
  if not, it runs process.single.game() first.

  compile.all.games() does everything that's needed to construct the
	entire record. This will be fastest if all games have already been
	downloaded and processed, possibly in parallel.
}
\author{A.C. Thomas <act@acthomas.ca>}
