\name{full.game.database}
\alias{full.game.database}
\alias{download.single.game}
\alias{download.games}

\title{Create a shell database for NHL games from 2002 to the present, then
  download them}
\description{
Creates a shell database for NHL games from 2002 to present, unfilled with
game information. Uses this to collect and scrape NHL data, but not
process them immediately.
}
\usage{
  full.game.database(extra.seasons=0)
  download.single.game (season="20122013",
                        gcode="20001",
                        rdata.folder="nhlr-data",
                        verbose=TRUE,
                        wait=20)
  download.games       (games=full.game.database(),
                        rdata.folder="nhlr-data",
                        ...)
}
\arguments{
  \item{extra.seasons}{Beyond 20142015, adds data for additional seasons
  (assuming nhlscrapr is not supported and updated before the season begins.}
  \item{season}{A character string for the two years specifying an NHL
    season.}
  \item{gcode}{The five-digit ID number for a particular NHL game.}
  \item{rdata.folder}{The location within the current directory to
  which to save the downloaded files. Will be created if it does not
  exist.}
  \item{verbose}{Report additional messages.}
  \item{wait}{Amount of time in seconds to wait between game downloads.}
  \item{games}{A game database, such as the one produced by
    full.game.database().}
  \item{...}{Arguments to pass to download.single.game().}
}
\details{
  full.game.database() gives ID numbers for all regular-season and
  playoff games played between 2002 and 2013, with indicators for
  whether any particular game is known to be unavailable.

  download.single.game() retrieves the relevant files for a single game
  from NHL.com.

  download.games() retrieves the files for all games in the table.
}
\value{
  full.game.database: a data frame with columns including season,
  session (Regular or Playoffs), game number/gcode (which game in the
  season?). Placeholders for teams, score and date of game are included
  to be filled in later. ``valid'' indicates whether a full record of
  the game is available for download.

  download.single.game: returns a single Boolean value indicating
  if no errors were recorded during the download. Saves the game to
  disk, particularly the PL, ES, SCH and SCV files, along with JSON data
  for x-y events.

  download.games: returns the input game database with ``valid'' changed
  to FALSE for any failed downloads.

}
\author{A.C. Thomas <act@acthomas.ca>}
\examples{

  #Select a part of the history.
  game.table <- full.game.database()[201:220,]

  #Download one game.
  download.single.game(game.table$season[1], game.table$gcode[1], wait=1)

  #Download all games.

\dontrun{
  game.table.updated <- download.games (games=game.table)
}
}

