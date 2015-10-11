
#load ("../../source-data/nhlscrapr-20142015.RData"); gamedata=grand.data

rushes.rebounds <- function (gamedata) {

    obrows <- nrow(gamedata)
    outvec <- rep("", obrows)
    allshots <- c("SHOT","GOAL","MISS","BLOCK")
    ongoals <- c("SHOT","GOAL")
    
    #rebound1 <-
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("SHOT") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 2)) > 0] <- "shotreb2"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("SHOT") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 3)) > 0] <- "shotreb3"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("SHOT") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 4)) > 0] <- "shotreb4"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("SHOT") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 5)) > 0] <- "shotreb5"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("SHOT") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 6)) > 0] <- "shotreb6"

    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("BLOCK") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 2)) > 0] <- "blockreb2"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("BLOCK") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 3)) > 0] <- "blockreb3"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("BLOCK") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 4)) > 0] <- "blockreb4"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("BLOCK") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 5)) > 0] <- "blockreb5"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("BLOCK") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 6)) > 0] <- "blockreb6"

    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("MISS") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 2)) > 0] <- "missreb2"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("MISS") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 3)) > 0] <- "missreb3"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("MISS") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 4)) > 0] <- "missreb4"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("MISS") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 5)) > 0] <- "missreb5"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   gamedata$ev.team[-1] == gamedata$ev.team[-obrows] &
                   gamedata$etype[-obrows] %in% c("MISS") &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] == 6)) > 0] <- "missreb6"

    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   ((gamedata$homezone[-1] == "Off" & gamedata$homezone[-obrows] == "Def") |
                    (gamedata$homezone[-1] == "Def" & gamedata$homezone[-obrows] == "Off")) &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 4)) > 0] <- "rusho4"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   ((gamedata$homezone[-1] == "Off" & gamedata$homezone[-obrows] == "Def") |
                    (gamedata$homezone[-1] == "Def" & gamedata$homezone[-obrows] == "Off")) &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] > 4 &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 8)) > 0] <- "rusho8"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   ((gamedata$homezone[-1] == "Off" & gamedata$homezone[-obrows] == "Def") |
                    (gamedata$homezone[-1] == "Def" & gamedata$homezone[-obrows] == "Off")) &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] > 8 &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 12)) > 0] <- "rusho12"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   ((gamedata$homezone[-1] == "Off" & gamedata$homezone[-obrows] == "Neu") |
                    (gamedata$homezone[-1] == "Def" & gamedata$homezone[-obrows] == "Neu")) &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 4)) > 0] <- "rushn4"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   ((gamedata$homezone[-1] == "Off" & gamedata$homezone[-obrows] == "Neu") |
                    (gamedata$homezone[-1] == "Def" & gamedata$homezone[-obrows] == "Neu")) &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] > 4 &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 8)) > 0] <- "rushn8"
    outvec[c(0, 1*(gamedata$etype[-1] %in% allshots &
                   ((gamedata$homezone[-1] == "Off" & gamedata$homezone[-obrows] == "Neu") |
                    (gamedata$homezone[-1] == "Def" & gamedata$homezone[-obrows] == "Neu")) &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] > 8 &
                   gamedata$seconds[-1] - gamedata$seconds[-obrows] <= 12)) > 0] <- "rushn12"

    return(outvec)
    
    
}

## Pulled goalies in events.

add.pulls <- function (gr.da) {

    home.pull <- which (gr.da$etype == "CHANGE" & gr.da$home.G > 1 & c(gr.da$home.G[-1], 0) == 1)
    gr.da$etype[home.pull] <- "PULL"
    gr.da$ev.team[home.pull] <- gr.da$hometeam[home.pull]
    
    away.pull <- which (gr.da$etype == "CHANGE" & gr.da$away.G > 1 & c(gr.da$away.G[-1], 0) == 1)
    gr.da$etype[away.pull] <- "PULL"
    gr.da$ev.team[away.pull] <- gr.da$awayteam[away.pull]
    return(gr.da)
    
}


## Back to backs.

add.fatigue <- function (games) {
    message ("Adding fatigue measurements to game table.")

    games$homeafterhome <- games$homeafteraway <- games$awayafterhome <- games$awayafteraway <- 0
    teams <- unique(c(games$hometeam, games$awayteam)); teams <- teams[nchar(teams)>0]
    lastgame <- rep(as.Date("2000-01-01"), length(teams))
    last.home <- rep(0, length(teams))
   
    for (kk in which(nchar(games$hometeam) > 0 & nchar(games$awayteam) > 0 & !is.na(games$date) & games$date != "")) {
        if (games$date[kk] == lastgame[which(teams == games$hometeam[kk])] + 1) {
            games$homeafterhome[kk] <- last.home[which(teams == games$hometeam[kk])]
            games$homeafteraway[kk] <- 1-last.home[which(teams == games$hometeam[kk])]
        }
        if (games$date[kk] == lastgame[which(teams == games$awayteam[kk])] + 1) {
            games$awayafterhome[kk] <- last.home[which(teams == games$awayteam[kk])]
            games$awayafteraway[kk] <- 1-last.home[which(teams == games$awayteam[kk])]
        }
        lastgame[which(teams == games$hometeam[kk])] <- lastgame[which(teams == games$awayteam[kk])] <- games$date[kk]
        last.home[which(teams == games$hometeam[kk])] <- 1
        last.home[which(teams == games$awayteam[kk])] <- 0
    }
    return(games)
    
}
