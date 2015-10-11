
## library(dplyr)
## load ("source-data/nhlscrapr-core.RData"); load ("source-data/nhlscrapr-20142015.RData");
## input: grand.data
produce.hexshots <- function (input.data) {
    ## input.data = grand.data

    ##scatter.grid <- point.grid()
    subdata <- mutate(input.data, gamestate = 1*(home.skaters == 6 & away.skaters == 6) +
                      2*(home.skaters > away.skaters) + 3*(home.skaters < away.skaters) +
                      4*(home.skaters == 5 & away.skaters == 5)) %>%
                          filter (home.G > 1,
                                  away.G > 1,
                                  gamestate > 0,
                                  gamestate < 5) %>% left_join (scatter.grid$xymap) %>% rename (hexblock = hexbin)
    
    ## Get game table time on ice. 
    total_timeonice_proto <- subdata %>% group_by (season, gcode, gamestate) %>% summarize (TTOI = sum(event.length))
    ttoi_away <- total_timeonice_proto %>% mutate(home = 0)
    ttoi_away$gamestate[ttoi_away$gamestate %in% 2:3] <- 5 - ttoi_away$gamestate[ttoi_away$gamestate %in% 2:3]
    ttoi <- rbind_list (total_timeonice_proto %>% mutate(home=1),
                        ttoi_away)

    toi.one <- rbind_list(group_by(subdata, season, gcode, gamestate, h1) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = h1) %>% mutate(home=1),
                          group_by(subdata, season, gcode, gamestate, h2) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = h2) %>% mutate(home=1),
                          group_by(subdata, season, gcode, gamestate, h3) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = h3) %>% mutate(home=1),
                          group_by(subdata, season, gcode, gamestate, h4) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = h4) %>% mutate(home=1),
                          group_by(subdata, season, gcode, gamestate, h5) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = h5) %>% mutate(home=1),
                          group_by(subdata, season, gcode, gamestate, h6) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = h6) %>% mutate(home=1),
                          group_by(subdata, season, gcode, gamestate, home.G) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = home.G) %>% mutate(home=1),
                          
                          group_by(subdata, season, gcode, gamestate, a1) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = a1) %>% mutate(home=0),
                          group_by(subdata, season, gcode, gamestate, a2) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = a2) %>% mutate(home=0),
                          group_by(subdata, season, gcode, gamestate, a3) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = a3) %>% mutate(home=0),
                          group_by(subdata, season, gcode, gamestate, a4) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = a4) %>% mutate(home=0),
                          group_by(subdata, season, gcode, gamestate, a5) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = a5) %>% mutate(home=0),
                          group_by(subdata, season, gcode, gamestate, a6) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = a6) %>% mutate(home=0),
                          group_by(subdata, season, gcode, gamestate, away.G) %>%
                          summarize (TOI = sum(event.length)) %>% ungroup %>% rename(ID = away.G) %>% mutate(home=0))
    
    toi.final <- toi.one %>% filter(ID > 1) %>% group_by (season, gcode, gamestate, home, ID) %>%
        summarize (TOI=sum(TOI))
    
    toi.final$gamestate[toi.final$gamestate %in% 2:3 & toi.final$home == 0] <- 5 - toi.final$gamestate[toi.final$gamestate %in% 2:3 & toi.final$home == 0]
    toi.final <- left_join (toi.final, ttoi) %>% mutate (TOI.off = TTOI - TOI)

    
    toi.team <- rbind_list(
        group_by(subdata, season, gcode, gamestate, awayteam) %>%
            summarize (TOI = sum(event.length)) %>% ungroup %>% rename(Team = awayteam) %>% mutate(home=0),
        group_by(subdata, season, gcode, gamestate, hometeam) %>%
            summarize (TOI = sum(event.length)) %>% ungroup %>% rename(Team = hometeam) %>% mutate(home=1))
    toi.team$gamestate[toi.team$gamestate %in% 2:3 & toi.team$home == 0] <- 
        5 - toi.team$gamestate[toi.team$gamestate %in% 2:3 & toi.team$home == 0]
    
    shotdata <- filter (subdata, !is.na(xcoord),
                        etype %in% c("GOAL","MISS","SHOT","BLOCK")) ## %>% mutate (hexblock = hexbin.quick (cbind(newyc, newxc), scatter.grid))
    shotdata$gamestate[shotdata$gamestate %in% 2:3 & shotdata$ev.team == shotdata$awayteam] <-
        5 - shotdata$gamestate[shotdata$gamestate %in% 2:3 & shotdata$ev.team == shotdata$awayteam]

    ## columns: newxc, new.loc.section
    ## roles: plus, shooter, minus

    sub.bit <- function (df, colname) {
        extract <- df[,c(colname, "season", "gcode", "seconds",

                         "etype",
                         "newxc", "newyc", "type", "new.loc.section","gamestate",
                         "hometeam", "awayteam",
                         "ev.team", "shot.feature", "hexblock")]
        colnames(extract)[1] <- "ID"
        extract
    }
    
    home.events <- filter(shotdata, ev.team==hometeam)
    away.events <- filter(shotdata, ev.team==awayteam)
    
    reduced.shots <- rbind_list (sub.bit(home.events, "h1") %>% mutate (home=1, role="O"),
                                 sub.bit(home.events, "h2") %>% mutate (home=1, role="O"),
                                 sub.bit(home.events, "h3") %>% mutate (home=1, role="O"),
                                 sub.bit(home.events, "h4") %>% mutate (home=1, role="O"),
                                 sub.bit(home.events, "h5") %>% mutate (home=1, role="O"),
                                 sub.bit(home.events, "h6") %>% mutate (home=1, role="O"),
                                 sub.bit(home.events, "home.G") %>% mutate (home=1, role="OG"),
                                 
                                 sub.bit(home.events, "a1") %>% mutate (home=0, role="D"),
                                 sub.bit(home.events, "a2") %>% mutate (home=0, role="D"),
                                 sub.bit(home.events, "a3") %>% mutate (home=0, role="D"),
                                 sub.bit(home.events, "a4") %>% mutate (home=0, role="D"),
                                 sub.bit(home.events, "a5") %>% mutate (home=0, role="D"),
                                 sub.bit(home.events, "a6") %>% mutate (home=0, role="D"),
                                 sub.bit(home.events, "away.G") %>% mutate (home=0, role="G"),
                                 
                                 sub.bit(home.events, "ev.player.1") %>% mutate (home=1, role="S"),
                                 
                                 sub.bit(away.events, "h1") %>% mutate (home=1, role="D"),
                                 sub.bit(away.events, "h2") %>% mutate (home=1, role="D"),
                                 sub.bit(away.events, "h3") %>% mutate (home=1, role="D"),
                                 sub.bit(away.events, "h4") %>% mutate (home=1, role="D"),
                                 sub.bit(away.events, "h5") %>% mutate (home=1, role="D"),
                                 sub.bit(away.events, "h6") %>% mutate (home=1, role="D"),
                                 sub.bit(away.events, "home.G") %>% mutate (home=1, role="G"),
                                 
                                 sub.bit(away.events, "a1") %>% mutate (home=0, role="O"),
                                 sub.bit(away.events, "a2") %>% mutate (home=0, role="O"),
                                 sub.bit(away.events, "a3") %>% mutate (home=0, role="O"),
                                 sub.bit(away.events, "a4") %>% mutate (home=0, role="O"),
                                 sub.bit(away.events, "a5") %>% mutate (home=0, role="O"),
                                 sub.bit(away.events, "a6") %>% mutate (home=0, role="O"),
                                 sub.bit(away.events, "away.G") %>% mutate (home=0, role="OG"),
                                 
                                 sub.bit(away.events, "ev.player.1") %>% mutate (home=0, role="S")
                                 
                                 ) %>% filter (ID > 1) %>% mutate (playerteam = ifelse (home == 1, hometeam, awayteam))

    


    sub.bit.team <- function (df, colname) {
        extract <- df[,c(colname, "season", "gcode", "seconds", "etype",
                         "newxc", "newyc", "type", "new.loc.section","gamestate",
                         "ev.team", "shot.feature", "hexblock")]
        colnames(extract)[1] <- "Team"
        extract
    }
    
    reduced.shots.team <- rbind_list (
        sub.bit.team(home.events, "hometeam") %>% mutate (home=1, role="O"),
        sub.bit.team(home.events, "awayteam") %>% mutate (home=0, role="D"),
        sub.bit.team(away.events, "hometeam") %>% mutate (home=1, role="D"),
        sub.bit.team(away.events, "awayteam") %>% mutate (home=0, role="O"))
    

    ## baseline measurements.
    baseline.counts <- group_by(shotdata, season, gamestate, new.loc.section) %>%
        summarize (BLOCK=sum(etype=="BLOCK"), SHOT=sum(etype=="SHOT"),
                   MISS=sum(etype=="MISS"), GOAL=sum(etype=="GOAL"))
    baseline.time <- group_by(subdata, season, gamestate) %>% summarize (TOI=sum(event.length))
    baseline.time$TOI[baseline.time$gamestate %in% c(1,4)] <- baseline.time$TOI[baseline.time$gamestate %in% c(1,4)]*2
    baseline.time$TOI[baseline.time$gamestate %in% 2:3] <- sum(baseline.time$TOI[baseline.time$gamestate %in% 2:3])
    baseline.data <- left_join (baseline.counts, baseline.time)

    ## ATLWPG, PHXARI
    swapout <- function (teamvec) {
        teamvec[teamvec %in% c("PHX","ARI")] <- "PHXARI"
        teamvec[teamvec %in% c("ATL","WPG")] <- "ATLWPG"
        teamvec
    }
    reduced.shots.team$Team <- swapout(reduced.shots.team$Team)
    toi.team$Team <- swapout(toi.team$Team)

    
    ## save (reduced.shots, file="reduced-hexshots.RData")
    return (list(shots=reduced.shots, TOI=toi.final,
                 shots.team=reduced.shots.team, TOI.team=toi.team,
                 baseline.data=baseline.data))

}









team.rosters.by.year <- function (season.pick=c("20052006","20062007","20072008","20082009", "20092010", "20102011",
                                      "20112012", "20122013", "20132014", "20142015"),
                                  sources="source-data/nhlscrapr-",
                                  output="common-data/hextally-roster-") {

    load (paste0(sources,"core.RData"))
    home.cols <- c("h1","h2","h3","h4","h5","h6", "home.G")
    away.cols <- c("a1","a2","a3","a4","a5","a6", "away.G")

    for (ss in 1:length(season.pick)) {

        message(season.pick[ss])
        load (paste0(sources, season.pick[ss],".RData"))
        
        teams <- sort(unique(c(grand.data$hometeam, grand.data$awayteam)))
        team.roster <- list()
        
        for (tt in 1:length(teams)) 
            team.roster[[tt]] <- 
                unique(c(as.matrix(grand.data[grand.data$hometeam==teams[tt], home.cols]),
                         as.matrix(grand.data[grand.data$awayteam==teams[tt], away.cols])))
        names(team.roster) <- teams

        save (team.roster, file=paste0(output,season.pick[ss],".RData"))
    }
}


get.shots.players <- function (season.pick=c("20052006","20062007","20072008","20082009", "20092010", "20102011",
                                   "20112012", "20122013", "20132014", "20142015"),
                               sources="source-data/nhlscrapr-",
                               output="common-data/hextally-players-",
                               do.es=TRUE, do.pp=TRUE, do.four=TRUE) {

    load (paste0(sources,"core.RData"))
    home.cols <- c("h1","h2","h3","h4","h5","h6")
    away.cols <- c("a1","a2","a3","a4","a5","a6")
    player.cols <- c(home.cols, away.cols)
    
    es.shots.prep <- function (datablock,
                               playerblock) {
    ## datablock = subset(grand.data, season %in% season.pick & home.skaters==6 & away.skaters==6 & home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"); playerblock=subset(grand.data, season %in% season.pick & substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)]
    
  ## get time on ice for each player in each season.
        
        time.off.ice.home <- time.off.ice.away <-
            time.on.ice.home <- time.on.ice.away <-
                matrix(0, nrow=nrow(roster.unique), ncol=length(season.pick))
        
                                        # games
        home.played <- away.played <- list(); away.played[[nrow(roster.unique)+1]] <- 1; home.played[[nrow(roster.unique)+1]] <- 1
        
        total.time.by.game <- matrix(0, nrow=length(unique(datablock$gcode)), ncol=length(season.pick))
        rownames(total.time.by.game) <- as.character(unique(paste0(datablock$season, datablock$gcode)))
        
        for (ss in 1:length(season.pick)) {
            ss.reduced <- subset(datablock, season==season.pick[ss])
            pl.reduced <- subset(playerblock, season==season.pick[ss])
            
            fullgames <- by(ss.reduced, paste0(ss.reduced$season, ss.reduced$gcode), identity)
            game.players <- by(pl.reduced, paste0(pl.reduced$season, pl.reduced$gcode), identity)
            
            t1 <- sapply (fullgames, function(gg) {
                
                thisone <- paste0(season.pick[ss], gg$gcode[1])
                ##cat(as.character(gg$gcode[1]), " ")
                ggplcol <- as.matrix(gg[,player.cols])
                gghomecol <- as.matrix(gg[,home.cols])
                ggawaycol <- as.matrix(gg[,away.cols])
                
                obj <- game.players[[which(names(game.players) == paste0(gg$season[1], gg$gcode[1]))]]
                
                home.players <- unique(c(as.matrix(obj[,home.cols])))
                for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
                
                away.players <- unique(c(as.matrix(obj[,away.cols])))
                for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)
                
                time.on.home <- sapply(home.players, function(pp) sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
                time.on.away <- sapply(away.players, function(pp) sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
                
                time.off.home <- sum(gg$event.length) - time.on.home
                time.off.away <- sum(gg$event.length) - time.on.away
                
                time.on.ice.home[home.players,ss] <<- time.on.ice.home[home.players,ss] + time.on.home
                time.on.ice.away[away.players,ss] <<- time.on.ice.away[away.players,ss] + time.on.away
                
                time.off.ice.home[home.players,ss] <<- time.off.ice.home[home.players,ss] + time.off.home
                time.off.ice.away[away.players,ss] <<- time.off.ice.away[away.players,ss] + time.off.away
            })
            
            time.by.game <- tapply (ss.reduced$event.length, as.character(paste0(ss.reduced$season, ss.reduced$gcode)), sum)
            total.time.by.game[match(names(time.by.game), rownames(total.time.by.game)), ss] <- time.by.game
        }
        
        shots <- subset(datablock, !is.na(xcoord) & etype %in% c("GOAL","MISS","SHOT","BLOCK"))
        flips <- which(shots$xcoord<0)
        shots$xcoord[flips] <- -shots$xcoord[flips]; shots$ycoord[flips] <- -shots$ycoord[flips]
        flips <- which(shots$newxc<0)
        shots$newxc[flips] <- -shots$newxc[flips]; shots$newyc[flips] <- -shots$newyc[flips]
        
        current.seasons.es <- shots[,c("season", "gcode", "period", "etype", "ev.team", "ev.player.1",
                                       "type", "xcoord", "ycoord", "loc.section", "awayteam", "hometeam",
                                       "newxc", "newyc", "new.loc.section",
                                       "away.score", "home.score",
                                       "away.G", "home.G", "home.skaters", "away.skaters", player.cols)]
        scatter.grid <- point.grid()
        
        seasonal.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set, scatter.grid)
        seasonal.bin.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set.blocks, scatter.grid)
        seasonal.superbin.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set.blocks, scatter.grid, use.superblocks=TRUE)
        
        quants <- list(shots=current.seasons.es,
                       
                       time.on.ice.home.O=time.on.ice.home,
                       time.on.ice.home.D=time.on.ice.home,
                       time.on.ice.away.O=time.on.ice.away,
                       time.on.ice.away.D=time.on.ice.away,
                       
                       time.off.ice.home.O=time.off.ice.home,
                       time.off.ice.home.D=time.off.ice.home,
                       time.off.ice.away.O=time.off.ice.away,
                       time.off.ice.away.D=time.off.ice.away,
                       
                       total.time.by.game.home.O=total.time.by.game,
                       total.time.by.game.away.O=total.time.by.game,
                       
                       home.played=home.played,
                       away.played=away.played,
                       
                       scatter.grid=scatter.grid,
                       
                       seasonal.counts=seasonal.counts,
                       seasonal.bin.counts=seasonal.bin.counts,
                       seasonal.superbin.counts=seasonal.superbin.counts
                       )
    
        return(quants)
    }
    
    
    pp.shots.prep <- function (datablock,
                               playerblock) {
        
        datablock.home <- subset(datablock, home.skaters > away.skaters)
        datablock.away <- subset(datablock, home.skaters < away.skaters)
        
        time.off.ice.home.O <- time.off.ice.away.O <- time.on.ice.home.O <- time.on.ice.away.O <-
            time.off.ice.home.D <- time.off.ice.away.D <- time.on.ice.home.D <- time.on.ice.away.D <-
                matrix(0, nrow=nrow(roster.unique), ncol=length(season.pick))
                                        # games
        home.played <- away.played <- list(); away.played[[nrow(roster.unique)+1]] <- 1; home.played[[nrow(roster.unique)+1]] <- 1
        
        total.time.by.game.home.O <- matrix(0, nrow=length(unique(paste0(datablock$season, datablock$gcode))), ncol=length(season.pick))
        rownames(total.time.by.game.home.O) <- as.character(unique(paste0(datablock$season, datablock$gcode)))
        total.time.by.game.away.O <- total.time.by.game.home.O
        
        for (ss in 1:length(season.pick)) {
            pl.reduced <- subset(playerblock, season==season.pick[ss])
            game.players <- by(pl.reduced, paste0(pl.reduced$season, pl.reduced$gcode), identity)
            
            ss.reduced.home <- subset(datablock.home, season==season.pick[ss])
            ss.reduced.away <- subset(datablock.away, season==season.pick[ss])
            
            t1 <- by (ss.reduced.home, paste0(ss.reduced.home$season, ss.reduced.home$gcode), function(gg) {
                
                thisone <- paste0(season.pick[ss], gg$gcode[1])
                ##cat(as.character(gg$gcode[1]), " ")
                ggplcol <- as.matrix(gg[,player.cols])
                gghomecol <- as.matrix(gg[,home.cols])
                ggawaycol <- as.matrix(gg[,away.cols])
                
                obj <- game.players[[which(names(game.players) == paste0(gg$season[1], gg$gcode[1]))]]
                home.players <- unique(c(as.matrix(obj[,home.cols])))
                for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
                away.players <- unique(c(as.matrix(obj[,away.cols])))
                for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)
                
                time.on.home <- sapply(home.players, function(pp) sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
                time.on.away <- sapply(away.players, function(pp) sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
                
                time.off.home <- sum(gg$event.length) - time.on.home  #.O
                time.off.away <- sum(gg$event.length) - time.on.away  #.D
                
                time.on.ice.home.O[home.players,ss] <<- time.on.ice.home.O[home.players,ss] + time.on.home
                time.on.ice.away.D[away.players,ss] <<- time.on.ice.away.D[away.players,ss] + time.on.away
                
                time.off.ice.home.O[home.players,ss] <<- time.off.ice.home.O[home.players,ss] + time.off.home
                time.off.ice.away.D[away.players,ss] <<- time.off.ice.away.D[away.players,ss] + time.off.away
                
            })
            time.by.game <- tapply (ss.reduced.home$event.length, as.character(paste0(ss.reduced.home$season, ss.reduced.home$gcode)), sum)
            total.time.by.game.home.O[match(names(time.by.game), rownames(total.time.by.game.home.O)), ss] <- time.by.game
            
            t1 <- by (ss.reduced.away, paste0(ss.reduced.away$season, ss.reduced.away$gcode), function(gg) {
                ##gg <- subset(ss.reduced.away, gcode == "20001")
                thisone <- paste0(season.pick[ss], gg$gcode[1])
                ##cat(as.character(gg$gcode[1]), " ")
                ggplcol <- as.matrix(gg[,player.cols])
                gghomecol <- as.matrix(gg[,home.cols])
                ggawaycol <- as.matrix(gg[,away.cols])
                
                obj <- game.players[[which(names(game.players) == paste0(gg$season[1], gg$gcode[1]))]]
                home.players <- unique(c(as.matrix(obj[,home.cols])))
                for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
                away.players <- unique(c(as.matrix(obj[,away.cols])))
                for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)
                
                time.on.home <- sapply(home.players, function(pp)
                                       sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
                time.on.away <- sapply(away.players, function(pp)
                                       sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
                
                time.off.home <- sum(gg$event.length) - time.on.home
                time.off.away <- sum(gg$event.length) - time.on.away
                
                time.on.ice.home.D[home.players,ss] <<- time.on.ice.home.D[home.players,ss] + time.on.home
                time.on.ice.away.O[away.players,ss] <<- time.on.ice.away.O[away.players,ss] + time.on.away
                
                time.off.ice.home.D[home.players,ss] <<- time.off.ice.home.D[home.players,ss] + time.off.home
                time.off.ice.away.O[away.players,ss] <<- time.off.ice.away.O[away.players,ss] + time.off.away
                
            })
            time.by.game <- tapply (ss.reduced.away$event.length, as.character(paste0(ss.reduced.away$season, ss.reduced.away$gcode)), sum)
            total.time.by.game.away.O[match(names(time.by.game), rownames(total.time.by.game.away.O)), ss] <- time.by.game
            
        }
        
        shots <- subset(datablock, !is.na(xcoord) & etype %in% c("GOAL","MISS","SHOT","BLOCK"))
        flips <- which(shots$xcoord<0)
        shots$xcoord[flips] <- -shots$xcoord[flips]; shots$ycoord[flips] <- -shots$ycoord[flips]
        flips <- which(shots$newxc<0)
        shots$newxc[flips] <- -shots$newxc[flips]; shots$newyc[flips] <- -shots$newyc[flips]
        
        
        current.seasons.pp <- shots[,c("season", "gcode", "period", "etype", "ev.team", "ev.player.1",
                                       "type", "xcoord", "ycoord", "loc.section", "awayteam", "hometeam",
                                       "newxc", "newyc", "new.loc.section",
                                       "away.score", "home.score",
                                       "away.G", "home.G", "home.skaters", "away.skaters", player.cols)]
        scatter.grid <- point.grid()
        
        seasonal.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season),
                              shot.bin.set, scatter.grid, c("newyc","newxc"))
        seasonal.bin.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season),
                                  shot.bin.set.blocks, scatter.grid, c("newyc","newxc"))
        seasonal.superbin.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season), shot.bin.set.blocks, scatter.grid, c("newyc","newxc"), use.superblocks=TRUE)
        
        quants <- list(shots=current.seasons.pp,
                       
                       time.on.ice.home.O=time.on.ice.home.O,
                       time.on.ice.home.D=time.on.ice.home.D,
                       time.on.ice.away.O=time.on.ice.away.O,
                       time.on.ice.away.D=time.on.ice.away.D,
                       
                       time.off.ice.home.O=time.off.ice.home.O,
                       time.off.ice.home.D=time.off.ice.home.D,
                       time.off.ice.away.O=time.off.ice.away.O,
                       time.off.ice.away.D=time.off.ice.away.D,
                       
                       total.time.by.game.home.O=total.time.by.game.home.O,
                       total.time.by.game.away.O=total.time.by.game.away.O,
                   
                       home.played=home.played,
                       away.played=away.played,
                       
                       scatter.grid=scatter.grid,
                       
                       seasonal.counts=seasonal.counts,
                       seasonal.bin.counts=seasonal.bin.counts,
                       seasonal.superbin.counts=seasonal.superbin.counts
                       
                      #,overall.counts=overall.counts
                      #,overall.bin.counts=overall.bin.counts
                       )
    
        return(quants)
    }

    for (ss in 1:length(season.pick)) {
        
        message("Hextally players ",season.pick[ss])
        load (paste0(sources, season.pick[ss],".RData"))
        
        if (do.es) {
            message ("Even strength")
            es.quants.s <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                                home.skaters==6 & away.skaters==6 &
                                                home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                         subset(grand.data, #season %in% season.pick &
                                                substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])
        
            es.quants.p <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                                home.skaters==6 & away.skaters==6 &
                                                home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                         subset(grand.data, #season %in% season.pick &
                                                substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
        } else {es.quants.s <- es.quants.p <- NULL}
        
        if (do.four) {
            message ("Four on four")
            four.quants.s <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                                  home.skaters==5 & away.skaters==5 &
                                                  home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                           subset(grand.data, #season %in% season.pick &
                                                  substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])
            
            four.quants.p <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                                  home.skaters==5 & away.skaters==5 &
                                                  home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                           subset(grand.data, #season %in% season.pick &
                                                  substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
        } else {four.quants.s <- four.quants.p <- NULL}

        if (do.pp) {
            message ("Power play")
            pp.quants.s <- pp.shots.prep(subset(grand.data, #season %in% season.pick &
                                                ((home.skaters > away.skaters) |
                                                 (home.skaters < away.skaters)) &
                                                home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                         subset(grand.data, #season %in% season.pick &
                                                substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])
    
            pp.quants.p <- pp.shots.prep(subset(grand.data, #season %in% season.pick &
                                                ((home.skaters > away.skaters) |
                                                 (home.skaters < away.skaters)) &
                                                home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                         subset(grand.data, #season %in% season.pick &
                                                substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
        } else {pp.quants.s <- pp.quants.p <- NULL}
        
        save(pp.quants.s, pp.quants.p,
             es.quants.s, es.quants.p,
             four.quants.s, four.quants.p,
             file=paste0(output,season.pick[ss],".RData"))

    }
    
}


###############################################################################


get.shots.goalies <- function (season.pick=c("20052006","20062007","20072008","20082009", "20092010", "20102011",
                                   "20112012", "20122013", "20132014", "20142015"),
                               sources="source-data/nhlscrapr-",
                               output="common-data/hextally-goalies-",
                               do.es=TRUE, do.pp=TRUE, do.four=TRUE) {

    load (paste0(sources,"core.RData"))
    home.cols <- c("home.G")
    away.cols <- c("away.G")
    player.cols <- c(home.cols, away.cols)
    
    es.shots.prep <- function (datablock,
                               playerblock) {
    ## datablock = subset(grand.data, season %in% season.pick & home.skaters==6 & away.skaters==6 & home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"); playerblock=subset(grand.data, season %in% season.pick & substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)]
    
  ## get time on ice for each player in each season.
        
        time.off.ice.home <- time.off.ice.away <-
            time.on.ice.home <- time.on.ice.away <-
                matrix(0, nrow=nrow(roster.unique), ncol=length(season.pick))
        
                                        # games
        home.played <- away.played <- list(); away.played[[nrow(roster.unique)+1]] <- 1; home.played[[nrow(roster.unique)+1]] <- 1
        
        total.time.by.game <- matrix(0, nrow=length(unique(datablock$gcode)), ncol=length(season.pick))
        rownames(total.time.by.game) <- as.character(unique(paste0(datablock$season, datablock$gcode)))
        
        for (ss in 1:length(season.pick)) {
            ss.reduced <- subset(datablock, season==season.pick[ss])
            pl.reduced <- subset(playerblock, season==season.pick[ss])
            
            fullgames <- by(ss.reduced, paste0(ss.reduced$season, ss.reduced$gcode), identity)
            game.players <- by(pl.reduced, paste0(pl.reduced$season, pl.reduced$gcode), identity)
            
            t1 <- sapply (fullgames, function(gg) {
                
                thisone <- paste0(season.pick[ss], gg$gcode[1])
                ##cat(as.character(gg$gcode[1]), " ")
                ggplcol <- as.matrix(gg[,player.cols])
                gghomecol <- as.matrix(gg[,home.cols])
                ggawaycol <- as.matrix(gg[,away.cols])
                
                obj <- game.players[[which(names(game.players) == paste0(gg$season[1], gg$gcode[1]))]]
                
                home.players <- unique(c(as.matrix(obj[,home.cols])))
                for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
                
                away.players <- unique(c(as.matrix(obj[,away.cols])))
                for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)
                
                time.on.home <- sapply(home.players, function(pp) sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
                time.on.away <- sapply(away.players, function(pp) sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
                
                time.off.home <- sum(gg$event.length) - time.on.home
                time.off.away <- sum(gg$event.length) - time.on.away
                
                time.on.ice.home[home.players,ss] <<- time.on.ice.home[home.players,ss] + time.on.home
                time.on.ice.away[away.players,ss] <<- time.on.ice.away[away.players,ss] + time.on.away
                
                time.off.ice.home[home.players,ss] <<- time.off.ice.home[home.players,ss] + time.off.home
                time.off.ice.away[away.players,ss] <<- time.off.ice.away[away.players,ss] + time.off.away
            })
            
            time.by.game <- tapply (ss.reduced$event.length, as.character(paste0(ss.reduced$season, ss.reduced$gcode)), sum)
            total.time.by.game[match(names(time.by.game), rownames(total.time.by.game)), ss] <- time.by.game
        }
        
        shots <- subset(datablock, !is.na(xcoord) & etype %in% c("GOAL","SHOT","MISS","BLOCK"))
        flips <- which(shots$xcoord<0)
        shots$xcoord[flips] <- -shots$xcoord[flips]; shots$ycoord[flips] <- -shots$ycoord[flips]
        flips <- which(shots$newxc<0)
        shots$newxc[flips] <- -shots$newxc[flips]; shots$newyc[flips] <- -shots$newyc[flips]
        
        current.seasons.es <- shots[,c("season", "gcode", "period", "etype", "ev.team", "ev.player.1",
                                       "type", "xcoord", "ycoord", "loc.section", "awayteam", "hometeam",
                                       "newxc", "newyc", "new.loc.section",
                                       "away.score", "home.score",
                                       "away.G", "home.G", "home.skaters", "away.skaters", player.cols)]
        scatter.grid <- point.grid()
        
        seasonal.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set, scatter.grid)
        seasonal.bin.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set.blocks, scatter.grid)
        seasonal.superbin.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set.blocks, scatter.grid, use.superblocks=TRUE)
        
        quants <- list(shots=current.seasons.es,
                       
                       time.on.ice.home.O=time.on.ice.home,
                       time.on.ice.home.D=time.on.ice.home,
                       time.on.ice.away.O=time.on.ice.away,
                       time.on.ice.away.D=time.on.ice.away,
                       
                       time.off.ice.home.O=time.off.ice.home,
                       time.off.ice.home.D=time.off.ice.home,
                       time.off.ice.away.O=time.off.ice.away,
                       time.off.ice.away.D=time.off.ice.away,
                       
                       total.time.by.game.home.O=total.time.by.game,
                       total.time.by.game.away.O=total.time.by.game,
                       
                       home.played=home.played,
                       away.played=away.played,
                       
                       scatter.grid=scatter.grid,
                       
                       seasonal.counts=seasonal.counts,
                       seasonal.bin.counts=seasonal.bin.counts,
                       seasonal.superbin.counts=seasonal.superbin.counts
                       )
    
        return(quants)
    }
    
    
    pp.shots.prep <- function (datablock,
                               playerblock) {
        
        datablock.home <- subset(datablock, home.skaters > away.skaters)
        datablock.away <- subset(datablock, home.skaters < away.skaters)
        
        time.off.ice.home.O <- time.off.ice.away.O <- time.on.ice.home.O <- time.on.ice.away.O <-
            time.off.ice.home.D <- time.off.ice.away.D <- time.on.ice.home.D <- time.on.ice.away.D <-
                matrix(0, nrow=nrow(roster.unique), ncol=length(season.pick))
                                        # games
        home.played <- away.played <- list(); away.played[[nrow(roster.unique)+1]] <- 1; home.played[[nrow(roster.unique)+1]] <- 1
        
        total.time.by.game.home.O <- matrix(0, nrow=length(unique(paste0(datablock$season, datablock$gcode))), ncol=length(season.pick))
        rownames(total.time.by.game.home.O) <- as.character(unique(paste0(datablock$season, datablock$gcode)))
        total.time.by.game.away.O <- total.time.by.game.home.O
        
        for (ss in 1:length(season.pick)) {
            pl.reduced <- subset(playerblock, season==season.pick[ss])
            game.players <- by(pl.reduced, paste0(pl.reduced$season, pl.reduced$gcode), identity)
            
            ss.reduced.home <- subset(datablock.home, season==season.pick[ss])
            ss.reduced.away <- subset(datablock.away, season==season.pick[ss])
            
            t1 <- by (ss.reduced.home, paste0(ss.reduced.home$season, ss.reduced.home$gcode), function(gg) {
                
                thisone <- paste0(season.pick[ss], gg$gcode[1])
                ##cat(as.character(gg$gcode[1]), " ")
                ggplcol <- as.matrix(gg[,player.cols])
                gghomecol <- as.matrix(gg[,home.cols])
                ggawaycol <- as.matrix(gg[,away.cols])
                
                obj <- game.players[[which(names(game.players) == paste0(gg$season[1], gg$gcode[1]))]]
                home.players <- unique(c(as.matrix(obj[,home.cols])))
                for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
                away.players <- unique(c(as.matrix(obj[,away.cols])))
                for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)
                
                time.on.home <- sapply(home.players, function(pp) sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
                time.on.away <- sapply(away.players, function(pp) sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
                
                time.off.home <- sum(gg$event.length) - time.on.home  #.O
                time.off.away <- sum(gg$event.length) - time.on.away  #.D
                
                time.on.ice.home.O[home.players,ss] <<- time.on.ice.home.O[home.players,ss] + time.on.home
                time.on.ice.away.D[away.players,ss] <<- time.on.ice.away.D[away.players,ss] + time.on.away
                
                time.off.ice.home.O[home.players,ss] <<- time.off.ice.home.O[home.players,ss] + time.off.home
                time.off.ice.away.D[away.players,ss] <<- time.off.ice.away.D[away.players,ss] + time.off.away
                
            })
            time.by.game <- tapply (ss.reduced.home$event.length, as.character(paste0(ss.reduced.home$season, ss.reduced.home$gcode)), sum)
            total.time.by.game.home.O[match(names(time.by.game), rownames(total.time.by.game.home.O)), ss] <- time.by.game
            
            t1 <- by (ss.reduced.away, paste0(ss.reduced.away$season, ss.reduced.away$gcode), function(gg) {
                ##gg <- subset(ss.reduced.away, gcode == "20001")
                thisone <- paste0(season.pick[ss], gg$gcode[1])
                ##cat(as.character(gg$gcode[1]), " ")
                ggplcol <- as.matrix(gg[,player.cols])
                gghomecol <- as.matrix(gg[,home.cols])
                ggawaycol <- as.matrix(gg[,away.cols])
                
                obj <- game.players[[which(names(game.players) == paste0(gg$season[1], gg$gcode[1]))]]
                home.players <- unique(c(as.matrix(obj[,home.cols])))
                for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
                away.players <- unique(c(as.matrix(obj[,away.cols])))
                for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)
                
                time.on.home <- sapply(home.players, function(pp)
                                       sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
                time.on.away <- sapply(away.players, function(pp)
                                       sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
                
                time.off.home <- sum(gg$event.length) - time.on.home
                time.off.away <- sum(gg$event.length) - time.on.away
                
                time.on.ice.home.D[home.players,ss] <<- time.on.ice.home.D[home.players,ss] + time.on.home
                time.on.ice.away.O[away.players,ss] <<- time.on.ice.away.O[away.players,ss] + time.on.away
                
                time.off.ice.home.D[home.players,ss] <<- time.off.ice.home.D[home.players,ss] + time.off.home
                time.off.ice.away.O[away.players,ss] <<- time.off.ice.away.O[away.players,ss] + time.off.away
                
            })
            time.by.game <- tapply (ss.reduced.away$event.length, as.character(paste0(ss.reduced.away$season, ss.reduced.away$gcode)), sum)
            total.time.by.game.away.O[match(names(time.by.game), rownames(total.time.by.game.away.O)), ss] <- time.by.game
            
        }
        
        shots <- subset(datablock, !is.na(xcoord) & etype %in% c("GOAL","SHOT","MISS","BLOCK"))
        flips <- which(shots$xcoord<0)
        shots$xcoord[flips] <- -shots$xcoord[flips]; shots$ycoord[flips] <- -shots$ycoord[flips]
        flips <- which(shots$newxc<0)
        shots$newxc[flips] <- -shots$newxc[flips]; shots$newyc[flips] <- -shots$newyc[flips]
        
        
        current.seasons.pp <- shots[,c("season", "gcode", "period", "etype", "ev.team", "ev.player.1",
                                       "type", "xcoord", "ycoord", "loc.section", "awayteam", "hometeam",
                                       "newxc", "newyc", "new.loc.section",
                                       "away.score", "home.score",
                                       "away.G", "home.G", "home.skaters", "away.skaters", player.cols)]
        scatter.grid <- point.grid()
        
        seasonal.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season),
                              shot.bin.set, scatter.grid, c("newyc","newxc"))
        seasonal.bin.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season),
                                  shot.bin.set.blocks, scatter.grid, c("newyc","newxc"))
        seasonal.superbin.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season), shot.bin.set.blocks, scatter.grid, c("newyc","newxc"), use.superblocks=TRUE)
        
        quants <- list(shots=current.seasons.pp,
                       
                       time.on.ice.home.O=time.on.ice.home.O,
                       time.on.ice.home.D=time.on.ice.home.D,
                       time.on.ice.away.O=time.on.ice.away.O,
                       time.on.ice.away.D=time.on.ice.away.D,
                       
                       time.off.ice.home.O=time.off.ice.home.O,
                       time.off.ice.home.D=time.off.ice.home.D,
                       time.off.ice.away.O=time.off.ice.away.O,
                       time.off.ice.away.D=time.off.ice.away.D,
                       
                       total.time.by.game.home.O=total.time.by.game.home.O,
                       total.time.by.game.away.O=total.time.by.game.away.O,
                   
                       home.played=home.played,
                       away.played=away.played,
                       
                       scatter.grid=scatter.grid,
                       
                       seasonal.counts=seasonal.counts,
                       seasonal.bin.counts=seasonal.bin.counts,
                       seasonal.superbin.counts=seasonal.superbin.counts
                       
                      #,overall.counts=overall.counts
                      #,overall.bin.counts=overall.bin.counts
                       )
    
        return(quants)
    }

    for (ss in 1:length(season.pick)) {
        
        message("Hextally players ",season.pick[ss])
        load (paste0(sources, season.pick[ss],".RData"))
        
        if (do.es) {
            message ("Even strength")
            es.quants.s <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                                home.skaters==6 & away.skaters==6 &
                                                home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                         subset(grand.data, #season %in% season.pick &
                                                substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])
        
            es.quants.p <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                                home.skaters==6 & away.skaters==6 &
                                                home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                         subset(grand.data, #season %in% season.pick &
                                                substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
        } else {es.quants.s <- es.quants.p <- NULL}
        
        if (do.four) {
            message ("Four on four")
            four.quants.s <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                                  home.skaters==5 & away.skaters==5 &
                                                  home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                           subset(grand.data, #season %in% season.pick &
                                                  substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])
            
            four.quants.p <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                                  home.skaters==5 & away.skaters==5 &
                                                  home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                           subset(grand.data, #season %in% season.pick &
                                                  substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
        } else {four.quants.s <- four.quants.p <- NULL}

        if (do.pp) {
            message ("Power play")
            pp.quants.s <- pp.shots.prep(subset(grand.data, #season %in% season.pick &
                                                ((home.skaters > away.skaters) |
                                                 (home.skaters < away.skaters)) &
                                                home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                         subset(grand.data, #season %in% season.pick &
                                                substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])
    
            pp.quants.p <- pp.shots.prep(subset(grand.data, #season %in% season.pick &
                                                ((home.skaters > away.skaters) |
                                                 (home.skaters < away.skaters)) &
                                                home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                         subset(grand.data, #season %in% season.pick &
                                                substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
        } else {pp.quants.s <- pp.quants.p <- NULL}
        
        save(pp.quants.s, pp.quants.p,
             es.quants.s, es.quants.p,
             four.quants.s, four.quants.p,
             file=paste0(output,season.pick[ss],".RData"))

    }
    
}








get.shots.teams <- function (season.pick=c("20052006","20062007","20072008","20082009", "20092010", "20102011",
                                   "20112012", "20122013", "20132014", "20142015"),
                             sources="source-data/nhlscrapr-",
                             output="common-data/hextally-teams-") {
    #season.pick="20062007"; sources="source-data/nhlscrapr-"; output="common-data/hextally-teams-"
    
    load (paste0(sources,"core.RData"))


    #datablock <- subset(grand.data, home.skaters==6 & away.skaters==6 & home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2")
    es.shots.prep <- function (datablock) {
        this.season.pick <- datablock$season[1]
        teams <- sort(unique(c(datablock$hometeam, datablock$awayteam)))
        
        time.on.ice.home <- time.on.ice.away <- matrix(0, nrow=length(teams), ncol=length(this.season.pick))
        rownames(time.on.ice.home) <- rownames(time.on.ice.away) <- teams
        colnames(time.on.ice.home) <- colnames(time.on.ice.away) <- paste0("TOI",this.season.pick)
        
        col <- "hometeam"  
        t.table <- tapply(datablock$event.length, list(datablock[,col], as.character(datablock$season)), sum)
        t.table[is.na(t.table)] <- 0
        time.on.ice.home[match(rownames(t.table), teams),] <- time.on.ice.home[match(rownames(t.table), teams),] + t.table
        
        col <- "awayteam"  
        t.table <- tapply(datablock$event.length, list(datablock[,col], as.character(datablock$season)), sum)
        t.table[is.na(t.table)] <- 0
        time.on.ice.away[match(rownames(t.table), teams),] <- time.on.ice.away[match(rownames(t.table), teams),] + t.table
        
        time.on.ice <- time.on.ice.home + time.on.ice.away    
        colnames(time.on.ice) <- colnames(time.on.ice.home) <- colnames(time.on.ice.away) <- this.season.pick
        
        shots <- subset(datablock, etype %in% c("GOAL","MISS","SHOT","BLOCK")) #!is.na(xcoord) & 
        flips <- which(shots$xcoord<0)
        shots$xcoord[flips] <- -shots$xcoord[flips]; shots$ycoord[flips] <- -shots$ycoord[flips]
        flips <- which(shots$newxc<0)
        shots$newxc[flips] <- -shots$newxc[flips]; shots$newyc[flips] <- -shots$newyc[flips]
        
        current.seasons.es <- shots[,c("season", "gcode", "period", "etype", "ev.team", "ev.player.1",
                                       "type", "xcoord", "ycoord", "loc.section", "awayteam", "hometeam",
                                       "distance",
                                       "newxc", "newyc", "new.loc.section",
                                       "away.score", "home.score",
                                       "away.G", "home.G", "home.skaters", "away.skaters")]
        
        scatter.grid <- point.grid()
        
        seasonal.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set, scatter.grid)
        seasonal.bin.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set.blocks, scatter.grid)
        seasonal.superbin.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set.blocks, scatter.grid, use.superblocks=TRUE)
        
        quants <- list(shots=current.seasons.es,
                       
                       time.on.ice.home.O=time.on.ice.home,
                       time.on.ice.home.D=time.on.ice.home,
                       time.on.ice.away.O=time.on.ice.away,
                       time.on.ice.away.D=time.on.ice.away,
                       
                       scatter.grid=scatter.grid,
                       seasonal.counts=seasonal.counts,
                       seasonal.bin.counts=seasonal.bin.counts,
                       seasonal.superbin.counts=seasonal.superbin.counts)
        
        return(quants)
    }
  
  
    pp.shots.prep <- function (datablock) {

        this.season.pick <- datablock$season[1]
        teams <- sort(unique(c(datablock$hometeam, datablock$awayteam)))

        datablock.home <- subset(datablock, home.skaters > away.skaters)
        datablock.away <- subset(datablock, home.skaters < away.skaters)
        
  ##get time on ice for each player in each season.

        placer <- matrix(0, nrow=length(teams), ncol=length(this.season.pick)); rownames(placer) <- teams; colnames(placer) <- paste0("TOI",this.season.pick)
        time.on.ice.home.O <- time.on.ice.home.D <-
            time.on.ice.away.O <- time.on.ice.away.D <- placer
        
        col <- "hometeam"  
        t.table <- tapply(datablock.home$event.length,
                          list(datablock.home[,"hometeam"], as.character(datablock.home$season)), sum)
        t.table[is.na(t.table)] <- 0
        time.on.ice.home.O[match(rownames(t.table), teams),] <- t.table   #time.on.ice.home.O[match(rownames(t.table), teams),] +
        
        t.table <- tapply(datablock.away$event.length,
                          list(datablock.away[,"hometeam"], as.character(datablock.away$season)), sum)
        t.table[is.na(t.table)] <- 0
        time.on.ice.home.D[match(rownames(t.table), teams),] <- t.table   #time.on.ice.away.O[match(rownames(t.table), teams),] +
        
        
        col <- "awayteam"  
        t.table <- tapply(datablock.home$event.length,
                          list(datablock.home[, "awayteam"], as.character(datablock.home$season)), sum)
        t.table[is.na(t.table)] <- 0
        time.on.ice.away.D[match(rownames(t.table), teams),] <- t.table   #time.on.ice.away.D[match(rownames(t.table), teams),] +
        
        t.table <- tapply(datablock.away$event.length,
                          list(datablock.away[, "awayteam"], as.character(datablock.away$season)), sum)
        t.table[is.na(t.table)] <- 0
        time.on.ice.away.O[match(rownames(t.table), teams),] <- t.table   #time.on.ice.home.D[match(rownames(t.table), teams),] +
        
        time.on.ice.O <- time.on.ice.home.O + time.on.ice.away.O
        time.on.ice.D <- time.on.ice.home.D + time.on.ice.away.D
        
        colnames(time.on.ice.O) <- colnames(time.on.ice.home.O) <- colnames(time.on.ice.away.O) <- colnames(time.on.ice.D) <- colnames(time.on.ice.home.D) <- colnames(time.on.ice.away.D) <- this.season.pick
        
        shots <- subset(datablock, !is.na(xcoord) & etype %in% c("GOAL","MISS","SHOT","BLOCK"))
        ## & ((ev.team == hometeam & home.skaters == 6) | (ev.team == awayteam & away.skaters == 6)))
        flips <- which(shots$xcoord<0)
        shots$xcoord[flips] <- -shots$xcoord[flips]; shots$ycoord[flips] <- -shots$ycoord[flips]
        flips <- which(shots$newxc<0)
        shots$newxc[flips] <- -shots$newxc[flips]; shots$newyc[flips] <- -shots$newyc[flips]
        
        current.seasons.pp <- shots[,c("season", "gcode", "period", "etype", "ev.team", "ev.player.1",
                                       "type", "xcoord", "ycoord", "loc.section", "awayteam", "hometeam",
                                       "distance",
                                       "newxc", "newyc", "new.loc.section",
                                       "away.score", "home.score",
                                       "away.G", "home.G", "home.skaters", "away.skaters")]
        
        scatter.grid <- point.grid()
        
        seasonal.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season), shot.bin.set, scatter.grid)
        seasonal.bin.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season), shot.bin.set.blocks, scatter.grid)
        seasonal.superbin.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season), shot.bin.set.blocks, scatter.grid, use.superblocks=TRUE)

        quants <- list(shots=current.seasons.pp,

                       time.on.ice.O=time.on.ice.O,
                       time.on.ice.D=time.on.ice.D,
                       
                       time.on.ice.away.O=time.on.ice.away.O,
                       time.on.ice.away.D=time.on.ice.away.D,
                       time.on.ice.home.O=time.on.ice.home.O,
                       time.on.ice.home.D=time.on.ice.home.D,
                       
                       scatter.grid=scatter.grid,
                       seasonal.counts=seasonal.counts,
                       seasonal.bin.counts=seasonal.bin.counts,
                       seasonal.superbin.counts=seasonal.superbin.counts)
        return(quants)
    }

    ## Here's where we do it.
    for (ss in 1:length(season.pick)) {
        
        message("Hextally teams ",season.pick[ss])
        load (paste0(sources, season.pick[ss],".RData"))
    
        es.quants.s <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                            home.skaters==6 & away.skaters==6 &
                                            home.G > 1 & away.G > 1 &
                                            substr(gcode,1,1) == "2"))
        es.quants.p <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                            home.skaters==6 & away.skaters==6 &
                                            home.G > 1 & away.G > 1 &
                                            substr(gcode,1,1) == "3"))
        four.quants.s <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                              home.skaters==5 & away.skaters==5 &
                                              home.G > 1 & away.G > 1 &
                                              substr(gcode,1,1) == "2"))
        four.quants.p <- es.shots.prep(subset(grand.data, #season %in% season.pick &
                                              home.skaters==5 & away.skaters==5 &
                                              home.G > 1 & away.G > 1 &
                                              substr(gcode,1,1) == "3"))
        pp.quants.s <- pp.shots.prep(subset(grand.data, #season %in% season.pick &
                                            ((home.skaters > away.skaters) |
                                             (home.skaters < away.skaters)) &
                                            home.G > 1 & away.G > 1 &
                                            substr(gcode,1,1) == "2"))
        pp.quants.p <- pp.shots.prep(subset(grand.data, #season %in% season.pick &
                                            ((home.skaters > away.skaters) |
                                             (home.skaters < away.skaters)) &
                                            home.G > 1 & away.G > 1 &
                                            substr(gcode,1,1) == "3"))
        
        save(pp.quants.s, pp.quants.p, es.quants.s, es.quants.p,
             four.quants.s, four.quants.p, file=paste0(output,season.pick[ss],".RData"))
    }
    
}






game.toi <- function (mega.file="nhlscrapr-probs.RData",
                      output.file="nhlscrapr-pp-shots-teams.RData") {
  #mega.file="m1.RData"
  load (mega.file)

  grand.data$manstate <- with(grand.data, 1*(home.skaters == 6 & away.skaters == 6) +
                              2*(home.skaters == 6 & away.skaters < 6) +
                              3*(home.skaters < 6 & away.skaters == 6) +
                              4*(home.skaters == 5 & away.skaters == 5))
                              
  
  season.pick <- c("20052006","20062007","20072008","20082009", "20092010", "20102011", "20112012", "20122013", "20132014")
  fourseasons.pp <- subset(grand.data, season %in% season.pick &
                           ((home.skaters == 6 & away.skaters < 6) | (home.skaters < 6 & away.skaters == 6)) &
                           home.G > 1 & away.G > 1 &
                           substr(gcode,1,1) == "2")
  fourseasons.pp.home <- subset(fourseasons.pp, home.skaters == 6)
  fourseasons.pp.away <- subset(fourseasons.pp, home.skaters < 6)

  
  gameprops <- fourseasons.pp[match(unique(paste(fourseasons.pp$season, fourseasons.pp$gcode)),
                                    paste(fourseasons.pp$season, fourseasons.pp$gcode)),
                              c("season","gcode","awayteam","hometeam")]

  gametime <- tapply(fourseasons.pp$event.length, list(paste(fourseasons.pp$season, fourseasons.pp$gcode), fourseasons.pp$manstate), sum)
  gameprops <- cbind(gameprops, gametime[match(paste(gameprops$season, gameprops$gcode), rownames(gametime)),])

  plot(gameprops[,5], gameprops[,6], ty="n")
  text(gameprops[,5], gameprops[,6], paste(gameprops$awayteam, gameprops$hometeam, sep="/"))
  
  

}
