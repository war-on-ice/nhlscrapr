
# Functions for processing HTML files raw.

get.game.start.end <- function(es.html) {
    ## es.html <- '<td align="center" style="font-size: 10px;font-weight:bold">Start&nbsp;12:06&nbsp;PDT</td>'

    es.html <- gsub("&nbsp;", " ", es.html)

    start.time.proto <- unlist(regmatches(es.html, gregexpr("([Ss]tart|[Ss]tart Time|[Dd]ebut|D\u0082but) *([0-9]{1,2}:[0-9]{2} *[APM]{0,2} *[A-Z]{0,3})", es.html)))
    start.time <- gsub ("([Ss]tart|[Ss]tart Time|[Dd]ebut|D\u0082but) *([0-9]{1,2}:[0-9]{2} *[APM]{0,2} *[A-Z]{0,3})", "\\2", start.time.proto)

    end.time.proto <- unlist(regmatches(es.html, gregexpr("(End|End Time|Fin) *([0-9]{1,2}:[0-9]{2} *[APM]{0,2} *[A-Z]{0,3})", es.html)))
    end.time <- gsub ("(End|End Time|Fin) *([0-9]{1,2}:[0-9]{2} *[APM]{0,2} *[A-Z]{0,3})", "\\2", end.time.proto)

    return(c(start.time, end.time))
  
}


process.pl.old <- function (pl.html) {
    ## load("~/Documents/nhlr/war-on-ice.com/nhlr-data/20052006-20203.RData");
  #load("../../nhlr-data/20052006-20049.RData");
  #pl.html = game.rec$pl
    
    pl.1 <- grep("<PRE>  #   Per  Time  Event", pl.html)
        if (length(pl.1) > 0) {pl.html <- c(pl.html[1:pl.1[1]],
                                            "",
                                            pl.html[(pl.1[1]+1):length(pl.html)])}
    
    pl.html <- gsub("<([A-Z]+)>", "<\\1>", pl.html)
    pl.html <- gsub("<TD", "<td", pl.html)
    
    date <- unlist(strsplit (gsub(" *([A-Za-z]+), ([A-Za-z]+) ([0-9]+), ([0-9]+).*",
                                  "\\1;\\2;\\3;\\4",
                                  pl.html[grep("[A-Za-z]+, [A-Za-z]+ [0-9]+, [0-9]+", pl.html)]),
                             ";"))
    ##if (is.null(date)) {
    ##    date <- unlist(strsplit (gsub(" *([A-Za-z/\\.]+), ([0-9]+) ([A-Za-z]+)\\. ([0-9]+).*",
    ##                                  "\\1;\\3;\\2;\\4",
    ##                                  pl.html[grep("([0-9]+) ([A-Za-z]+)\\. ([0-9]+)", pl.html)]),
    ##                             ";"))
    ##    date[1] <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[match(
    ##        date[1], c("Dim./Sun.", "Lun./Mon.", "Mar./Tue.", "Mer./Wed.", "Jeu./Thu.", "Ven./Fri.", "Sam./Sat."))]
    ##}
    
    nums <- range(grep("[Pp][Rr][Ee]>", pl.html))
    if (nums[1] == nums[2]) nums[2] <- length(pl.html)
    extract <- pl.html[(nums[1]+3):(nums[2]-1)]
    extract <- gsub("<i>(.*)</i>", "\\1", extract)
    extract <- gsub("<I>(.*)</I>", "\\1", extract)
    
    extract <- gsub("<strong>", "", extract)
    extract <- gsub("</strong>", "", extract)
    extract <- gsub("<STRONG>", "", extract)
    extract <- gsub("</STRONG>", "", extract)
    extract <- gsub("Penalty Shot,", "Penalty Shot", extract)
    extract <- gsub("\\t ", "", extract)
    extract <- gsub("\\t", "", extract)


    goal.lines <- which(substr(extract,5,5) == " ")
    ##event.rec <- extract[gsub(" +([0-9]+).*", "\\1", extract) != extract]
    if (length(goal.lines)>0) event.rec <- extract[-goal.lines] else event.rec <- extract
    event.rec <- event.rec[substr(event.rec,5,5) %in% as.character(0:9) &
                           substr(event.rec,1,1) == " "]
    
    goal.players <- extract[goal.lines]
    goal.players <- goal.players[grep("^ *[A-Z\\.]{3}", goal.players)]
    goal.players <- t(matrix(goal.players[!grepl(" +SO +", goal.players)], nrow=2))
    
    new.event <- data.frame(event=0, period=0, seconds=0, etype="", etext="",
                            a1="", a2="", a3="", a4="", a5="", a6="",
                            h1="", h2="", h3="", h4="", h5="", h6="",
                            ev.team="",
                            
                            ev.player.1="", ev.player.2="", ev.player.3="",
                            distance=NA, type="", type2 = "", homezone="",
                            stringsAsFactors=FALSE)
    
    playbyplay <- NULL; for (kk in 1:length(event.rec)) playbyplay <- rbind(playbyplay, new.event)
    for (kk in c(4:20,22:24)) playbyplay[,kk] <- as.character(playbyplay[,kk])
    
    playbyplay$event <- as.numeric(substr(event.rec,1,5))
    playbyplay$period <- as.numeric(substr(event.rec,6,9))
    playbyplay$seconds <- 1200*(playbyplay$period-1)+60*as.numeric(substr(event.rec,12,13))+as.numeric(substr(event.rec,15,16))
    playbyplay$etype <- substr(event.rec,18,34)
    playbyplay$ev.team <- substr(event.rec,35,37)
    playbyplay$etext <- substr(event.rec,43,nchar(event.rec))

                                        ##BLOCK, GOAL, MISS, SHOT      FAC,    GIVE, TAKE      PENL    HIT    
                                        ##STOP  PEND 
    event.types <- matrix(c("BLOCK", "BLOCKED SHOT     ",  "GOAL",  "GOAL             ",
                            "MISS",  "MISSED SHOT      ",  "SHOT",  "SHOT             ",
                            "FAC",   "FACE-OFF         ",  "GIVE",  "GIVEAWAY         ",
                            "TAKE",  "TAKEAWAY         ",  "PENL",  "PENALTY          ",
                            "HIT",   "HIT              ",  "STOP",  "STOPPAGE         ",
                            "PULL",  "GOALIE           ",  "HIT",   "HIT (!)          ",
                            "HIT",   "HIT (*)          ",  "SHOT",  "SHOT (!)         ",
                            "SHOT",  "SHOT (*)         "), nrow=2)
    playbyplay$etype <- event.types[1, match(playbyplay$etype, event.types[2,])]
    playbyplay$etype[is.na(playbyplay$etype)] <- ""
    
    playbyplay <- playbyplay[!(playbyplay$etype %in% c("STOP","")),]    #, "PEND"
  
  #playbyplay[playbyplay$etype=="GOAL",-(6:17)]
  # FAC.
  #c1 <- which(playbyplay$etype=="FAC")
  #teamwon;homezone;ateam;anum;aname;hteam;hnum;hname
    p1 <- sapply(gsub(" *([A-Z'\\.\\-]+) won - ([A-z]+) zone\\. ([A-Z'\\.\\-]+) +([0-9]+) ([A-Z' \\.\\-]+) vs ([A-Z'\\.\\-]+) +([0-9]+) ([A-Z' \\.\\-]+)",
                      "\\1;\\2;\\3;\\4;\\5;\\6;\\7;\\8",
                      playbyplay$etext[playbyplay$etype=="FAC"]),
                 function(tt) unlist(strsplit (tt, ";")))
    teams <- p1[c(3,6), 1]
    playbyplay$ev.team[playbyplay$etype=="FAC"] <- p1[1,]
    playbyplay$homezone[playbyplay$etype=="FAC"] <- p1[2,]

  ## switch up: winner, then loser here.
    playbyplay$ev.player.1[playbyplay$etype=="FAC"][p1[1,]==p1[3,]] <- paste(p1[4,], p1[5,])[p1[1,]==p1[3,]]
    playbyplay$ev.player.1[playbyplay$etype=="FAC"][p1[1,]!=p1[3,]] <- paste(p1[7,], p1[8,])[p1[1,]!=p1[3,]]
    playbyplay$ev.player.2[playbyplay$etype=="FAC"][p1[1,]==p1[3,]] <- paste(p1[7,], p1[8,])[p1[1,]==p1[3,]]
    playbyplay$ev.player.2[playbyplay$etype=="FAC"][p1[1,]!=p1[3,]] <- paste(p1[4,], p1[5,])[p1[1,]!=p1[3,]]
    
    playbyplay$etext[playbyplay$etype=="FAC"] <- ""
    

  
  
  #BLOCK
  #num;name
    if (sum(playbyplay$etype=="BLOCK") > 0) {
        p1 <- sapply(gsub(" *([0-9]+) +([A-Z' \\.\\-]+)", "\\1;\\2",
                          playbyplay$etext[playbyplay$etype=="BLOCK"]),
                     function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.2[playbyplay$etype=="BLOCK"] <- paste(p1[1,], p1[2,])   #Gorram it! 2014-10-22
        
    #teamset <- unique(playbyplay$ev.team); teamset <- teamset[nchar(teamset)>0]          #The other team made the shot.
        playbyplay$ev.team[playbyplay$etype=="BLOCK" & playbyplay$ev.team==teams[1]] <- "SWITCH"
        playbyplay$ev.team[playbyplay$etype=="BLOCK" & playbyplay$ev.team==teams[2]] <- teams[1]
        playbyplay$ev.team[playbyplay$etype=="BLOCK" & playbyplay$ev.team=="SWITCH"] <- teams[2]
        playbyplay$etext[playbyplay$etype=="BLOCK"] <- ""
    }

  #MISS
  #num;name
    if (sum(playbyplay$etype=="MISS") > 0) {
        p1 <- sapply(gsub(" *([0-9]+) +([A-Z' \\.\\-]+), (.*)", "\\1;\\2;\\3",
                          playbyplay$etext[playbyplay$etype=="MISS"]),
                     function(tt) {
                         text1 <- unlist(strsplit (tt, ";"))
                         if (length(text1) %in% 1:2) text1 <- unlist(strsplit(gsub(" *([0-9]+) +([A-Z' \\.\\-]+) (.*)", "\\1;\\2;\\3 ", text1), ";"))
                         text1
                     })
        playbyplay$ev.player.1[playbyplay$etype=="MISS"] <- paste(p1[1,], p1[2,])
        playbyplay$type2[playbyplay$etype=="MISS"] <- p1[3,]
        playbyplay$etext[playbyplay$etype=="MISS"] <- ""
    }

  #HIT, TAKE, GIVE
  #num;name
    if (sum(playbyplay$etype %in% c("HIT", "GIVE", "TAKE")) > 0) {
        p1 <- sapply(gsub(" *([0-9]+) +([A-Z' \\.\\-]+)", "\\1;\\2",
                          playbyplay$etext[playbyplay$etype %in% c("HIT", "TAKE", "GIVE")]),
                     function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.1[playbyplay$etype %in% c("HIT", "TAKE", "GIVE")] <- paste(p1[1,], p1[2,])
        playbyplay$etext[playbyplay$etype %in% c("HIT", "TAKE", "GIVE")] <- ""
    }

  #PENL
  #num;name;type
    if (sum(playbyplay$etype=="PENL") > 0) {
        p1 <- sapply(gsub(" *([0-9]+) +([A-Z' \\.\\-]+), (.*)", "\\1;\\2;\\3",
                          playbyplay$etext[playbyplay$etype=="PENL"]),
                     function(tt) {
                         text1 <- unlist(strsplit (tt, ";"))
                         if (length(text1) != 3) text1 <- c("","","Bench Minor")
                         text1
                     })
        playbyplay$ev.player.1[playbyplay$etype=="PENL"] <- paste(p1[1,], p1[2,])
        playbyplay$type[playbyplay$etype=="PENL"] <- p1[3,]
        playbyplay$etext[playbyplay$etype=="PENL"] <- ""
    }
  

  
  #SHOT
  #name happens one time in mixed case: 38 Robinson, 0304-2-0333
    if (sum(playbyplay$etype=="SHOT") > 0) {
        p1 <- sapply(gsub(" *([0-9]+) +([A-Za-z' \\.\\-]+), +([A-Za-z '\\-]+), +([0-9]+) .*", "\\1;\\2;\\3;\\4",
                          playbyplay$etext[playbyplay$etype=="SHOT"]),
                     function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.1[playbyplay$etype=="SHOT"] <- paste(p1[1,], toupper(p1[2,]))
        playbyplay$type[playbyplay$etype=="SHOT"] <- p1[3,]
        playbyplay$distance[playbyplay$etype=="SHOT"] <- as.numeric(p1[4,])
        playbyplay$etext[playbyplay$etype=="SHOT"] <- ""
    }
  
  #GOAL
  #2 assists
    c2 <- which(playbyplay$etype=="GOAL" & grepl("A: +[0-9]+ +[A-Z' \\.\\-]+, +[0-9]+", playbyplay$etext))
    if (length(c2)>0) {
        p1 <- sapply(gsub(" *([0-9]+) +([A-Z' \\.\\-]+),? +A: +([0-9]+) +([A-Z' \\.\\-]+), +([0-9]+) +([A-Z' \\.\\-]+),? *([A-Za-z '\\-]+), +([0-9]+) .*",
                          "\\1;\\2;\\3;\\4;\\5;\\6;\\7;\\8",
                          playbyplay$etext[c2]),
                     function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.1[c2] <- paste(p1[1,], p1[2,])
        playbyplay$ev.player.2[c2] <- paste(p1[3,], p1[4,])
        playbyplay$ev.player.3[c2] <- paste(p1[5,], p1[6,])
        playbyplay$type[c2] <- p1[7,]
        playbyplay$distance[c2] <- as.numeric(p1[8,])
    }
  
  #1 assist
  #name happens one time in mixed case: 22 Pock- 0304-2-1136
    c2 <- which(playbyplay$etype=="GOAL" & grepl("A: +[0-9]+ +[A-Z' \\.\\-]+, +[A-Za-z]+", playbyplay$etext))
    if (length(c2)>0) {
        p1 <- sapply(gsub(" *([0-9]+) ([A-Za-z' \\.\\-]+), A: +([0-9]+) ([A-Z' \\.\\-]+), ([A-Za-z '\\-]+), +([0-9]+) .*",
                          "\\1;\\2;\\3;\\4;\\5;\\6",
                          playbyplay$etext[c2]),
                     function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.1[c2] <- paste(p1[1,], toupper(p1[2,]))
        playbyplay$ev.player.2[c2] <- paste(p1[3,], toupper(p1[4,]))
        playbyplay$type[c2] <- p1[5,]
        playbyplay$distance[c2] <- as.numeric(p1[6,])
    }
  
  #unassisted
    c2 <- which(playbyplay$etype=="GOAL" & grepl("A: +[A-Za-z'\\-]+, +[A-Za-z]+", playbyplay$etext))
    if (length(c2)>0) {
        p1 <- sapply(gsub(" *([0-9]+) ([A-Z' \\.\\-]+), A: [A-Za-z'\\-]+, ([A-Za-z '\\-]+), +([0-9]+).*",
                          "\\1;\\2;\\3;\\4",
                          playbyplay$etext[c2]),
                     function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.1[c2] <- paste(p1[1,], p1[2,])
        playbyplay$type[c2] <- p1[3,]
        playbyplay$distance[c2] <- as.numeric(p1[4,])
    }
    
    playbyplay$etext[playbyplay$etype=="GOAL"] <- ""

  
  # players on ice for goals.
  #goal.rec <- array(0, c(dim(goal.players), 6))
    goal.evs <- which(playbyplay$etype=="GOAL")
    if (length(goal.evs) > 0)
        for (ii in 1:dim(goal.players)[1]) {
            for (jj in 1:2) {
                                        #home team?
                home <- 1*(gsub(" +([A-Z\\.]+).*", "\\1", goal.players[ii,jj])==teams[2])
                sq <- unlist(strsplit(goal.players[ii,jj], " "))
                                        #manual fix for space names like DE VRIES.
                for (xx in length(sq):2)
                    if (grepl("[A-Z]+", sq[xx]) & grepl("[A-Z]+", sq[xx-1])) {
                        sq[xx-1] <- paste(sq[xx-1], sq[xx])
                        sq <- sq[-xx]
                    }
                
                nums <- grep("[0-9]+", sq)
                playbyplay[goal.evs[ii],5+6*home+1:length(nums)] <- paste(sq[nums], sq[nums+1])
            }
        }
    
    playbyplay$homezone[playbyplay$homezone == "defensive"] <- "Def"
    playbyplay$homezone[playbyplay$homezone == "offensive"] <- "Off"
    playbyplay$homezone[playbyplay$homezone == "neutral"] <- "Neu"

  
  #remove text.
  ##playbyplay <- playbyplay[,-5]

  #flip zone to home zone referent.
    flip.1 <- playbyplay$ev.team == teams[1] & playbyplay$homezone=="Off"
    flip.2 <- playbyplay$ev.team == teams[1] & playbyplay$homezone=="Def"
    playbyplay$homezone[flip.1] <- "Def"
    playbyplay$homezone[flip.2] <- "Off"
    
  #Add this.
    playbyplay$homezone[playbyplay$etype %in% c("SHOT", "BLOCK", "GOAL", "MISS") & playbyplay$ev.team == teams[1]] <- "Def"
    playbyplay$homezone[playbyplay$etype %in% c("SHOT", "BLOCK", "GOAL", "MISS") & playbyplay$ev.team == teams[2]] <- "Off"

    output <- list(playbyplay=playbyplay,
                   teams=teams,
                   date=date)
                 
    return(output)
  
}


p.split <- function(bit, pieces) {
  out1 <- unlist(strsplit (bit, ";"))
  if (length(out1) == 1) out1 <- rep("", pieces)   
  out1
}

process.pl.new <- function (pl.html) {
    ## load("~/Documents/nhlr/war-on-ice.com/nhlr-data/20072008-20001.RData");
    ##load("../../nhlr-data/20142015-20008.RData");
    ##pl.html = game.rec$pl
  
    teams <- pl.html[grep(".*On Ice.*", pl.html)][1:2]
    teams <- gsub("<td width=\"10%\" class=\"heading \\+ bborder\" align=\"center\">([A-Z\\.]+) On Ice</td>.*", "\\1", teams)
    
    date <- pl.html[grep("<td align=\"center\" style=\"font-size: 10px;font-weight:bold\">([A-Za-z]+), ([A-Za-z]+) ([0-9]+), ([0-9]+)</td>", pl.html)][1]
    date <- gsub("<td align=\"center\" style=\"font-size: 10px;font-weight:bold\">([A-Za-z]+), ([A-Za-z]+) ([0-9]+), ([0-9]+)</td>", "\\1;\\2;\\3;\\4", date)
    date <- unlist(strsplit(date, ";"))
    
    pl.main <- {
        find.1 <- grep("class=\"[a-z \\+]* bborder\"", pl.html)
        find.2 <- grep("<font style=\"cursor:hand;\"", pl.html)
        pl.reduced <- pl.html[sort(c(find.1, find.2))]
        pl.reduced <- gsub("&nbsp;", ", ", pl.reduced)
        pl.reduced <- gsub("; ", ", ", pl.reduced)
        pl.reduced <- gsub("[\uC9\uCA\uCB]", "E", pl.reduced)
        
                                        # p: player id.
        pl.reduced.1 <- gsub("<font style=\"cursor:hand;\" title=\"([A-Za-z ]+) - ([A-Za-z' \\.\\(\\)-]+)\">([0-9]+).*", "p;\\1;\\2;\\3", pl.reduced)
                                        # s: time
        pl.reduced.1 <- gsub("<td class=\"[a-z \\+]* \\+ bborder\" align=\"center\">([0-9]+):([0-9]+)<br.*", "s;\\1;\\2;", pl.reduced.1)
                                        # q: period number.
        pl.reduced.1 <- gsub("<td class=\"[a-z]* \\+ bborder\" align=\"center\">([0-9]+)</td>.*", "q;\\1;;", pl.reduced.1)
        
                                        # t: event type;
        pl.reduced.2 <- gsub("<td class=\"[a-z]* \\+ bborder\">([A-Z]{3,})</td>", "t;\\1;;", pl.reduced.1)
        pl.reduced.2 <- gsub("<td class=\"[a-z]* \\+ bborder\" align=\"center\">([A-Z]{3,})</td>", "t;\\1;;", pl.reduced.2)
        
                                        # x: event text;
                                        #pl.reduced.3 <- gsub("<td class=\"[a-z]* \\+ bborder\">([A-Za-z0-9',# \\(\\)\\.-]{3,})</td>", "x;\\1;;", pl.reduced.2)
                                        #pl.reduced.3 <- gsub("<td class=\"[a-z]* \\+ bborder\" align=\"center\">([A-Za-z0-9',# \\(\\)\\.-]{3,})</td>", "x;\\1;;", pl.reduced.3)
        pl.reduced.3 <- gsub("<td class=\"[a-z]* \\+ bborder\">(.{3,})</td>", "x;\\1;;", pl.reduced.2)
        pl.reduced.3 <- gsub("<td class=\"[a-z]* \\+ bborder\" align=\"center\">(.{3,})</td>", "x;\\1;;", pl.reduced.3)
        
                                        # e: event number;
        pl.reduced.4 <- gsub("<td align=\"center\" class=\"[a-z \\+]* \\+ bborder\">([0-9]+)</td>", "e;\\1;;", pl.reduced.3)
        
                                        # b: break;
        pl.reduced.5 <- gsub("<td class=\"[a-z \\+]* \\+ bborder\">", "b;;;", pl.reduced.4)
        
        pl.reduced.5[substr(pl.reduced.5, 1, 1) != "<"]
    }

  #fix one.
    pl.main <- gsub("Penalty Shot,", "Penalty Shot", pl.main)
  
    pl.grid <- sapply(pl.main, function(tt) {
        tt <- unlist(strsplit(tt, ";"))
        while (length(tt) < 4) tt <- c(tt, "")
        tt
    })
    
    playbyplay <- NULL
    new.event <- data.frame(event=0, period=0, seconds=0, etype="", etext="",
                            
                            a1=NA, a2=NA, a3=NA, a4=NA, a5=NA, a6=NA,
                            h1=NA, h2=NA, h3=NA, h4=NA, h5=NA, h6=NA)
    
    started <- FALSE; current.event <- NULL
    
    a.h <- rep(0,dim(pl.grid)[2])
    sw <- 1
    for (kk in 1:length(pl.main)) {
        if (pl.grid[1,kk] %in% c("e","b")) sw <- 1-sw
        a.h[kk] <- sw
    }
    pl.grid <- rbind(pl.grid, a.h)
    players <- unique(t(pl.grid[2:5, pl.grid[1,]=="p"]))
    rownames(players) <- NULL
    
    
    for (row in 1:length(pl.main)) {
        if (pl.grid[1, row] == "e") {
            if (started) playbyplay <- rbind(playbyplay, current.event) else started <- TRUE
            current.event <- new.event
            current.event$event <- as.numeric(pl.grid[2,row])
            coln <- 6
            ttype <- FALSE
            homeyet <- FALSE
        }
        if (pl.grid[1, row] == "q") current.event$period <- as.numeric(pl.grid[2,row])
        if (pl.grid[1, row] == "s") current.event$seconds <- 60*as.numeric(pl.grid[2,row])+as.numeric(pl.grid[3,row])
        if (pl.grid[1, row] == "t") current.event$etype <- pl.grid[2,row]
        if (pl.grid[1, row] == "x") current.event$etext <- pl.grid[2,row]
        
        if (pl.grid[1, row] == "p") {
                                        #      current.event[1,coln] <- match(paste(pl.grid[3:4,row], collapse=" "),
                                        #                                     paste(players[,2],players[,3]))
            current.event[1,coln] <- paste(pl.grid[4:3,row], collapse=" ")  #pl.grid[4,row]  #
            if (coln != 17 && coln != 11) coln <- coln + 1  
        }
        if (pl.grid[1, row] == "b") coln <- 12    #home team now.
    }
    playbyplay <- rbind(playbyplay, current.event)
    
    
    playbyplay$ev.team <- substr(playbyplay$etext, 1, 3)
    playbyplay$ev.player.1 <- ""
    playbyplay$ev.player.2 <- ""
    playbyplay$ev.player.3 <- ""
    playbyplay$distance <- NA
    playbyplay$type <- ""
    playbyplay$type2 <- ""
    playbyplay$homezone <- ""

  
    ## BLOCK, GOAL, MISS, SHOT      FAC, GIVE, TAKE    PENL      HIT    
    ## STOP  PEND 
    playbyplay <- playbyplay[!(playbyplay$etype %in% c("PSTR","GEND")),]   ## ,"ICING","OFFSIDE"
  
  #SHOT
    c1 <- which(playbyplay$etype=="SHOT")
    if (length(c1)>0) {
        p1 <- sapply(gsub("[A-Z \\.#-]*?([0-9]+) ([A-Z' \\.-]+), ([A-Za-z-]+),[A-Za-z,\\. ]*([0-9]+).*", "\\1;\\2;\\3;\\4", playbyplay$etext[c1]),
                     function(tt) {
                         out1 <- unlist(strsplit (tt, ";"))
                         if (length(out1) != 4) {  #No shot type listed.
                             out1 <- gsub(".*([0-9]+) ([A-Z' \\.-]+),[A-Za-z,\\. ]*([0-9]+).*", "\\1;\\2;\\3", out1)
                             out1 <- p.split(out1, 3)
                             out1 <- c(out1[1:2], "", out1[3])
                         }
                         out1
                     })
        playbyplay$ev.player.1[playbyplay$etype=="SHOT"] <- paste(p1[1,], p1[2,])
        playbyplay$distance[playbyplay$etype=="SHOT"] <- as.numeric(p1[4,])
        playbyplay$type[playbyplay$etype=="SHOT"] <- p1[3,]
        playbyplay$etext[c1] <- ""
    }
  
  #BLOCK
  #num;name;num2;name2;type
    c1 <- which(playbyplay$etype=="BLOCK")
    if (length(c1)>0) {
        p1 <- sapply(gsub(".*#([0-9]+) ([A-Z' \\.-]+) BLOCK[A-Z\\. ]+#([0-9]+) ([A-Z' \\.-]+), ([A-Za-z \\.]+),.*", "\\1;\\2;\\3;\\4;\\5",
                          playbyplay$etext[c1]),
                     function(tt) {
                         out1 <- unlist(strsplit (tt, ";"))
                         if (length(out1)==1) {
                             out1 <- gsub(".*#([0-9]+) ([A-Z' \\.-]+) BLOCK[A-Z\\. ]+#([0-9]+) ([A-Z' \\.-]+),.*",
                                          "\\1;\\2;\\3;\\4",
                                          out1)
                             out1 <- p.split(out1, 4)
                             out1 <- c(out1, "")
                         }
                         out1
                     })
        playbyplay$ev.player.1[playbyplay$etype=="BLOCK"] <- paste(p1[1,], p1[2,])
        playbyplay$ev.player.2[playbyplay$etype=="BLOCK"] <- paste(p1[3,], p1[4,])
        playbyplay$type[playbyplay$etype=="BLOCK"] <- p1[5,]
        playbyplay$etext[c1] <- ""
    }
    
  #MISS
  #num;name;type;homezone;distance
  #Wide of net; crossbar; post  -- ignored for now.

    c1 <- which(playbyplay$etype=="MISS")
    if (length(c1)>0) {
        
        p1 <- sapply(gsub(".* #([0-9]+) ([A-Z' \\.-]+), ([A-Za-z -]+), ?([A-Za-z,\\. ]*[rt]), ([A-Za-z]+)\\..* ([0-9]+) .*", "\\1;\\2;\\3;\\4;\\5;\\6", playbyplay$etext[c1]),
                     function(tt) {
                         out1 <- unlist(strsplit (tt, ";"))
                         if (length(out1)==1) { #assume missing net location.
                             out1 <- gsub(".* #([0-9]+) ([A-Z' \\.-]+), ([A-Za-z -]+), ([A-Za-z]+)\\..* ([0-9]+) .*",
                                          "\\1;\\2;\\3;NoInfo;\\4;\\5",
                                          out1)
                             out1 <- p.split(out1, 6)
                         }
                         out1
                     })
        playbyplay$ev.player.1[playbyplay$etype=="MISS"] <- paste(p1[1,], p1[2,])
        playbyplay$distance[playbyplay$etype=="MISS"] <- as.numeric(p1[6,])
        playbyplay$type[playbyplay$etype=="MISS"] <- p1[3,]
        playbyplay$type2[playbyplay$etype=="MISS"] <- p1[4,]
        playbyplay$etext[c1] <- ""
    }
  
  #GOAL
  #num;name;type;distance
  #c2 <- which(playbyplay$etype=="GOAL" & grepl("Assists:", playbyplay$etext))
    c1 <- which(playbyplay$etype=="GOAL")
    if (length(c1)>0) {
        p1 <- sapply(gsub(".* #([0-9]+) ([A-Z' \\.-]+)[\\(\\)0-9, ]+([A-Za-z -]+), [A-Za-z]+\\..* ([0-9]+) .*",
                          "\\1;\\2;\\3;\\4", playbyplay$etext[c1]),
                     function(tt) {
                         out1 <- unlist(strsplit (tt, ";"))
                         if (length(out1)==1) { #assume missing shot type.
                             out2 <- gsub(".* #([0-9]+) ([A-Z' \\.-]+)[\\(\\)0-9, ]+ [A-Za-z]+\\..* ([0-9]+) .*",
                                          "\\1;\\2;;\\3",
                                          out1)
                             if (out2 == out1) #missing zone.
                                 out2 <- gsub(".* #([0-9]+) ([A-Z' \\.-]+)[\\(\\)0-9, ]+([A-Za-z -]+), ([0-9]+) .*",
                                              "\\1;\\2;\\3;\\4",
                                              out1)
                             out1 <- p.split(out2, 4)
                         }
                         out1
                     })
        playbyplay$ev.player.1[c1] <- paste(p1[1,], p1[2,])
        playbyplay$distance[c1] <- as.numeric(p1[4,])
        playbyplay$type[c1] <- p1[3,]
    }
    
    c2 <- which(playbyplay$etype=="GOAL" & grepl("Assists:", playbyplay$etext))
    if (length(c2) > 0) {
        p1 <- sapply(gsub(".*Assists: #([0-9]+) ([A-Z' \\.-]+)[\\(\\)0-9]+, #([0-9]+) ([A-Z' \\.-]+)[\\(\\)0-9, ]+.*", "\\1;\\2;\\3;\\4", playbyplay$etext[c2]),
                     p.split, 4)
                                        #                 function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.2[c2] <- paste(p1[1,], p1[2,])
        playbyplay$ev.player.3[c2] <- paste(p1[3,], p1[4,])
    }
    
    c3 <- which(playbyplay$etype=="GOAL" & grepl("Assist:", playbyplay$etext))
    if (length(c3) > 0) {
        p1 <- sapply(gsub(".*Assist: #([0-9]+) ([A-Z' \\.-]+).*", "\\1;\\2;\\", playbyplay$etext[c3]),
                     p.split, 2)
        playbyplay$ev.player.2[c3] <- paste(p1[1,], p1[2,])
    }

    playbyplay$etext[union(c1,union(c2,c3))] <- ""

  
  #FAC
  #winner;zone;num1;name1;num2;name2
  
    c1 <- which(playbyplay$etype=="FAC")
    if (length(c1)>0) {
        p1 <- sapply(gsub(paste0(".* won ", ## winning team
                                 "([A-Za-z]+)\\..*", ## zone
                                 "([A-Z\\.]{3}) #([0-9]+) ([A-Z' \\.-]+) .*", ## visiting team, away player
                                 "([A-Z\\.]{3}) #([0-9]+) ([A-Z' \\.-]+).*"), ## home player
                          "\\1;\\2;\\3;\\4;\\5;\\6;\\7",
                          playbyplay$etext[c1]),
                     p.split, 7)
        
        firstplayer <- paste(p1[3,], p1[4,]); secondplayer <- paste(p1[6,], p1[7,])
        firstteam <- p1[2,]; secondteam <- p1[5,]
        firstteamwon <- which(firstteam == playbyplay$ev.team[c1])
        secondteamwon <- which(secondteam == playbyplay$ev.team[c1])
        
        ## homepicks <- which(playbyplay$ev.team[c1] == teams[2])
        ## awaypicks <- which(playbyplay$ev.team[c1] == teams[1])
        
        playbyplay$ev.player.1[c1[firstteamwon]] <- firstplayer[firstteamwon]
        playbyplay$ev.player.2[c1[firstteamwon]] <- secondplayer[firstteamwon]
        playbyplay$ev.player.1[c1[secondteamwon]] <- secondplayer[secondteamwon]
        playbyplay$ev.player.2[c1[secondteamwon]] <- firstplayer[secondteamwon]
    
        ## playbyplay$ev.player.1[c1[awaypicks]] <- awayplayer[awaypicks]
        ## playbyplay$ev.player.2[c1[awaypicks]] <- homeplayer[awaypicks]
        
        playbyplay$homezone[c1] <- p1[1,]  ## This zone is relative to the winning team; fixed at the end.
        playbyplay$etext[c1] <- ""
    }
  
  # GIVE
  #num;name;zone
    c1 <- which(playbyplay$etype=="GIVE")
    if (length(c1)>0) {
        p1 <- sapply(gsub(".*#([0-9]+) ([A-Z' \\.-]+), ([A-Za-z]+)\\..*", "\\1;\\2;\\3",
                          playbyplay$etext[playbyplay$etype=="GIVE"]),
                     p.split, 3)
        
                                        #                                function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.1[playbyplay$etype=="GIVE"] <- paste(p1[1,], p1[2,])
        playbyplay$homezone[playbyplay$etype=="GIVE"] <- p1[3,]
        playbyplay$etext[c1] <- ""
    }
  
  # TAKE
  #num;name;zone
    c1 <- which(playbyplay$etype=="TAKE")
    if (length(c1)>0) {
        p1 <- sapply(gsub(".*#([0-9]+) ([A-Z' \\.-]+), ([A-Za-z]+)\\..*", "\\1;\\2;\\3",
                          playbyplay$etext[playbyplay$etype=="TAKE"]),
                     p.split, 3)
        playbyplay$ev.player.1[playbyplay$etype=="TAKE"] <- paste(p1[1,], p1[2,])
        playbyplay$homezone[playbyplay$etype=="TAKE"] <- p1[3,]
        playbyplay$etext[c1] <- ""
    }

  # PENL
    c1 <- which(playbyplay$etype=="PENL" & grepl("Drawn By:", playbyplay$etext))
    if (length(c1) > 0) {
        etext <- playbyplay$etext[c1]   #2014-10-27, missing zones
        indicated.zone <- grepl("Zone", etext)
        etext[!indicated.zone] <- gsub("min)", "min), Unk. Zone", etext[!indicated.zone])
        
        ##                    Number Name, penalty, zone, number name
        rep.pattern <- ".*#([0-9]+) ([A-Z' \\.-]+), ([A-Za-z\\(\\)0-9 :#-]+),? ([A-Za-z]+)\\..*#([0-9]+) ([A-Z' \\.-]+).*"
            p1 <- sapply(gsub(rep.pattern, "\\1;\\2;\\3;\\4;\\5;\\6",
                              etext),
                         p.split, 6)
        
        playbyplay$ev.player.1[c1] <- paste(p1[1,], p1[2,])
        playbyplay$ev.player.2[c1] <- paste(p1[5,], p1[6,])
        playbyplay$homezone[c1] <- p1[4,]
        playbyplay$type[c1] <- p1[3,]
    }
    
    c2 <- which(playbyplay$etype=="PENL" & !grepl("Drawn By:", playbyplay$etext))
    if (length(c2) > 0) {
        
        etext <- playbyplay$etext[c2]   #2014-10-27, missing zones
        indicated.zone <- grepl("Zone", etext)
        etext[!indicated.zone] <- gsub("min)", "min), Unk. Zone", etext[!indicated.zone])
        
        p1 <- sapply(gsub(".*#([0-9]+) ([A-Z' \\.-]+), ([A-Za-z\\(\\)0-9 :#-]+),? ([A-Za-z]+)\\..*",
                          "\\1;\\2;\\3;\\4",
                          etext),
                     function(tt) {
                         out1 <- unlist(strsplit (tt, ";"))
                         if (length(out1) == 1) out1 <- c("","","Bench minor", "")
                         out1
                     })
        playbyplay$ev.player.1[c2] <- paste(p1[1,], p1[2,])
        playbyplay$homezone[c2] <- p1[4,]
        playbyplay$type[c2] <- p1[3,]
    }

    playbyplay$etext[union(c1,c2)] <- ""


  #HIT
    c1 <- which(playbyplay$etype=="HIT")
    if (length(c1)>0) {
        p1 <- sapply(gsub(".*#([0-9]+) ([A-Z' \\.-]+) HIT.*#([0-9]+) ([A-Z' \\.-]+), ([A-Za-z]+).*", "\\1;\\2;\\3;\\4;\\5",
                          playbyplay$etext[playbyplay$etype=="HIT"]),
                     p.split, 5)
                                        #                 function(tt) unlist(strsplit (tt, ";")))
        playbyplay$ev.player.1[playbyplay$etype=="HIT"] <- paste(p1[1,], p1[2,])
        playbyplay$ev.player.2[playbyplay$etype=="HIT"] <- paste(p1[3,], p1[4,])
        playbyplay$homezone[playbyplay$etype=="HIT"] <- p1[5,]
        playbyplay$etext[c1] <- ""
    }

  ########################################################################
  # replace number-last with number-first-last.

  #on.ice.players <- NULL
  #for (kk in 6:17) on.ice.players <- c(on.ice.players, playbyplay[,kk])
  #on.ice.players <- unique(on.ice.players)
  

  # seconds in game.
    playbyplay$seconds <- playbyplay$seconds + 1200*(playbyplay$period-1)

  # add approximate changes.
    orig.rec <- dim(playbyplay)[1]
    for (kk in 1:(orig.rec-1)) {
        past <- na.omit(unlist(playbyplay[kk,6:17]))
        future <- na.omit(unlist(playbyplay[kk+1,6:17]))
        if (playbyplay[kk,3] < playbyplay[kk+1,3]) 
            if (length(past) != length(future)) {
                new.record <- playbyplay[kk,]
                new.record[1,4] <- "CHANGE"
                new.record[1,5] <- "CHANGE"
                new.record$ev.team <- ""
                new.record[1,3] <- mean(c(playbyplay[kk,3], playbyplay[kk+1,3]))
                playbyplay <- rbind(playbyplay, new.record)
            } else if (any(past != future)) {
                new.record <- playbyplay[kk,]
                new.record[1,4] <- "CHANGE"
                new.record[1,5] <- "CHANGE"
                new.record$ev.team <- ""
                new.record[1,3] <- mean(c(playbyplay[kk,3], playbyplay[kk+1,3]))
                playbyplay <- rbind(playbyplay, new.record)
            }
    }
    playbyplay <- playbyplay[order(playbyplay$seconds),]
    playbyplay$event <- 1:nrow(playbyplay)
    
  #remove text.
  ##playbyplay <- playbyplay[,-5]

  #flip zone to home zone referent.
    flip.1 <- playbyplay$ev.team == teams[1] & playbyplay$homezone=="Off"
    flip.2 <- playbyplay$ev.team == teams[1] & playbyplay$homezone=="Def"
    playbyplay$homezone[flip.1] <- "Def"
    playbyplay$homezone[flip.2] <- "Off"
    
    playbyplay$homezone[playbyplay$etype=="CHANGE"] <- "Neu"
    playbyplay$homezone[playbyplay$etype=="STOP"] <- "Unk"
    
    stops <- which(playbyplay$ev.team != "ICI" & playbyplay$homezone == ""); playbyplay$homezone[stops] <- playbyplay$homezone[stops+1]
    
    playbyplay$homezone[playbyplay$etype %in% c("SHOT", "BLOCK", "GOAL", "MISS") & playbyplay$ev.team == teams[1]] <- "Def"
    playbyplay$homezone[playbyplay$etype %in% c("SHOT", "BLOCK", "GOAL", "MISS") & playbyplay$ev.team == teams[2]] <- "Off"

  ##faceoffs follow stoppages, so cut them. -- not any more, 2014-12-03
  ##playbyplay <- playbyplay[playbyplay$etype != "STOP",]
  
    output <- list(playbyplay=playbyplay,
                   players=players,
                   teams=teams,
                   date=date)
    return(output)
}



process.es.new <- function(es.html) {
    ## load("nhlr-data/20072008-20001.RData")
    ## es.html=game.rec$es; pl.html=game.rec$pl

    es.html <- gsub ("\uC9","E",es.html)
    es.html <- gsub ("\uCA","E",es.html)
    es.html <- gsub ("\uC8","E",es.html)

    divpoint <- grep("TEAM SUMMARY", es.html)
    if (length(divpoint) > 0) es.html <- es.html[divpoint[1]:length(es.html)]
    
    rel.rows.1 <- grep("<tr", es.html)
    rel.rows.2 <- sort(c(outer(rel.rows.1, 1:3, "+")))
    picks <- es.html[rel.rows.2[grep(" *<td [awc ].*", es.html[rel.rows.2])]]
    
    picks.1 <- gsub(" *<td.*> *([0-9]{1,2})</td>", "n;\\1", picks)
    picks.2 <- gsub(" *<td.*>([CLRDG])</td>", "p;\\1", picks.1)
    picks.3 <- gsub(" *<td.*>([A-Za-z'\\.\\(\\), -]{6,})</td>", "m;\\1", picks.2)
    picks.3 <- picks.3[substr(picks.3, 1, 1) != "<"]
    
    if (length(picks)>2) {
        rk <- substr(picks.3,1,1)
        nms <- substr(picks.3,3,nchar(picks.3))
        lr <- length(rk)
        rk1 <- rbind(rk[1:(lr-2)], rk[2:(lr-1)], rk[3:lr])
        sts <- which(apply(rk1 == c("n","p","m"), 2, prod)>0)
        
        division <- grep("TEAM TOTALS", picks.3)[1]
    } else {nms <- rep("",3); sts <- NULL; division=3}
    
    names.mat <- data.frame(number=nms[sts],
                            pos=nms[sts+1],
                            lastfirst=toupper(nms[sts+2]),
                            hometeam=1*(sts>division), stringsAsFactors=FALSE)
    names.mat <- names.mat[grepl(",", names.mat$lastfirst),]
    names.mat$last <- gsub("(.*), .*", "\\1", names.mat$lastfirst)
    names.mat$first <- gsub(".*, (.*)", "\\1", names.mat$lastfirst)
    names.mat$numlast <- paste(names.mat$number, names.mat$last)
    names.mat$numfirstlast <- paste(names.mat$number, names.mat$first, names.mat$last)
    
    ## remove duplicates.
    lastfirst <- unique(names.mat$lastfirst)
    if (length(lastfirst) != dim(names.mat)[1]) for (mm in lastfirst) {
        rws <- rev(which(names.mat$lastfirst == mm))
        if (length(rws)>1) names.mat <- names.mat[-rws[2:length(rws)],]
    }
  
    names.mat <- names.mat[order(names.mat$hometeam, as.numeric(names.mat$number)),]
    return(names.mat)
  
}



integrate.new.pieces <- function (es.html, pl.html) {
  #load("nhlr-data/20072008-20001.RData")
  #es.html=game.rec$es; pl.html=game.rec$pl
  
  es.table <- try(process.es.new(es.html), TRUE)
  if (class(es.table)=="try-error") es.table <- NULL
  pl.table <- try(process.pl.new(pl.html), TRUE)
  if (class(pl.table)=="try-error") pl.table <- NULL

  game.times <- get.game.start.end(es.html)

  # replace with full names.
  for (ii in which(colnames(pl.table$playbyplay) %in% c("ev.player.1", "ev.player.2", "ev.player.3")))
    pl.table$playbyplay[,ii] <- es.table$numfirstlast[match(pl.table$playbyplay[,ii],
                                                             es.table$numlast)]
    
  pl.table$players <- es.table
  pl.table$game.times <- game.times
  return(pl.table)
  
}

integrate.old.pieces <- function (home.image, away.image,
                                  es.html, pl.html, game) {
    ## load("nhlr-data/20052006-20051.RData");
    ## home.image=game.rec$imh; away.image=game.rec$imv; es.html=game.rec$es; pl.html=game.rec$pl; game="20003"

    es.table <- try(process.es.new(es.html), TRUE)
    if (class(es.table)=="try-error") es.table <- NULL
    pl.table <- try(process.pl.old(pl.html), TRUE)
    if (class(pl.table)=="try-error") pl.table <- NULL

    game.times <- get.game.start.end(es.html)
    
    if (length(pl.table) > 0) {
        
        if (substr(game,1,1)=="2") pl.table$playbyplay <- pl.table$playbyplay[pl.table$playbyplay$seconds <= 3900,]
        
                                        # replace with full names.
        for (ii in which(colnames(pl.table$playbyplay) %in% c("ev.player.1", "ev.player.2", "ev.player.3")))
            pl.table$playbyplay[,ii] <- es.table$numfirstlast[match(pl.table$playbyplay[,ii],
                                                                    es.table$numlast)]
        for (ii in which(colnames(pl.table$playbyplay) %in% c(paste0("a",1:6), paste0("h",1:6))))
            pl.table$playbyplay[pl.table$playbyplay$etype=="GOAL",ii] <-
                es.table$numfirstlast[match(pl.table$playbyplay[pl.table$playbyplay$etype=="GOAL",ii],
                                            es.table$numlast)]
        es.home <- es.table[es.table$hometeam==1,]; es.home <- rbind(es.home, "")
        es.away <- es.table[es.table$hometeam==0,]; es.away <- rbind(es.away, "")
        
        if (length(home.image) == 0) warning ("Home image file does not exist.")
        if (length(away.image) == 0) warning ("Away image file does not exist.")
        
        home.players <- try(pick.out.features (pl.table$playbyplay$seconds, home.image), TRUE)
        if (class(home.players)=="try-error") home.players <- array(0, c(6, length(pl.table$playbyplay$seconds)))
        away.players <- try(pick.out.features (pl.table$playbyplay$seconds, away.image), TRUE)
        if (class(away.players)=="try-error") away.players <- array(0, c(6, length(pl.table$playbyplay$seconds)))
        
        home.players[home.players==0] <- nrow(es.home)
        away.players[away.players==0] <- nrow(es.away)
        
                                        # Players on ice.
        home.players.names <- t(array(es.home$numfirstlast[home.players], dim(home.players)))
        away.players.names <- t(array(es.away$numfirstlast[away.players], dim(away.players)))
        for (cc in c(paste0("a",1:6), paste0("h",1:6))) pl.table$playbyplay[,cc] <- as.character(pl.table$playbyplay[,cc])
        
        pl.table$playbyplay[pl.table$playbyplay$etype!="GOAL", paste0("a",1:6)] <- away.players.names[pl.table$playbyplay$etype!="GOAL",]
        pl.table$playbyplay[pl.table$playbyplay$etype!="GOAL", paste0("h",1:6)] <- home.players.names[pl.table$playbyplay$etype!="GOAL",]


  #Add changes.
        orig.rec <- dim(pl.table$playbyplay)[1]
        for (kk in 1:(orig.rec-1)) {
            past <- na.omit(unlist(pl.table$playbyplay[kk,6:17-1]))
            future <- na.omit(unlist(pl.table$playbyplay[kk+1,6:17-1]))
            if (pl.table$playbyplay[kk,3] < pl.table$playbyplay[kk+1,3]) 
                if (length(past) != length(future)) {
                    new.record <- pl.table$playbyplay[kk,]
                    new.record[1,4] <- "CHANGE"
                    new.record$ev.team <- ""
                    new.record[1,3] <- mean(c(pl.table$playbyplay[kk,3], pl.table$playbyplay[kk+1,3]))
                    pl.table$playbyplay <- rbind(pl.table$playbyplay, new.record)
                } else if (any(sort(past) != sort(future))) {
                    new.record <- pl.table$playbyplay[kk,]
                    new.record[1,4] <- "CHANGE"
                    new.record$ev.team <- ""
                    new.record[1,3] <- mean(c(pl.table$playbyplay[kk,3], pl.table$playbyplay[kk+1,3]))
                    pl.table$playbyplay <- rbind(pl.table$playbyplay, new.record)
                }
        }
        pl.table$playbyplay <- pl.table$playbyplay[order(pl.table$playbyplay[,3]),]
        pl.table$playbyplay[,1] <- 1:dim(pl.table$playbyplay)[1]
        pl.table$playbyplay$homezone[pl.table$playbyplay$etype=="CHANGE"] <- "Neu"
        
        pl.table$players <- es.table
        pl.table$game.times <- game.times
    } else {pl.table <- list(players=NULL, playbyplay=NULL, teams=NULL, date=NULL, game.times=game.times)}  
    
    return(pl.table)

}

