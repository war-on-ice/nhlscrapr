
# In this version: no reflections, no x-y bias. Just find the best constrained solution that gives a AA matrix.
# ACT, 6-11-14

#library(nhlscrapr)

season.block.2d <- function (season) {
  year <- as.numeric(substr(season,1,4))
  year.set <- seq(2008, 2013)
  output <- 0*year
  for (yy in year.set) output <- output + 1*(year >= yy)
  output
}

make.AA <- function (pp, moves, states=16) {  # pp=rep(0, nrow(moves))
  AA <- 0*diag(states)
  AA[moves[,1] + states*(moves[,2]-1)] <- pp
  diag(AA) <- 1-colSums(AA)
  AA[AA<0] <- 0
  return(AA)
}

#this one solves the reverse process, which gives us the fix.
solver.with.nlm <- function (obs.set, true.set, moves, twoway=FALSE) {
  ## obs.set=portions.full[,1]; true.set=portions.full[,2]; moves=pulls
  
  #This is the easiest way to fix it for now!
  thing.to.opt <- function (pp) {
    AA <- make.AA(pp, moves)
    pars <- c(diag(AA), pp)
    1000*sum((AA%*%obs.set - true.set)^2) +
      sum(pp^2) + #keep small if possible!
        1000*sum(abs(pars)*(pars < 0)) +  #can't be negative.
          1000*sum((pars-1)*(pars > 1))   #can't be bigger than the whole share!
  }
  
  getme <- suppressWarnings(nlm (thing.to.opt, rep(0, nrow(moves))))
  ## print (cbind(AA%*%getme$estimate + true.set, obs.set, true.set))

  pp <- getme$estimate
  if (twoway) {
    dd <- nrow(moves)/2
    pp <- pmax (pp-pp[c(dd+1:dd, 1:dd)], 0)
  }
  list(AA=make.AA(pp, moves), estimate=pp, minimum=getme$minimum)
}

sorter.table <- function (event.table,
                          input=list(player1="NYR",
                               season="20132014",
                               shottype="Wrist")) 
  cbind(homefor=tabulate(event.table$loc.section[event.table$hometeam == input$player1 &
          event.table$ev.team == input$player1], 16),
        awayfor=tabulate(event.table$loc.section[event.table$awayteam == input$player1 &
          event.table$ev.team == input$player1], 16),
        homeagn=tabulate(event.table$loc.section[event.table$hometeam == input$player1 &
          event.table$ev.team != input$player1], 16),
        awayagn=tabulate(event.table$loc.section[event.table$awayteam == input$player1 &
          event.table$ev.team != input$player1], 16))


get.move.matrix <- function (shots,
                             input=list(player1="NYR",
                               season="20132014",
                               shottype="Wrist")) {

  if (nrow(shots) > 10) {
    total.awayhome.four <- sorter.table (shots, input)
    total.awayhome <- total.awayhome.four[,c(1,2)] + total.awayhome.four[,c(3,4)]
    portions.full <- cbind(homefake=total.awayhome[,1]/sum(total.awayhome[,1]),
                           awaytrue=total.awayhome[,2]/sum(total.awayhome[,2]))
    
                                        #pulls and pushes are included data sets.
    trials <- list (solver.with.nlm (portions.full[,1], portions.full[,2], nhlscrapr::pulls),
                    solver.with.nlm (portions.full[,1], portions.full[,2], nhlscrapr::pushes),
                    solver.with.nlm (portions.full[,1], portions.full[,2],
                                     cbind(c(nhlscrapr::pulls[,1], nhlscrapr::pushes[,1]),
                                           c(nhlscrapr::pulls[,2], nhlscrapr::pushes[,2])),
                                     twoway=TRUE))
    
    pick <- which.min (sapply (trials, function (tt) tt$minimum))
    
    move.matrix <- trials[[pick]]$AA
    return(move.matrix)
  } else return (diag(16))

}


single.subzone.move <- function (shots,
                                 input=list(player1="NYR",
                                   season="20132014",
                                   shottype="Wrist"),
                                 move.matrix) {
  ##input=list(player1="T.B", season="20132014", shottype="Wrist"); scatter.grid=indata$scatter.grid
  ##input=list(player1="PHI", season="20102011", shottype="Backhand")
  ##shots = season.data[[ss]][rws,]; input = list(player1=mm)
  ##shots=subset(shots.split[[prop[[1]]]], type == prop[[2]]); input=list(player1=prop[[3]]); move.matrix=zone.adjust$AA.matrices[[rr]]

  
  
  for (aa in 1:nrow(move.matrix)) {
    selection <- which(shots$loc.section == aa & shots$hometeam == input$player1)
    newpicks <- sample(1:ncol(move.matrix), length(selection), prob=move.matrix[,aa], replace=TRUE)
    
    shots$new.loc.section[selection] <- newpicks
    for (ss in unique(newpicks)) if (ss != aa) {
      poolpicks <- which(shots$loc.section == ss)   #Don't care about the actual coordinate for now.
      if (length(poolpicks) > 0) {  
        shots[selection[newpicks==ss],c("newxc","newyc")] <-
          shots[sample(poolpicks, sum(newpicks==ss), replace=TRUE),c("xcoord","ycoord")]
      }
    }
  }

  output <- shots[shots$hometeam == input$player1, c("new.loc.section","newxc","newyc")]
  return(output)
  
}



update.adjusted.subzone <- function (shot.data, zone.adjust) {

  shot.data$seasonblock <- season.block.2d(shot.data$season)
  if (max(shot.data$seasonblock) > 0) {
    seasonblocks <- 1:max(shot.data$seasonblock)
    shot.data$type[shot.data$type == "Snap"] <- "Wrist"
  
    shots.split <- lapply(seasonblocks,
                          function (bb) shot.data[shot.data$seasonblock==bb,])

    for (rr in 1:nrow(zone.adjust$split.table)) {
      prop <- zone.adjust$split.table[rr,]
  ##  message(prop)
      if (length(shots.split) >= prop[[1]]) {
        sub1 <- shots.split[[prop[[1]]]][shots.split[[prop[[1]]]]$type == prop[[2]],] 
        m1 <- single.subzone.move (sub1,
                                   input=list(player1=prop[[3]]),
                                   zone.adjust$AA.matrices[[rr]])
        
        shots.split[[prop[[1]]]][shots.split[[prop[[1]]]]$type == prop[[2]] &
                                 shots.split[[prop[[1]]]]$hometeam == prop[[3]],
                                 c("new.loc.section","newxc","newyc")] <- m1
      }
    }
    
    for (bb in seasonblocks) shot.data[shot.data$seasonblock==bb, ] <- shots.split[[bb]]
  }
  
  return(shot.data[,c("new.loc.section","newxc","newyc")])

}


make.adjusted.subzone.homeaway.table <- function (shot.data) {

  message ("Creating distance adjustment tables.")
  shot.data$seasonblock <- season.block.2d(shot.data$season)
  seasonblocks <- 1:max(shot.data$seasonblock)
  shot.data$type[shot.data$type == "Snap"] <- "Wrist"
  shots.split <- lapply(seasonblocks,
                        function (bb) shot.data[shot.data$seasonblock==bb,])
  
  split.table <- expand.grid (seasonblock=seasonblocks, type=unique(shot.data$type),
                              teams=unique(shot.data$hometeam), stringsAsFactors = FALSE)

  AA.matrices <- lapply (1:nrow(split.table), function(rr) {
    prop <- split.table[rr,]
    message (prop)
    sub1 <- shots.split[[prop[[1]]]][shots.split[[prop[[1]]]]$type == prop[[2]],]
    get.move.matrix (sub1,
                     input=list(player1=prop[[3]]))
  })

  return (list(split.table=split.table, AA.matrices=AA.matrices))
  
}

create.subzone.adjustments <- function (grand.data,
                                        zone.adjust=NULL,
                                        force.new.zone.adjust=FALSE) { #grand.data
  
  message ("Correcting For Sub-Zone Anomalies.")
  grand.data$new.loc.section <- grand.data$loc.section
  grand.data$newxc <- grand.data$xcoord 
  grand.data$newyc <- grand.data$ycoord 

  etypes <- c("GOAL","SHOT","MISS")
  shot.rows <- which (grand.data$etype %in% etypes)
  shot.data <- grand.data[shot.rows,
                          c("season", "type", "ev.team", "hometeam", "awayteam",
                            "loc.section", "xcoord", "ycoord",
                            "new.loc.section", "newxc", "newyc")]

  if (is.null(zone.adjust)) zone.adjust <- nhlscrapr::zone.adjust.prefab  #make.adjusted.subzone.homeaway.table (shot.data)
  if (force.new.zone.adjust) zone.adjust <- make.adjusted.subzone.homeaway.table (shot.data)
  ## zone.adjust.prefab <- zone.adjust; save(zone.adjust.prefab, file="zone-adjust-prefab.RData")
  
  zone.one <- update.adjusted.subzone (shot.data, zone.adjust)
  grand.data[shot.rows, c("new.loc.section","newxc","newyc")] <- zone.one

  return(grand.data)

}
