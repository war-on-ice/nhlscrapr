
match.xy <- function (pl.table, json.object) {
  #json.object=game.rec$xy
  
  #just get the relevant stuff.
  if (is.null(json.object$data)) ob1 <- json.object$game$plays$play else ob1 <- json.object$data$game$plays$play

  xy.event.table <-
    t(sapply(ob1, function(pp)
             c(ifelse(!is.null(pp$ycoord), pp$ycoord, NA),
               ifelse(!is.null(pp$xcoord), pp$xcoord, NA),
               ifelse(!is.null(pp$period), pp$period, NA),
               ifelse(!is.null(pp$time), pp$time, NA),
               ifelse(!is.null(pp$type), pp$type, NA))))

  if (length(xy.event.table)>0) {
    s1 <- 60*as.numeric(substr(xy.event.table[,4],1,2))+as.numeric(substr(xy.event.table[,4],4,5))
    if (s1[2]-s1[1]<0 & s1[3]-s1[2]<0) s1 <- 1200-s1
  
    xy.frame <- data.frame(xcoord=as.numeric(xy.event.table[,2]),
                           ycoord=as.numeric(xy.event.table[,1]),
                           seconds=1200*(as.numeric(xy.event.table[,3])-1)+s1,
                           etype=toupper(xy.event.table[,5]))
    
    rows <- match(xy.frame$seconds, pl.table$playbyplay$seconds)
    pl.table$playbyplay$xcoord[rows[!is.na(rows)]] <- xy.frame$xcoord[!is.na(rows)]
    pl.table$playbyplay$ycoord[rows[!is.na(rows)]] <- xy.frame$ycoord[!is.na(rows)]
  }
  
  return(pl.table)

}
