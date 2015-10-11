
#########################################################################
#
# Goldsberry plot of NHL shot data
#

#######################################################################
#Make hexagons.
#turn90 <- function (nby2) cbind(nby2[,2], -nby2[,1])

ordinal.maker <- function (vec, cuts=quantile(vec, c(0, 0.35, 0.7), na.rm=TRUE)) {
    apply(1*outer (vec, cuts, ">="), 1, sum, na.rm=TRUE)
}
ordinal.maker.zeroes <- function (vec, cuts=quantile(vec[vec>0], c(0, 0.35, 0.7))) {
    r1 <- 0*vec
    r1[vec>0] <- apply(1*outer (vec[vec>0], cuts, ">="), 1, sum, na.rm=TRUE)
    r1
}

hexagon.coords.single <- function (x=0, y=0, radius=2.01) {
    t(radius*rbind(x=c(1,1,0,-1,-1,0),
                   y=c(1/sqrt(3), -1/sqrt(3), -2/sqrt(3),
                       -1/sqrt(3), 1/sqrt(3), 2/sqrt(3))) + c(x,y))
}
hexagon.coords.auto <- function (xygrid, relative.size=0.8) {

    output <- matrix(NA, nrow=nrow(xygrid)*7, ncol=2)
    new.size <- relative.size*max(abs(xygrid[1,1] - xygrid[2,1]), abs(xygrid[1,2] - xygrid[2,2]))/2
    for (kk in 1:nrow(xygrid))
        output[1:6 + (kk-1)*7,] <- hexagon.coords.single (xygrid[kk,1], xygrid[kk,2], new.size)
    return(output)
}

hexagon.coords.ordinal <- function (scatter.grid, sizes) {
    new.size <- rep(0, length(sizes))
    new.size[sizes >= 1] <- new.size[sizes >= 1] + 0.3
    new.size[sizes >= 2] <- new.size[sizes >= 2] + 0.3
    new.size[sizes >= 3] <- new.size[sizes >= 3] + 0.3
    new.size <- new.size * scatter.grid$radius
    output <- matrix(NA, nrow=length(sizes)*7, ncol=2)
    for (kk in 1:length(sizes))
        output[1:6 + (kk-1)*7,] <- hexagon.coords.single (scatter.grid$grid[kk,2], 
                                                          scatter.grid$grid[kk,1],
                                                          new.size[kk])
    return(output)
}
hexagon.coords <- function (...) hexagon.coords.ordinal(...)

#######################################################################
#Bin into quadrilaterals.

in.triangle <- function (xyp, tr) {
  area <- (-tr[5]*tr[3] + tr[4]*(tr[3]-tr[2]) + tr[1]*(-tr[6]+tr[5]) + tr[2]*tr[6])/2
  ss <- 1/2/area * (tr[4]*tr[3] - tr[1]*tr[6] + (tr[6]-tr[4])*xyp[,1] + (tr[1]-tr[3])*xyp[,2])
  tt <- 1/2/area * (tr[1]*tr[5] - tr[4]*tr[2] + (tr[4]-tr[5])*xyp[,1] + (tr[2]-tr[1])*xyp[,2])
  output <- (ss > 0 & tt > 0 & 1 > ss+tt)
  return(output)
}
in.tri.rev <- function (tr=matrix(c(0,0.5,1, 0,1,0), nrow=3), xy.points) in.triangle (xy.points, tr)
pick.section.side <- function (xy.points) {
  data(quadsarrayplot)
  
  in.1 <- apply(quadsarrayplot[1:3,,], 3, in.tri.rev, xy.points)
  in.2 <- apply(quadsarrayplot[c(1,3,4),,], 3, in.tri.rev, xy.points)
  picks <- in.1 | in.2
  picks[is.na(picks)] <- FALSE
  
  picker <- function (row) if (sum(row)>0) min(which(row)) else 0
  sections <- apply (picks, 1, picker)
  return(sections)
}


############################################################################
# Point grid for binning.
# We can make this fresh if we need to from here, but by default it's saved in the package.

point.grid <- function (ybins=31, xrange=c(-1,100), yrange=c(-42, 42), save.to.file=TRUE) {

  radius <- 85/(2*ybins)
  initial.y <- seq(yrange[1]+radius, yrange[2]-radius, length=ybins)
  shifted.y <- seq(yrange[1], yrange[2], length=ybins+1)
  xvalue <- xrange[1]+0.01
  scatter.grid <- NULL
  while (xvalue < xrange[2]) {
    scatter.grid <- rbind(scatter.grid,
                          cbind(initial.y, xvalue),
                          cbind(shifted.y, xvalue + sqrt(3)*radius))
    xvalue <- xvalue + sqrt(3)*radius*2
  }
  
  scatter.grid <- data.frame(plotting.x=scatter.grid[,2], plotting.y=scatter.grid[,1])
  scatter.grid <- subset(scatter.grid, plotting.x < xrange[2])

  uniques <- t(expand.grid(newxc=seq(xrange[1], xrange[2]),
                           newyc=seq(yrange[1], yrange[2])))

  dist2 <- function (single.plot) which.min((scatter.grid[,1] - single.plot[1])^2 +
                                            (scatter.grid[,2] - single.plot[2])^2)
  bins <- apply(uniques, 2, dist2)

  zone.section <- pick.section.side (scatter.grid[,2:1])

  three.superblocks <- c(1,1,1,  1,2,2,2,1,  1,2,3,3,2,1,  1,1)
  zone.section.bigblock <- three.superblocks[zone.section]

  hexgrid <- cbind (scatter.grid, zone.section, zone.section.bigblock)
    
  scatter.grid <- list(grid=hexgrid,
                       xymap=data.frame(t(uniques), hexbin=bins),
                       ybins=ybins,
                       radius=radius)
  if (save.to.file) save (scatter.grid, file="hexgrid.RData")
  
  return(scatter.grid)

}


###################################################################################
# Actual binning.
# must go faster!

## xycoords <- grand.data
hexbin <- function (xycoords) {
    left_join (select(xycoords, newxc, newyc), scatter.grid$xymap) %>% select (hexbin) %>% c
}

nhl.hexbin <- function (xycoords) {
    basic <- data.frame(hexbin=1:nrow(scatter.grid$grid))
    bincount <- data.frame(hexbin(xycoords)) %>% group_by (hexbin) %>% summarize (count=n())
    totes <- left_join (basic, bincount)
    totes$count[is.na(totes$count)] <- 0
    totes$count
}


nhl.zonebin.prime <- function (bin.counts, use.superblocks=FALSE) {
  #sum up over bins in each zone. Redistribute over bins.
    
    if (use.superblocks) {
        basic <- data.frame(hexbin=1:max(scatter.grid$grid$zone.section.bigblock))
        count <- group_by (data.frame(hexbin=scatter.grid$grid$zone.section.bigblock,
                                      count=bin.counts), hexbin) %>% summarize (count=sum(count))
    } else {
        basic <- data.frame(hexbin=1:max(scatter.grid$grid$zone.section))
        count <- group_by (data.frame(hexbin=scatter.grid$grid$zone.section,
                                      count=bin.counts), hexbin) %>% summarize (count=sum(count))
    }
    retval <- (left_join (basic, count))$count
    return(retval)
    
}

nhl.zonebin <- function (bin.counts, scatter.grid, use.superblocks=FALSE) {
  #sum up over bins in each zone. Redistribute over bins.
    bin.overall <- nhl.zonebin.prime (bin.counts, use.superblocks)
    output <- rep(NA, nrow(scatter.grid$grid))
    if (use.superblocks) {
        for (bb in 1:length(bin.overall)) output[scatter.grid$zone.section.bigblock == bb] <- bin.overall[bb]
    } else {
        for (bb in 1:length(bin.overall)) output[scatter.grid$zone.section == bb] <- bin.overall[bb]
    }
    output
}








rink.hexplot <- function (scatter.grid, sizes, colors, bordercolor=NA, ...) {

  #sizes is now ordinal: 0=none, 1=0.4, 2=0.8, 3=0.8 black border
    par(mar=c(0, 0, 3, 0))
    rink.plot(...) #...)
    hex.coords <- hexagon.coords.ordinal(scatter.grid, sizes)
    colors[sizes==0] <- 0
    
    bordercolor <- rep(bordercolor, length(colors)); bordercolor[sizes == 0] <- NA
    polygon(hex.coords, col=colors, border=bordercolor, lwd=2)

}

rink.hexplot.auto <- function (scatter.grid, sizes, colors, ...) {
    rink.hexplot (scatter.grid, sizes=ordinal.maker.zeroes(sizes), colors, ...)
}

shot.bin.set <- function (event.df,
                          scatter.grid=point.grid(),
                          coordnames=c("ycoord","xcoord")) {
    ## event.df=player.events; coordnames=coordset()
  #message(coordnames)
  all.shots <- nhl.hexbin (event.df[,coordnames], scatter.grid)
  all.goals <- nhl.hexbin (subset(event.df, etype=="GOAL")[,coordnames], scatter.grid)
  
  bin.shots <- nhl.zonebin (all.shots, scatter.grid)
  bin.goals <- nhl.zonebin (all.goals, scatter.grid)

  superbin.shots <- nhl.zonebin (all.shots, scatter.grid, use.superblocks=TRUE)
  superbin.goals <- nhl.zonebin (all.goals, scatter.grid, use.superblocks=TRUE)
  

  return(cbind(all.shots=all.shots,
               all.goals=all.goals,
               
               binned.frac=bin.goals/bin.shots,
               
               bin.shots=bin.shots,
               bin.goals=bin.goals,

               superbin.shots=superbin.shots,
               superbin.goals=superbin.goals

               ))
  
}

shot.bin.set.blocks <- function (event.df,
                                 scatter.grid=point.grid(),
                                 coordnames=c("ycoord","xcoord")
                                 , use.superblocks=FALSE
                                 ) {

  #message(coordnames)
  all.shots <- nhl.zonebin.prime(nhl.hexbin (event.df[,coordnames], scatter.grid),
                                 scatter.grid, use.superblocks)
  all.goals <- nhl.zonebin.prime(nhl.hexbin (subset(event.df, etype=="GOAL")[,coordnames], scatter.grid),
                                 scatter.grid, use.superblocks)
  
  return(cbind(all.shots=all.shots,
               all.goals=all.goals))
  
}









