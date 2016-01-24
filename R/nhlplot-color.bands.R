

absolute.spectrum <- function (...) {
  par(mar=c(0,3,0,0))
  yy <- 0:30/100
  xlim1 <- 0.3*c(-0.5, 0.5)/7/1.7
  plot (rep(0, length(yy)), yy, ty="n", xlim=xlim1, axes=FALSE, ...) #pch=20, cex=3, col=absolute.binning(xx), 
  polygon(hexagon.coords.auto(cbind(0,yy)),
          col=absolute.binning(yy),
          border=0)
  axis(2, seq(0, 0.3, by=0.03))
}

relative.spectrum <- function (...) {
  par(mar=c(0,3,0,0))
  yy <- seq(0, 30/12, length=30)
  xlim1 <- 30/12*c(-0.5, 0.5)/7/1.7
  plot (rep(0, length(yy)), yy, ty="n", xlim=xlim1, axes=FALSE, ...)   #pch=20, cex=3, col=relative.binning(xx), 
  polygon(hexagon.coords.auto(cbind(0,yy)),
          col=relative.binning(yy),
          border=0)
  abline(h=1, col=2, lwd=2)
  axis(2, seq(0, 3, by=0.4))
}

z.score.spectrum <- function (...) {
  par(mar=c(0,3,0,0))
  yy <- seq(-8, 8, length=30)
  xlim1 <- 16*c(-0.5, 0.5)/7/1.7
  plot (rep(0, length(yy)), yy, ty="n", xlim=xlim1, axes=FALSE, ...)   #pch=20, cex=3, col=relative.binning(xx), 
  polygon(hexagon.coords.auto(cbind(0,yy)),
          col=z.score.binning(yy),
          border=0)
  abline(h=c(-2,2), col=2, lwd=2)
  axis(2, seq(-8, 8, by=2))
}

diff.spectrum <- function (...) {
  par(mar=c(0,3,0,0))
  yy <- seq(-8, 8, length=30)/10
  xlim1 <- 16*c(-0.5, 0.5)/7/1.7/10
  plot (rep(0, length(yy)), yy, ty="n", xlim=xlim1, axes=FALSE, ...)   #pch=20, cex=3, col=relative.binning(xx), 
  polygon(hexagon.coords.auto(cbind(0,yy)),
          col=diff.binning(yy),
          border=0)
  abline(h=c(-2,2), col=2, lwd=2)
  axis(2, seq(-8, 8, by=2)/10)
}

z.score.binomial <- function (m1, m2) {

  p1 <- (m1[,2]+1)/(m1[,1]+2)
  p2 <- m2[,2]/m2[,1]

  (p1-p2)/sqrt(p1*(1-p1)/m1[,1])

}

z.score.poisson <- function (result, expected)
  (result-expected)/sqrt(result)

z.score.poisson.two <- function (count1, count2, time1, time2) {
  my.mean <- (count1/time1 - count2/time2)
  my.sd <- sqrt(count1/time1^2 + count2/time2^2)
  my.mean/my.sd
}


###################################################################################
# Color binning.

absolute.binning <- function (frac) {

  #green.
  my.colors <- c("#DDFFDD", "#88FF88", "#22FF22",
                 "#00DD00",
                 "#00AA00", "#007700", "#003300")
  my.colors[ordinal.maker (frac, c(0.03, 0.06, 0.09, 0.12, 0.16, 0.24)) + 1]
  
#  output <- rep(my.colors[1], length(frac))
#  for (kk in 1:(length(my.colors)-1)) output[frac > (kk-1)/30] <- my.colors[kk+1]
#  return(output)
  
}


relative.binning <- function (frac) {

  my.colors <- c("#000088","#4444FF","#8888FF","#CCCCFF",
                 "#CCCCCC",
                 "#FFCCCC","#FF8888","#FF4444","#BB0000")
#  my.colors <- c("#000088","#4444FF","#8888FF","#CCCCFF","#FFDDDD","#FF8888","#FF4444","#BB0000")
  my.colors[ordinal.maker (frac, c(1/2, 2/3, 5/6, 11/12, 13/12, 5/4, 3/2, 2)) + 1]

  #output <- rep(my.colors[1], length(frac))
  #for (kk in 1:(length(my.colors)-1)) output[frac > (kk-1)/3] <- my.colors[kk+1]
  #return(output)

}

z.score.binning <- function (zz) {
  my.colors <- c("#000088","#4444FF","#8888FF","#CCCCFF",
                 "#CCCCCC",
                 "#FFCCCC","#FF8888","#FF4444","#BB0000")
  my.colors[ordinal.maker (zz, c(-6, -4, -2, -0.75, 0.75, 2, 4, 6)) + 1]
}

diff.binning <- function (zz) {
  my.colors <- c("#000088","#4444FF","#8888FF","#CCCCFF",
                 "#CCCCCC",
                 "#FFCCCC","#FF8888","#FF4444","#BB0000")
  my.colors[ordinal.maker (zz, c(-6, -4, -2, -0.75, 0.75, 2, 4, 6)/6) + 1]
}

