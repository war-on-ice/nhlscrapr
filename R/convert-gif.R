
# Extract "on ice" data from GIF file.

pick.out.features <- function(times,  # in seconds
                              imagefile) {  #with color codes.
  #imagefile = im1$image; times <- c(0, 365, 1200, 1345)
  #imagefile = game.rec$imh; times <- seq(59, 3599, by=60)
  #times=pl.table$game.record$seconds; imagefile=away.gif$image

  ## 0. replace codes.
    imagefile[imagefile == 119] <- 0
    imagefile[imagefile == 249] <- 13
    imagefile[imagefile == 250] <- 14
    imagefile[imagefile == 255] <- 19
    
  #1. get period end times.
  reference <- min(which(imagefile[,201]==0))
  if (is.na(reference)) stop ("Reference point along (,201) not registered.")
  if (reference>0) imagefile <- imagefile[-(1:reference),]
  
  if (imagefile[17,201] != 0) stop ("Reference point at (17,201) not registered.")
  pdends <- which(imagefile[17,] %in% c(13,249))
  pixels.per.period <- pdends[1]-202

  times.pixel <- floor(times / 1200 * pixels.per.period) + 201
  
  relevant.subset <- imagefile[seq(24,dim(imagefile)[1],by=18),1:max(pdends)]
  #paint it green for left-continuity.
  relevant.subset[relevant.subset[,-1] == 14 & relevant.subset[,-dim(relevant.subset)[2]] == 0] <- 14

  #two black bars amidst white: switch left one on.
  picks <- which(relevant.subset[,-c(1:3)] == 19 &
                 relevant.subset[,-c(1:2, dim(relevant.subset)[2])] == 0 &
                 relevant.subset[,-c(1, dim(relevant.subset)[2] + (-1:0))] == 0 &
                 relevant.subset[,-c(dim(relevant.subset)[2] + (-2:0))] == 19 &
                 imagefile[22, 1+1:(max(pdends)-3)] != 0)
  relevant.subset[picks+20] <- 14

  #binary.
  relevant.subset[relevant.subset != 14] <- 0; relevant.subset <- 1*(relevant.subset > 0)

  output <- sapply(times.pixel, function(tt) {
    c1 <- which(relevant.subset[,tt] > 0)
    if (length(c1)<6) c1 <- c(c1, rep(0, 6-length(c1)))
    c1[1:6]
  })
  if (class(output) != "matrix") stop ("Image doing weird things. This should not happen ever.")

  return(output)
}
