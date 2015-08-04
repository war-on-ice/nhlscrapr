#===========================================================================#
# caTools - R library                                                       #
# Copyright (C) 2005 Jarek Tuszynski                                        #
# Distributed under GNU General Public License version 3                    #
#===========================================================================#


read.gif = function(filename, frame=0, flip=FALSE, verbose=FALSE)
{
  if (!is.character(filename)) stop("write.gif: 'filename' has to be a string")
  if (length(filename)>1) filename = paste(filename, collapse = "")  # combine characters into a string
  isURL = length(grep("^http://", filename)) | 
          length(grep("^ftp://",  filename)) | 
          length(grep("^file://", filename))
  if(isURL) {
    tf <- tempfile()
    download.file(filename, tf, mode='wb', quiet=TRUE)
    filename = tf
  }

  x = .Call("imreadgif", filename, as.integer(frame), as.integer(verbose), 
       PACKAGE="nhlscrapr") 
  comt = as.character(attr(x, 'comm'))
  if (isURL) file.remove(filename)

  nRow    = x[1]
  nCol    = x[2]
  nBand   = x[3]
  tran    = x[4]
  success = x[5]
  nPixel  = nRow*nCol*nBand
  stats = -success
  if (stats>=6)  {
    warning("write.gif: file '", filename, 
      "' contains multiple color-maps. Use 'frame' > 0.") 
    stats = stats-6
  }
  if (nPixel==0) {
    switch (stats,
    stop("write.gif: cannot open the input file: ", filename, call.=FALSE),
    stop("write.gif: input file '", filename, "' is not a GIF file", call.=FALSE),
    stop("write.gif: unexpected end of file: ", filename, call.=FALSE),
    stop("write.gif: syntax error in file: ", filename, call.=FALSE) )
  } else {
    switch (stats, , , 
    warning("write.gif: unexpected end of file: ", filename, call.=FALSE),
    warning("write.gif: syntax error in file: ", filename, call.=FALSE),
    warning("write.gif: file '", filename,
      "' contains multiple images (frames) of uneven length. Use 'frame' > 0." , call.=FALSE))
  }   
  Palette = x[ 10:265 ]
  x       = x[-(1:265)] # delete non image data
  if (nBand>1) { # 3D data cubes
    dim(x)  = c(nCol, nRow, nBand)
    if (flip) x = x[,ncol(x):1,]
    else x = aperm(x, c(2,1,3))
  } else {       # this is a matrix
    dim(x) = c(nCol, nRow)
    if (flip) x = x[,ncol(x):1]
    else x = t(x)
  }
  Palette = Palette[Palette>=0]
  red     = bitAnd(bitShiftR(Palette,16), 255)
  green   = bitAnd(bitShiftR(Palette, 8), 255)
  blue    = bitAnd(          Palette    , 255)
  Palette = rgb (red, green, blue, 255, maxColorValue = 255)
  if (tran==-1) tran = NULL
  return (list(image=x, col=Palette, transparent=tran, comment=comt))
}

# source("c:/programs/R/rw2011/src/library/caTools/R/GIF.R")
