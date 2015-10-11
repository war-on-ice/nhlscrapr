/*===========================================================================*/
/* GifTools - GIF encoder / decoder                                          */
/* Copyright (C) 2005 Jarek Tuszynski                                        */
/* Distributed under GNU General Public License version 3                    */
/*===========================================================================*/
/*                                                                           */
/* This file contains interface between GifTools.cpp and caTools R Package   */
/*===========================================================================*/

#include "GifTools.h"
extern "C" {
      
  void imwritegif(char** filename, 
                int* Data, int *ColorMap, int *param, char** comment)
  {
    int i, nPixel = param[0]*param[1]*param[2];
    bool Interlace = (param[6]!=0);
  
    uchar* data = Calloc(nPixel, uchar);
    for(i=0; i<nPixel; i++) data[i] = Data[i]&0xff;
    param[7] = imwriteGif(*filename, data, param[0], param[1], param[2], 
      param[3], ColorMap, Interlace, param[4], param[5], *comment);
    Free(data);
  }
  
  SEXP imreadgif(SEXP filename, SEXP NImage, SEXP Verbose)
  { // The only R specific function
    int i, j, nPixel, nRow, nCol, nBand, ColorMap[256]; 
    int transparent, success, *ret, nImage, verbose;
    char *comment;
    const char *fname;
    uchar* data=0;
    SEXP Ret;
  
    // initialize data 
    nRow=nCol=nBand=transparent=0;
    comment = NULL;
    nImage  = asInteger(NImage);    
    verbose = asInteger(Verbose);    
    fname   = CHAR(STRING_ELT(filename, 0));
    success = imreadGif(fname, nImage, (bool) verbose, &data, nRow, nCol, 
              nBand, ColorMap, transparent, &comment); 
    nPixel  = nRow*nCol*nBand;
    PROTECT(Ret = allocVector(INTSXP, 9+256+nPixel));
    ret     = (int*) INTEGER(Ret);  /* get pointer to R's Ret */
    ret[0]  = nRow;       // pack output into array of integers
    ret[1]  = nCol;
    ret[2]  = nBand;
    ret[3]  = transparent;
    ret[4]  = success;
    j=9;
    for(i=0; i<256   ; i++) ret[j++] = ColorMap[i];
    for(i=0; i<nPixel; i++) ret[j++] = data[i];
    Free(data);
    if(comment && strlen(comment)) // if comment was found than pack it too
      setAttrib(Ret, install("comm"), mkString(comment));
    if(comment) Free(comment);
    UNPROTECT(1);
    return Ret;
  }

}
