/*===========================================================================*/
/* GifTools - GIF encoder / decoder                                          */
/* Copyright (C) 2005 Jarek Tuszynski                                        */
/* Distributed under GNU General Public License version 3                    */
/*===========================================================================*/

#ifndef GIF_TOOLS_H
#define GIF_TOOLS_H
#include <R.h>
#include <Rinternals.h> 

extern "C" {
  #define print Rprintf
  #define Error error
  typedef unsigned char uchar;

  int imreadGif(const char* filename, int nImage, bool verbose,
              uchar** data, int &nRow, int &nCol, int &nBand,
              int ColorMap[255], int &Transparent, char** Comment);
              
  int imwriteGif(const char* filename, const uchar* data, int nRow, int nCol,
                  int nBand, int nColor, const int *ColorMap,  bool interlace, 
                 int transparent, int DalayTime, char* comment);
}
#endif
              
