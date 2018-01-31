C     *==========================================================*
C     | LAND_SIZE.h Declare size of arrays for Land Model
C     *==========================================================*

#ifndef SIZE_IS_SET
C     MITgcm declaration of grid size. 
#include "SIZE.h"
#define SIZE_IS_SET

#endif

C- dimension for Land package   
C     land_nLev :: Nunber of soil layers for land model
      INTEGER land_nLev
      PARAMETER ( land_nLev=2 ) 

