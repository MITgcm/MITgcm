C $Header: /u/gcmpack/MITgcm/pkg/thsice/THSICE_SIZE.h,v 1.1 2003/11/23 01:20:13 jmc Exp $
C $Name:  $

#ifdef ALLOW_THSICE
C     *==========================================================*
C     | THSICE_SIZE.h Declare size of arrays for Therm_SeaIce pkg
C     *==========================================================*

C.. number layers of ice 
C     nlyr   ::   maximum number of ice layers
      INTEGER nlyr
      PARAMETER (nlyr = 2)

#endif /* ALLOW_THSICE */

