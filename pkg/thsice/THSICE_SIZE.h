C $Header: /u/gcmpack/MITgcm/pkg/thsice/THSICE_SIZE.h,v 1.2 2005/06/24 04:36:54 edhill Exp $
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

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
