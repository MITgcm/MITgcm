C $Header: /u/gcmpack/MITgcm/pkg/thsice/THSICE_SIZE.h,v 1.3 2007/04/04 02:40:42 jmc Exp $
C $Name:  $

#ifdef ALLOW_THSICE
C     *==========================================================*
C     | THSICE_SIZE.h Declare size of arrays for Therm_SeaIce pkg
C     *==========================================================*

C.. number layers of ice
C     nlyr   ::   maximum number of ice layers
      INTEGER nlyr
      PARAMETER (nlyr = 2)

C--   identifiers for advected properties
      INTEGER GAD_SI_FRAC, GAD_SI_HSNOW 
      INTEGER GAD_SI_HICE, GAD_SI_QICE1, GAD_SI_QICE2
      PARAMETER ( GAD_SI_FRAC  = -5,
     &            GAD_SI_HSNOW = -6,
     &            GAD_SI_HICE  = -7,
     &            GAD_SI_QICE1 = -8,
     &            GAD_SI_QICE2 = -9 )

#endif /* ALLOW_THSICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
