C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_TAVE.h,v 1.1 2003/06/12 17:54:22 jmc Exp $
C $Name:  $

#ifdef ALLOW_LAND

C     *==========================================================*
C     | LAND_TAVE.h
C     | o Header for LAND time-average diagnostic
C     *==========================================================*
C     | Declares global arrays used for holding/accumulating
C     | diagnostic output from LAND.
C     *==========================================================*

#ifdef ALLOW_LAND_TAVE

C--   COMMON /LAND_TAVE/ Timer for Land Time-Average Diagnostics
      COMMON /LAND_TAVE/ land_timeAve
      _RL land_timeAve(Nr,nSx,nSy)

C--   COMMON /LAND_TAVE_A/ Time average land-variables
C     land_grTtave  :: time average ground Temp.
C     land_grWtave  :: time average soil moisture 
C     land_ROftave  :: time average Run-Off
      COMMON /LAND_TAVE_A/
     &            land_grTtave, land_grWtave, land_ROftave

      _RL land_grTtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_grWtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_ROftave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_LAND_TAVE */


#endif /* ALLOW_LAND */
