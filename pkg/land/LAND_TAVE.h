C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_TAVE.h,v 1.4 2009/12/29 23:05:52 jmc Exp $
C $Name:  $


CBOP
C     !ROUTINE: LAND_TAVE.h
C     !INTERFACE:
C     include "LAND_TAVE.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | LAND_TAVE.h
C     | o Header for LAND time-average diagnostic
C     *==========================================================*
C     | Declares global arrays used for holding/accumulating
C     | diagnostic output from LAND.
C     *==========================================================*
C     \ev
CEOP

#ifdef ALLOW_LAND
#ifdef ALLOW_LAND_TAVE

C--   COMMON /LAND_TAVE_VARS/ Time average land-variables
C     land_timeAve  :: cumulated time [s]
C     land_grTtave  :: time average ground Temp. [oC]
C     land_entave   :: time average enthalpy of each layer [J/m3]
C     land_grWtave  :: time average soil moisture [0-1]
C     land_sTtave   :: time average surface Temperature [oC]
C     land_hStave   :: time average thickness of snow [m]
C     land_sAtave   :: time average snow age (s)
C     land_ROftave  :: time average Run-Off per surf unit [kg/m2/s]
C     land_eROtave  :: time average energy flux related to run-Off [W/m2]
      COMMON /LAND_TAVE_VARS/
     &            land_timeAve,
     &            land_grTtave, land_entave , land_grWtave,
     &            land_sTtave , land_hStave , land_sAtave ,
     &            land_ROftave, land_eROtave

      _RL land_timeAve(nSx,nSy)
      _RL land_grTtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_entave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_grWtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_sTtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_hStave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_sAtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_ROftave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_eROtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_LAND_TAVE */
#endif /* ALLOW_LAND */
