C $Header: /u/gcmpack/MITgcm/model/inc/Attic/GW.h,v 1.5 2003/10/09 04:19:18 edhill Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_NONHYDROSTATIC
CBOP
C    !ROUTINE: GW.h
C    !INTERFACE:
C    include GW.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | GW.h                                                      
C     | o Additional state variables for non-hydrostatic mode     
C     *==========================================================*
C     \ev
CEOP
C
C     wVel  - zonal velocity (m/s, i=1 held at western face)
C     gX, gXNM1 - Time tendencies at current and prvious time levels.

      COMMON /GW_R/ 
     &                   gw,gwNm1
      _RL  gW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gWnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#endif /* ALLOW_NONHYDROSTATIC */
