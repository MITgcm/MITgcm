C $Header: /u/gcmpack/MITgcm/model/inc/Attic/GW.h,v 1.2 2001/02/02 21:04:47 adcroft Exp $

#ifdef ALLOW_NONHYDROSTATIC
C     /==========================================================\
C     | GW.h                                                     |
C     | o Additional state variables for non-hydrostatic mode    |
C     \==========================================================/
C
C     wVel  - zonal velocity (m/s, i=1 held at western face)
C     gX, gXNM1 - Time tendencies at current and prvious time levels.

      COMMON /GW_R/ 
     &                   gw,gwNm1
      _RL  gW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gWnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#endif /* ALLOW_NONHYDROSTATIC */
