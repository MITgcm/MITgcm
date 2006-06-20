C $Header: /u/gcmpack/MITgcm/model/inc/NH_VARS.h,v 1.2 2006/06/20 20:57:37 baylor Exp $
C $Name:  $

CBOP
C     !ROUTINE: NH_VARS.h
C     !INTERFACE:
C     include "NH_VARS.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | NH_VARS.h
C     | o Additional state variables for non-hydrostatic model
C     *==========================================================*
C     | In N-H mode, wVel becomes a prognostic variable: need
C     | to hold two levels of time tendency for w (for AB-2)
C     *==========================================================*
C     \ev
CEOP

#ifdef ALLOW_NONHYDROSTATIC
C--   COMMON /NH_VARS_R/ REAL NH state variables
C     phi_nh    :: Non-hydrostatic potential (=NH-Pressure/rhoConst)
C     gX, gxNm1 :: Time tendencies at current and previous time levels.
C     viscA?_W  :: Horizontal variable viscosities

      COMMON /NH_VARS_R/
     &                   phi_nh,
     &                   gW, gwNm1,
     &                   viscAh_W, viscA4_W
      _RL  phi_nh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gwNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  viscAh_W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  viscA4_W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#endif /* ALLOW_NONHYDROSTATIC */
