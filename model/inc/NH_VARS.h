C $Header: /u/gcmpack/MITgcm/model/inc/NH_VARS.h,v 1.4 2013/07/28 21:06:00 jmc Exp $
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
C     | to hold two levels of time tendency for w (for AB)
C     *==========================================================*
C     \ev
CEOP

#ifdef ALLOW_NONHYDROSTATIC
C--   COMMON /NH_VARS_R/ REAL NH state variables
C     phi_nh    :: Non-hydrostatic potential (=NH-Pressure/rhoConst)
C     dPhiNH    :: Surface-Hydrostatic correction to Non-hydrostatic Phi
C     gX, gxNm1 :: Time tendencies at current and previous time levels.
C     viscA?_W  :: Horizontal variable viscosities

#ifdef ALLOW_ADAMSBASHFORTH_3
      COMMON /NH_VARS_R/
     &                   phi_nh, dPhiNH,
     &                   gW, gwNm
#else /* ALLOW_ADAMSBASHFORTH_3 */
      COMMON /NH_VARS_R/
     &                   phi_nh, dPhiNH,
     &                   gW, gwNm1
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      _RL  phi_nh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  dPhiNH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
      _RL  gW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_ADAMSBASHFORTH_3
      _RL  gwNm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
#else /* ALLOW_ADAMSBASHFORTH_3 */
      _RL  gwNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_ADAMSBASHFORTH_3 */

#endif /* ALLOW_NONHYDROSTATIC */
