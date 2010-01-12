C $Header: /u/gcmpack/MITgcm/pkg/longstep/LONGSTEP.h,v 1.4 2010/01/12 23:55:48 jahn Exp $
C $Name:  $

#ifdef ALLOW_LONGSTEP

CBOP
C     !ROUTINE: LONGSTEP.h
C     !INTERFACE:
C     include "LONGSTEP.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | LONGSTEP.h
C     | o Longstep state variables: averages of model variables
C     *==========================================================*
C     \ev
CEOP
C
C     LS_doTimeStep :: .TRUE. if ptracers are updated in this timestep
C
      LOGICAL LS_doTimeStep
      COMMON /LONGSTEP_STATE/ LS_doTimeStep

C     LS_uVel         :: longstep average of zonal velocity
C     LS_vVel         :: longstep average of meridional velocity
C     LS_wVel         :: longstep average of vertical velocity
C     LS_theta        :: longstep average of potential temperature
C     LS_salt         :: longstep average of salinity
C     LS_IVDConvCount :: longstep average of IVD convection counter
C     LS_fwFlux       :: longstep average of either PmEpR or EmPmR (note sign!)
C
      _RL LS_uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_IVDConvCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_fwFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER LS_uVelCount(nSx,nSy)
      INTEGER LS_vVelCount(nSx,nSy)
      INTEGER LS_wVelCount(nSx,nSy)
      INTEGER LS_thetaCount(nSx,nSy)
      INTEGER LS_saltCount(nSx,nSy)
      INTEGER LS_IVDConvCountCount(nSx,nSy)
      INTEGER LS_fwFluxCount(nSx,nSy)
      COMMON /LONGSTEP_DYNVARS_R/
     &       LS_uVel, LS_vVel, LS_wVel,
     &       LS_theta, LS_salt, LS_IVDConvCount,
     &       LS_fwFlux
      COMMON /LONGSTEP_DYNVARS_I/
     &       LS_uVelCount, LS_vVelCount, LS_wVelCount,
     &       LS_thetaCount, LS_saltCount, LS_IVDConvCountCount,
     &       LS_fwFluxCount

#ifdef ALLOW_GMREDI
C     Bottom row of tensor corresponds to W points
C     LS_Kwx :: longstep average of K_31 element, X direction at W point
C     LS_Kwy :: longstep average of K_32 element, Y direction at W point
C     LS_Kwz :: longstep average of K_33 element, Z direction at W point
C
      _RL LS_Kwx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_Kwy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_Kwz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER LS_KwxCount(nSx,nSy)
      INTEGER LS_KwyCount(nSx,nSy)
      INTEGER LS_KwzCount(nSx,nSy)
      COMMON /LONGSTEP_GM_R/ LS_Kwx, LS_Kwy, LS_Kwz
      COMMON /LONGSTEP_GM_I/ LS_KwxCount,LS_KwyCount,LS_KwzCount
#endif /* ALLOW_GMREDI */

#ifdef ALLOW_KPP
C     LS_KPPdiffKzS :: longstep average of Vert. diff. coeff. for tracers
C     LS_KPPghat    :: longstep average of Nonlocal transport coefficient
C
      _RL LS_KPPdiffKzS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_KPPghat    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER LS_KPPdiffKzSCount(nSx,nSy)
      INTEGER LS_KPPghatCount   (nSx,nSy)
      COMMON /LONGSTEP_KPP_R/ LS_KPPdiffKzS, LS_KPPghat
      COMMON /LONGSTEP_KPP_I/ LS_KPPdiffKzSCount, LS_KPPghatCount
#endif

#ifdef SHORTWAVE_HEATING
C     LS_Qsw :: longstep average of net upward shortwave radiation after ice
C
      _RL LS_Qsw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER LS_QswCount(nSx,nSy)
      COMMON /LONGSTEP_EXTRA_R/ LS_Qsw
      COMMON /LONGSTEP_EXTRA_I/ LS_QswCount
#endif

C     ice?
C     forcing?

#endif /* ALLOW_LONGSTEP */

