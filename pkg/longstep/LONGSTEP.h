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
C     Horizontal part of the tensor
C     LS_Kux :: longstep average of K_11 element of GM/Redi tensor,
C               X direction at U point
C     LS_Kvy :: longstep average of K_22 element of GM/Redi tensor,
C               Y direction at V point
C     First/second rows of tensor corresponds to U/V points
C     LS_Kuz :: longstep average of K_13 element of GM/Redi tensor,
C               Z direction at U point
C     LS_Kvz :: longstep average of K_23 element of GM/Redi tensor,
C               Z direction at V point
C
      _RL LS_Kwx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_Kwy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_Kwz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_Kux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_Kvy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_Kuz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_Kvz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /LONGSTEP_GM_R/
     &       LS_Kwx, LS_Kwy, LS_Kwz,
     &       LS_Kux, LS_Kvy,
     &       LS_Kuz, LS_Kvz

C     streamfunction computed in GMREDI
C     LS_PsiX :: longstep average of K_31 element, X direction at W point
C     LS_PsiY :: longstep average of K_32 element, Y direction at W point
      _RL LS_PsiX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL LS_PsiY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /LONGSTEP_GM_PSI/ LS_PsiX, LS_PsiY

C     And all corresponding counters for GM/Redi Longstep-averaging
      INTEGER LS_KwxCount(nSx,nSy)
      INTEGER LS_KwyCount(nSx,nSy)
      INTEGER LS_KwzCount(nSx,nSy)
      INTEGER LS_KuxCount(nSx,nSy)
      INTEGER LS_KvyCount(nSx,nSy)
      INTEGER LS_KuzCount(nSx,nSy)
      INTEGER LS_KvzCount(nSx,nSy)
      INTEGER LS_PsiXCount(nSx,nSy)
      INTEGER LS_PsiYCount(nSx,nSy)
      COMMON /LONGSTEP_GM_I/
     &       LS_KwxCount, LS_KwyCount, LS_KwzCount,
     &       LS_KuxCount, LS_KvyCount,
     &       LS_KuzCount, LS_KvzCount,
     &       LS_PsiXCount, LS_PsiYCount
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

