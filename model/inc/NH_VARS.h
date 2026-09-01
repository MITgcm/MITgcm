!BOP
! !ROUTINE: NH_VARS.h
! !INTERFACE:
! include "NH_VARS.h"
! !DESCRIPTION:
! \bv
! *==========================================================*
! | NH_VARS.h
! | o Additional state variables for non-hydrostatic model
! |   and Quasi-Hydrostatic time-stepping
! *==========================================================*
! | In N-H mode, wVel becomes a prognostic variable: need
! | to hold two levels of time tendency for w (for AB)
! *==========================================================*
! \ev
!EOP

#ifdef ALLOW_NONHYDROSTATIC
!--   COMMON /NH_VARS_R/ REAL NH state variables
! phi_nh    :: Non-hydrostatic potential (=NH-Pressure/rhoConst)
! dPhiNH    :: Surface-Hydrostatic correction to Non-hydrostatic Phi
! gW, gwNm1 :: Time tendencies at current and previous time levels.
! viscA?_W  :: Horizontal variable viscosities

#ifdef ALLOW_ADAMSBASHFORTH_3
      COMMON /NH_VARS_R/                                                          &
     &      phi_nh, dPhiNH,                                                       &
     &      gW, gwNm
#else /* ALLOW_ADAMSBASHFORTH_3 */
      COMMON /NH_VARS_R/                                                          &
     &      phi_nh, dPhiNH,                                                       &
     &      gW, gwNm1
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

#ifdef ALLOW_QHYD_STAGGER_TS
!--   COMMON /NH_VARS_QH_AB/ Quasi-Hydrostatic Adams-Bashforth variables
! QHydGwNm  :: QuasiHydrostatic vertical acceleration to add to Buoyancy at
!              previous time-step for AB with staggerTimeStep (units: m/s^2)
      COMMON /NH_VARS_QH_AB/                                                      &
     &      QHydGwNm
# ifdef ALLOW_ADAMSBASHFORTH_3
      _RL  QHydGwNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
# else /* ALLOW_ADAMSBASHFORTH_3 */
      _RL  QHydGwNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
# endif /* ALLOW_ADAMSBASHFORTH_3 */
#endif /* ALLOW_QHYD_STAGGER_TS */
