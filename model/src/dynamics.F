#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

#undef DYNAMICS_GUGV_EXCH_CHECK

CBOP
C     !ROUTINE: DYNAMICS
C     !INTERFACE:
      SUBROUTINE DYNAMICS(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DYNAMICS
C     | o Controlling routine for the explicit part of the model
C     |   dynamics.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_VISC.h"
#endif
#ifdef ALLOW_CD_CODE
# include "CD_CODE_VARS.h"
#endif
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
# include "EOS.h"
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS_FIELDS.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS_PARAMS.h"
#  include "OBCS_FIELDS.h"
#  ifdef ALLOW_PTRACERS
#   include "OBCS_PTRACERS.h"
#  endif
# endif
# ifdef ALLOW_MOM_FLUXFORM
#  include "MOM_FLUXFORM.h"
# endif
#endif /* ALLOW_AUTODIFF */

C     !CALLING SEQUENCE:
C     DYNAMICS()
C      |
C      |-- CALC_EP_FORCING
C      |
C      |-- CALC_GRAD_PHI_SURF
C      |
C      |-- CALC_VISCOSITY
C      |
C      |-- MOM_CALC_3D_STRAIN
C      |
C      |-- CALC_EDDY_STRESS
C      |
C      |-- CALC_PHI_HYD
C      |
C      |-- MOM_FLUXFORM
C      |
C      |-- MOM_VECINV
C      |
C      |-- MOM_CALC_SMAG_3D
C      |-- MOM_UV_SMAG_3D
C      |
C      |-- TIMESTEP
C      |
C      |-- MOM_U_IMPLICIT_R
C      |-- MOM_V_IMPLICIT_R
C      |
C      |-- IMPLDIFF
C      |
C      |-- OBCS_APPLY_UV
C      |
C      |-- CALC_GW
C      |
C      |-- DIAGNOSTICS_FILL
C      |-- DEBUG_STATS_RL

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !FUNCTIONS:
#ifdef ALLOW_DIAGNOSTICS
c     LOGICAL  DIAGNOSTICS_IS_ON
c     EXTERNAL DIAGNOSTICS_IS_ON
#endif

C     !LOCAL VARIABLES:
C     == Local variables
C     fVer[UV]               o fVer: Vertical flux term - note fVer
C                                    is "pipelined" in the vertical
C                                    so we need an fVer for each
C                                    variable.
C     phiHydC    :: hydrostatic potential anomaly at cell center
C                   In z coords phiHyd is the hydrostatic potential
C                      (=pressure/rho0) anomaly
C                   In p coords phiHyd is the geopotential height anomaly.
C     phiHydF    :: hydrostatic potential anomaly at middle between 2 centers
C     dPhiHydX,Y :: Gradient (X & Y directions) of hydrostatic potential anom.
C     phiSurfX,  ::  gradient of Surface potential (Pressure/rho, ocean)
C     phiSurfY             or geopotential (atmos) in X and Y direction
C     guDissip   :: dissipation tendency (all explicit terms), u component
C     gvDissip   :: dissipation tendency (all explicit terms), v component
C     kappaRU    :: vertical viscosity for velocity U-component
C     kappaRV    :: vertical viscosity for velocity V-component
C     iMin, iMax :: Ranges and sub-block indices on which calculations
C     jMin, jMax    are applied.
C     bi, bj     :: tile indices
C     k          :: current level index
C     km1, kp1   :: index of level above (k-1) and below (k+1)
C     kUp, kDown :: Index for interface above and below. kUp and kDown are
C                   are switched with k to be the appropriate index into fVerU,V
      _RL fVerU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL phiHydF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiHydC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL guDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gvDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kappaRU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL kappaRV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
#ifdef ALLOW_SMAG_3D
C     str11       :: strain component Vxx @ grid-cell center
C     str22       :: strain component Vyy @ grid-cell center
C     str33       :: strain component Vzz @ grid-cell center
C     str12       :: strain component Vxy @ grid-cell corner
C     str13       :: strain component Vxz @ above uVel
C     str23       :: strain component Vyz @ above vVel
C     viscAh3d_00 :: Smagorinsky viscosity @ grid-cell center
C     viscAh3d_12 :: Smagorinsky viscosity @ grid-cell corner
C     viscAh3d_13 :: Smagorinsky viscosity @ above uVel
C     viscAh3d_23 :: Smagorinsky viscosity @ above vVel
C     addDissU    :: zonal momentum tendency from 3-D Smag. viscosity
C     addDissV    :: merid momentum tendency from 3-D Smag. viscosity
      _RL str11(1-OLx:sNx+OLx,1-OLy:sNy+OLy, Nr )
      _RL str22(1-OLx:sNx+OLx,1-OLy:sNy+OLy, Nr )
      _RL str33(1-OLx:sNx+OLx,1-OLy:sNy+OLy, Nr )
      _RL str12(1-OLx:sNx+OLx,1-OLy:sNy+OLy, Nr )
      _RL str13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL str23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL viscAh3d_00(1-OLx:sNx+OLx,1-OLy:sNy+OLy, Nr )
      _RL viscAh3d_12(1-OLx:sNx+OLx,1-OLy:sNy+OLy, Nr )
      _RL viscAh3d_13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL viscAh3d_23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL addDissU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL addDissV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#elif ( defined ALLOW_NONHYDROSTATIC )
      _RL str13(1), str23(1), str33(1)
      _RL viscAh3d_00(1), viscAh3d_13(1), viscAh3d_23(1)
#endif

      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, km1, kp1, kUp, kDown
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      PARAMETER( iMin = 0 , iMax = sNx+1 )
      PARAMETER( jMin = 0 , jMax = sNy+1 )
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (tile dependent)
C     kkey :: tape key (level and tile dependent)
      INTEGER tkey, kkey
#endif

#ifdef ALLOW_DIAGNOSTICS
c     LOGICAL dPhiHydDiagIsOn
      _RL tmpFac
#endif /* ALLOW_DIAGNOSTICS */

C---    The algorithm...
C
C       "Correction Step"
C       =================
C       Here we update the horizontal velocities with the surface
C       pressure such that the resulting flow is either consistent
C       with the free-surface evolution or the rigid-lid:
C         U[n] = U* + dt x d/dx P
C         V[n] = V* + dt x d/dy P
C
C       "Calculation of Gs"
C       ===================
C       This is where all the accelerations and tendencies (ie.
C       physics, parameterizations etc...) are calculated
C         rho = rho ( theta[n], salt[n] )
C         b   = b(rho, theta)
C         K31 = K31 ( rho )
C         Gu[n] = Gu( u[n], v[n], wVel, b, ... )
C         Gv[n] = Gv( u[n], v[n], wVel, b, ... )
C         Gt[n] = Gt( theta[n], u[n], v[n], wVel, K31, ... )
C         Gs[n] = Gs( salt[n], u[n], v[n], wVel, K31, ... )
C
C       "Time-stepping" or "Prediction"
C       ================================
C       The models variables are stepped forward with the appropriate
C       time-stepping scheme (currently we use Adams-Bashforth II)
C       - For momentum, the result is always *only* a "prediction"
C       in that the flow may be divergent and will be "corrected"
C       later with a surface pressure gradient.
C       - Normally for tracers the result is the new field at time
C       level [n+1} *BUT* in the case of implicit diffusion the result
C       is also *only* a prediction.
C       - We denote "predictors" with an asterisk (*).
C         U* = U[n] + dt x ( 3/2 Gu[n] - 1/2 Gu[n-1] )
C         V* = V[n] + dt x ( 3/2 Gv[n] - 1/2 Gv[n-1] )
C         theta[n+1] = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         salt[n+1] = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C       With implicit diffusion:
C         theta* = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         salt* = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         (1 + dt * K * d_zz) theta[n] = theta*
C         (1 + dt * K * d_zz) salt[n] = salt*
C---
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER( 'DYNAMICS', myThid )
#endif

#ifdef ALLOW_DIAGNOSTICS
c     dPhiHydDiagIsOn = .FALSE.
c     IF ( useDiagnostics )
c    &  dPhiHydDiagIsOn = DIAGNOSTICS_IS_ON( 'Um_dPHdx', myThid )
c    &               .OR. DIAGNOSTICS_IS_ON( 'Vm_dPHdy', myThid )
#endif

C-- Call to routine for calculation of Eliassen-Palm-flux-forced
C    U-tendency, if desired:
#ifdef INCLUDE_EP_FORCING_CODE
      CALL CALC_EP_FORCING(myThid)
#endif

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
      CALL DUMMY_IN_DYNAMICS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAF
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAF
CHPF$  INDEPENDENT, NEW (fVerU,fVerV
CHPF$&                  ,phiHydF
CHPF$&                  ,kappaRU,kappaRV
CHPF$&                  )
#endif /* ALLOW_AUTODIFF_TAMC */

       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Set up work arrays with valid (i.e. not NaN) values
C     These initial values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.

#ifdef ALLOW_AUTODIFF
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
c--   need some re-initialisation here to break dependencies
           gU(i,j,k,bi,bj) = 0. _d 0
           gV(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fVerU  (i,j,1) = 0. _d 0
          fVerU  (i,j,2) = 0. _d 0
          fVerV  (i,j,1) = 0. _d 0
          fVerV  (i,j,2) = 0. _d 0
          phiHydF (i,j)  = 0. _d 0
          phiHydC (i,j)  = 0. _d 0
#ifndef INCLUDE_PHIHYD_CALCULATION_CODE
          dPhiHydX(i,j)  = 0. _d 0
          dPhiHydY(i,j)  = 0. _d 0
#endif
          phiSurfX(i,j)  = 0. _d 0
          phiSurfY(i,j)  = 0. _d 0
          guDissip(i,j)  = 0. _d 0
          gvDissip(i,j)  = 0. _d 0
#ifdef ALLOW_AUTODIFF
          phiHydLow(i,j,bi,bj) = 0. _d 0
# if (defined NONLIN_FRSURF) && (defined ALLOW_MOM_FLUXFORM)
#  ifndef DISABLE_RSTAR_CODE
          dWtransC(i,j,bi,bj) = 0. _d 0
          dWtransU(i,j,bi,bj) = 0. _d 0
          dWtransV(i,j,bi,bj) = 0. _d 0
#  endif
# endif
#endif /* ALLOW_AUTODIFF */
         ENDDO
        ENDDO
        IF ( useDiagnostics ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           botDragU(i,j,bi,bj)  = 0. _d 0
           botDragV(i,j,bi,bj)  = 0. _d 0
          ENDDO
         ENDDO
        ENDIF

C--     Start computation of dynamics

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wVel (:,:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Explicit part of the Surface Potential Gradient (add in TIMESTEP)
C       (note: this loop will be replaced by CALL CALC_GRAD_ETA)
        IF (implicSurfPress.NE.1.) THEN
          CALL CALC_GRAD_PHI_SURF(
     I         bi,bj,iMin,iMax,jMin,jMax,
     I         etaN,
     O         phiSurfX,phiSurfY,
     I         myThid )
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uVel     (:,:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE vVel     (:,:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
#ifdef ALLOW_KPP
CADJ STORE KPPviscAz(:,:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
#endif /* ALLOW_KPP */
#endif /* ALLOW_AUTODIFF_TAMC */

#ifndef ALLOW_AUTODIFF
        IF ( .NOT.momViscosity ) THEN
#endif
          DO k=1,Nr+1
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             kappaRU(i,j,k) = 0. _d 0
             kappaRV(i,j,k) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
#ifndef ALLOW_AUTODIFF
        ENDIF
#endif
#ifdef INCLUDE_CALC_DIFFUSIVITY_CALL
C--     Calculate the total vertical viscosity
        IF ( momViscosity ) THEN
          CALL CALC_VISCOSITY(
     I            bi,bj, iMin,iMax,jMin,jMax,
     O            kappaRU, kappaRV,
     I            myThid )
        ENDIF
#endif /* INCLUDE_CALC_DIFFUSIVITY_CALL */

#ifdef ALLOW_SMAG_3D
        IF ( useSmag3D ) THEN
          CALL MOM_CALC_3D_STRAIN(
     O         str11, str22, str33, str12, str13, str23,
     I         bi, bj, myThid )
        ENDIF
#endif /* ALLOW_SMAG_3D */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRU(:,:,:) = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE kappaRV(:,:,:) = comlev1_bibj, key=tkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_OBCS
C--   For Stevens boundary conditions velocities need to be extrapolated
C     (copied) to a narrow strip outside the domain
        IF ( useOBCS ) THEN
          CALL OBCS_COPY_UV_N(
     U         uVel(1-OLx,1-OLy,1,bi,bj),
     U         vVel(1-OLx,1-OLy,1,bi,bj),
     I         Nr, bi, bj, myThid )
        ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_EDDYPSI
        CALL CALC_EDDY_STRESS(bi,bj,myThid)
#endif

C--     Start of dynamics loop
        DO k=1,Nr

C--       km1    Points to level above k (=k-1)
C--       kup    Cycles through 1,2 to point to layer above
C--       kDown  Cycles through 2,1 to point to current layer

          km1  = MAX(1,k-1)
          kp1  = MIN(k+1,Nr)
          kup  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE totPhiHyd (:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE phiHydLow (:,:,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE theta (:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt  (:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
# ifdef NONLIN_FRSURF
cph-test
CADJ STORE  phiHydC (:,:)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE  phiHydF (:,:)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gU(:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gV(:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
#  ifndef ALLOW_ADAMSBASHFORTH_3
CADJ STORE guNm1(:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gvNm1(:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
#  else
CADJ STORE guNm(:,:,k,bi,bj,1)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE guNm(:,:,k,bi,bj,2)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gvNm(:,:,k,bi,bj,1)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gvNm(:,:,k,bi,bj,2)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif
#  ifdef ALLOW_CD_CODE
CADJ STORE uNM1(:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE vNM1(:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE uVelD(:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE vVelD(:,:,k,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_AUTODIFF_TAMC */

C--      Integrate hydrostatic balance for phiHyd with BC of phiHyd(z=0)=0
         CALL CALC_PHI_HYD(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     U        phiHydF,
     O        phiHydC, dPhiHydX, dPhiHydY,
     I        myTime, myIter, myThid )

C--      Calculate accelerations in the momentum equations (gU, gV, ...)
C        and step forward storing the result in gU, gV, etc...
         IF ( momStepping ) THEN
#ifdef ALLOW_AUTODIFF
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              guDissip(i,j)  = 0. _d 0
              gvDissip(i,j)  = 0. _d 0
            ENDDO
           ENDDO
#endif /* ALLOW_AUTODIFF */
#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) && (defined ALLOW_MOM_FLUXFORM)
#  ifndef DISABLE_RSTAR_CODE
CADJ STORE dWtransC(:,:,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dWtransU(:,:,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dWtransV(:,:,bi,bj)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif
# endif /* NONLIN_FRSURF and ALLOW_MOM_FLUXFORM */
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE fVerU(:,:,:) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE fVerV(:,:,:) = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */
           IF (.NOT. vectorInvariantMomentum) THEN
#ifdef ALLOW_MOM_FLUXFORM
              CALL MOM_FLUXFORM(
     I         bi,bj,k,iMin,iMax,jMin,jMax,
     I         kappaRU, kappaRV,
     U         fVerU(1-OLx,1-OLy,kUp),   fVerV(1-OLx,1-OLy,kUp),
     O         fVerU(1-OLx,1-OLy,kDown), fVerV(1-OLx,1-OLy,kDown),
     O         guDissip, gvDissip,
     I         myTime, myIter, myThid)
#endif
           ELSE
#ifdef ALLOW_MOM_VECINV
             CALL MOM_VECINV(
     I         bi,bj,k,iMin,iMax,jMin,jMax,
     I         kappaRU, kappaRV,
     I         fVerU(1-OLx,1-OLy,kUp),   fVerV(1-OLx,1-OLy,kUp),
     O         fVerU(1-OLx,1-OLy,kDown), fVerV(1-OLx,1-OLy,kDown),
     O         guDissip, gvDissip,
     I         myTime, myIter, myThid)
#endif
           ENDIF

#ifdef ALLOW_SMAG_3D
           IF ( useSmag3D ) THEN
             CALL MOM_CALC_SMAG_3D(
     I         str11, str22, str33, str12, str13, str23,
     O         viscAh3d_00, viscAh3d_12, viscAh3d_13, viscAh3d_23,
     I         smag3D_hLsC, smag3D_hLsW, smag3D_hLsS, smag3D_hLsZ,
     I         k, bi, bj, myThid )
             CALL MOM_UV_SMAG_3D(
     I         str11, str22, str12, str13, str23,
     I         viscAh3d_00, viscAh3d_12, viscAh3d_13, viscAh3d_23,
     O         addDissU, addDissV,
     I         iMin,iMax,jMin,jMax, k, bi, bj, myThid )
             DO j= jMin,jMax
              DO i= iMin,iMax
               guDissip(i,j) = guDissip(i,j) + addDissU(i,j)
               gvDissip(i,j) = gvDissip(i,j) + addDissV(i,j)
              ENDDO
             ENDDO
           ENDIF
#endif /* ALLOW_SMAG_3D */

           CALL TIMESTEP(
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     I         dPhiHydX,dPhiHydY, phiSurfX, phiSurfY,
     I         guDissip, gvDissip,
     I         myTime, myIter, myThid)

         ENDIF

C--     end of dynamics k loop (1:Nr)
        ENDDO

C--     Implicit Vertical advection & viscosity
#if (defined (INCLUDE_IMPLVERTADV_CODE) && \
     defined (ALLOW_MOM_COMMON) && !(defined ALLOW_AUTODIFF))
        IF ( momImplVertAdv .OR. implicitViscosity
     &                      .OR. selectImplicitDrag.GE.1 ) THEN
C      to recover older (prior to 2016-10-05) results:
c       IF ( momImplVertAdv ) THEN
          CALL MOM_U_IMPLICIT_R( kappaRU,
     I                           bi, bj, myTime, myIter, myThid )
          CALL MOM_V_IMPLICIT_R( kappaRV,
     I                           bi, bj, myTime, myIter, myThid )
        ELSEIF ( implicitViscosity ) THEN
#else /* INCLUDE_IMPLVERTADV_CODE */
        IF     ( implicitViscosity ) THEN
#endif /* INCLUDE_IMPLVERTADV_CODE */
#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE gU(:,:,:,bi,bj) = comlev1_bibj , key=tkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         -1, kappaRU, recip_hFacW(1-OLx,1-OLy,1,bi,bj),
     U         gU(1-OLx,1-OLy,1,bi,bj),
     I         myThid )
#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE gV(:,:,:,bi,bj) = comlev1_bibj , key=tkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         -2, kappaRV, recip_hFacS(1-OLx,1-OLy,1,bi,bj),
     U         gV(1-OLx,1-OLy,1,bi,bj),
     I         myThid )
        ENDIF

#ifdef ALLOW_OBCS
C--      Apply open boundary conditions
        IF ( useOBCS ) THEN
C--      but first save intermediate velocities to be used in the
C        next time step for the Stevens boundary conditions
          CALL OBCS_SAVE_UV_N(
     I        bi, bj, iMin, iMax, jMin, jMax, 0,
     I        gU, gV, myThid )
          CALL OBCS_APPLY_UV( bi, bj, 0, gU, gV, myThid )
        ENDIF
#endif /* ALLOW_OBCS */

#ifdef    ALLOW_CD_CODE
        IF (implicitViscosity.AND.useCDscheme) THEN
#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE vVelD(:,:,:,bi,bj) = comlev1_bibj , key=tkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         0, kappaRU, recip_hFacW(1-OLx,1-OLy,1,bi,bj),
     U         vVelD(1-OLx,1-OLy,1,bi,bj),
     I         myThid )
#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE uVelD(:,:,:,bi,bj) = comlev1_bibj , key=tkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         0, kappaRV, recip_hFacS(1-OLx,1-OLy,1,bi,bj),
     U         uVelD(1-OLx,1-OLy,1,bi,bj),
     I         myThid )
        ENDIF
#endif    /* ALLOW_CD_CODE */
C--     End implicit Vertical advection & viscosity

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_NONHYDROSTATIC
C--   Step forward W field in N-H algorithm
        IF ( nonHydrostatic ) THEN
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('CALC_GW', myThid )
#endif
         CALL TIMER_START('CALC_GW          [DYNAMICS]',myThid)
         CALL CALC_GW(
     I                 bi,bj, kappaRU, kappaRV,
     I                 str13, str23, str33,
     I                 viscAh3d_00, viscAh3d_13, viscAh3d_23,
     I                 myTime, myIter, myThid )
        ENDIF
        IF ( nonHydrostatic.OR.implicitIntGravWave )
     &   CALL TIMESTEP_WVEL( bi,bj, myTime, myIter, myThid )
        IF ( nonHydrostatic )
     &   CALL TIMER_STOP ('CALC_GW          [DYNAMICS]',myThid)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    end of bi,bj loops
       ENDDO
      ENDDO

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
        CALL OBCS_EXCHANGES( myThid )
      ENDIF
#endif

Cml(
C     In order to compare the variance of phiHydLow of a p/z-coordinate
C     run with etaH of a z/p-coordinate run the drift of phiHydLow
C     has to be removed by something like the following subroutine:
C      CALL REMOVE_MEAN_RL( 1, phiHydLow, maskInC, maskInC, rA, drF,
C     &                     'phiHydLow', myTime, myIter, myThid )
Cml)

#ifdef INCLUDE_SOUNDSPEED_CALC_CODE
      CALL DIAGS_SOUND_SPEED( myThid )
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

       CALL DIAGNOSTICS_FILL(totPhihyd,'PHIHYD  ',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(phiHydLow,'PHIBOT  ',0, 1,0,1,1,myThid)
       tmpFac = 1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL(totPhihyd,tmpFac,2,
     &                                 'PHIHYDSQ',0,Nr,0,1,1,myThid)

       CALL DIAGNOSTICS_SCALE_FILL(phiHydLow,tmpFac,2,
     &                                 'PHIBOTSQ',0, 1,0,1,1,myThid)

       IF ( selectImplicitDrag.EQ.0 .AND.
     &      (  no_slip_bottom
     &    .OR. selectBotDragQuadr.GE.0
     &    .OR. bottomDragLinear.NE.0. ) ) THEN
        CALL DIAGNOSTICS_FILL_RS( botDragU, 'botTauX ',
     &                            0, 1, 0, 1, 1, myThid )
        CALL DIAGNOSTICS_FILL_RS( botDragV, 'botTauY ',
     &                            0, 1, 0, 1, 1, myThid )
       ENDIF

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
       CALL DEBUG_STATS_RL(1,EtaN,'EtaN (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,uVel,'Uvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,vVel,'Vvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,wVel,'Wvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,theta,'Theta (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,salt,'Salt (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gU,'Gu (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gV,'Gv (DYNAMICS)',myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
       CALL DEBUG_STATS_RL(Nr,guNm1,'GuNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gvNm1,'GvNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gtNm1,'GtNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gsNm1,'GsNm1 (DYNAMICS)',myThid)
#endif
      ENDIF
#endif

#ifdef DYNAMICS_GUGV_EXCH_CHECK
C- jmc: For safety checking only: This Exchange here should not change
C       the solution. If solution changes, it means something is wrong,
C       but it does not mean that it is less wrong with this exchange.
      IF ( debugLevel .GE. debLevE ) THEN
       CALL EXCH_UV_XYZ_RL(gU,gV,.TRUE.,myThid)
      ENDIF
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE( 'DYNAMICS', myThid )
#endif

      RETURN
      END
