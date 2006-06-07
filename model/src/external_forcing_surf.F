C $Header: /u/gcmpack/MITgcm/model/src/external_forcing_surf.F,v 1.35 2006/06/07 01:55:13 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
 
CBOP
C     !ROUTINE: EXTERNAL_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_SURF( 
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXTERNAL_FORCING_SURF                          
C     | o Determines forcing terms based on external fields       
C     |   relaxation terms etc.                                   
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#ifdef ALLOW_SEAICE
#include "SEAICE.h"
#endif /* ALLOW_SEAICE */
#ifdef ALLOW_SHELFICE
#include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */ 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi,bj  :: tile indices
C     iMin,iMax, jMin,jMax :: Range of points for calculation
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j
C     number of surface interface layer
      INTEGER ks
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpFac
#endif /* ALLOW_DIAGNOSTICS */
CEOP

      IF ( usingPCoords ) THEN
       ks        = Nr 
      ELSE
       ks        = 1
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( doThetaClimRelax .OR. doSaltClimRelax ) THEN
C--   Start with surface restoring term :

       DO j = jMin, jMax
        DO i = iMin, iMax
#ifdef ALLOW_SEAICE
C     Do not restore under sea-ice
C     Heat Flux (restoring term) : 
          surfaceForcingT(i,j,bi,bj) = 
     &      -lambdaThetaClimRelax(i,j,bi,bj) * (1-AREA(i,j,1,bi,bj))
     &         *(theta(i,j,ks,bi,bj)-SST(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
C     Salt Flux (restoring term) : 
          surfaceForcingS(i,j,bi,bj) =
     &      -lambdaSaltClimRelax(i,j,bi,bj) * (1-AREA(i,j,1,bi,bj))
     &         *(salt(i,j,ks,bi,bj)-SSS(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
#else /* ifndef ALLOW_SEAICE */
C     Heat Flux (restoring term) : 
          surfaceForcingT(i,j,bi,bj) = 
     &      -lambdaThetaClimRelax(i,j,bi,bj)
     &         *(theta(i,j,ks,bi,bj)-SST(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
C     Salt Flux (restoring term) : 
          surfaceForcingS(i,j,bi,bj) =
     &      -lambdaSaltClimRelax(i,j,bi,bj)
     &         *(salt(i,j,ks,bi,bj)-SSS(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
#endif /* ALLOW_SEAICE */
        ENDDO
       ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef NONLIN_FRSURF
C-    T,S surface forcing will be applied (thermodynamics) after the update
C     of surf.thickness (hFac): account for change in surf.thickness
       IF (staggerTimeStep.AND.nonlinFreeSurf.GT.0) THEN
        IF (select_rStar.GT.0) THEN
# ifndef DISABLE_RSTAR_CODE
         DO j=jMin,jMax
          DO i=iMin,iMax
            surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &                                  * rStarExpC(i,j,bi,bj)
            surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &                                  * rStarExpC(i,j,bi,bj)
          ENDDO
         ENDDO
# endif /* DISABLE_RSTAR_CODE */
        ELSE
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF (ks.EQ.ksurfC(i,j,bi,bj)) THEN
            surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &             *_recip_hFacC(i,j,ks,bi,bj)*hFac_surfC(i,j,bi,bj)
            surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &             *_recip_hFacC(i,j,ks,bi,bj)*hFac_surfC(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN

C     tRelax (temperature relaxation [W/m2], positive <-> increasing Theta)
        tmpFac = HeatCapacity_Cp*recip_horiVertRatio*rhoConst
        CALL DIAGNOSTICS_SCALE_FILL(
     &           surfaceForcingT(1-Olx,1-Oly,bi,bj),tmpFac,1,
     &                             'TRELAX  ',0, 1,2,bi,bj,myThid)

C     sRelax (salt relaxation [g/m2/s], positive <-> increasing Salt)
        tmpFac = recip_horiVertRatio*rhoConst
        CALL DIAGNOSTICS_SCALE_FILL(
     &           surfaceForcingS(1-Olx,1-Oly,bi,bj),tmpFac,1,
     &                             'SRELAX  ',0, 1,2,bi,bj,myThid)

       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      ELSE
C--   No restoring for T & S : set surfaceForcingT,S to zero :

       DO j = jMin, jMax
        DO i = iMin, iMax
          surfaceForcingT(i,j,bi,bj) = 0. _d 0
          surfaceForcingS(i,j,bi,bj) = 0. _d 0
        ENDDO
       ENDDO

C--   end restoring / no restoring block.
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Surface Fluxes :

      DO j = jMin, jMax
         DO i = iMin, iMax

C     Zonal wind stress fu:
          surfaceForcingU(i,j,bi,bj) = 
     &      fu(i,j,bi,bj)*horiVertRatio*recip_rhoConst
C     Meridional wind stress fv:
          surfaceForcingV(i,j,bi,bj) = 
     &      fv(i,j,bi,bj)*horiVertRatio*recip_rhoConst
C     Net heat flux Qnet:
          surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &       - ( Qnet(i,j,bi,bj)
#ifdef SHORTWAVE_HEATING
     &          -Qsw(i,j,bi,bj)
#endif
     &         ) *recip_Cp*horiVertRatio*recip_rhoConst
C     Net Salt Flux : 
          surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &      -saltFlux(i,j,bi,bj)*horiVertRatio*recip_rhoConst

         ENDDO
      ENDDO
        
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Fresh-water flux 

C-    Apply mask on Fresh-Water flux 
C      (needed for SSH forcing, whether or not exactConserv is used) 
      IF ( useRealFreshWaterFlux ) THEN
        DO j=1,sNy
         DO i=1,sNx
           EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)*maskH(i,j,bi,bj) 
         ENDDO
        ENDDO
      ENDIF

#ifdef EXACT_CONSERV
c NB: synchronous time step: PmEpR lag 1 time step behind EmPmR
c     to stay consitent with volume change (=d/dt etaH).
      IF ( staggerTimeStep ) THEN
        DO j=1,sNy
         DO i=1,sNx
           PmEpR(i,j,bi,bj) = -EmPmR(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &     .AND. useRealFreshWaterFlux ) THEN

c-  NonLin_FrSurf and RealFreshWaterFlux : PmEpR effectively changes 
c   the water column height ; temp., salt, (tracer) flux associated 
c   with this input/output of water is added here to the surface tendency.
c

       IF (temp_EvPrRn.NE.UNSET_RL) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &      + PmEpR(i,j,bi,bj)
     &         *( temp_EvPrRn - theta(i,j,ks,bi,bj) )
     &         *convertEmP2rUnit
         ENDDO
        ENDDO
       ENDIF

       IF (salt_EvPrRn.NE.UNSET_RL) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &      + PmEpR(i,j,bi,bj)
     &         *( salt_EvPrRn - salt(i,j,ks,bi,bj) )
     &         *convertEmP2rUnit
         ENDDO
        ENDDO
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
#else /* EXACT_CONSERV */
      IF (.TRUE.) THEN
#endif /* EXACT_CONSERV */

c- EmPmR does not really affect the water column height (for tracer budget)
c   and is converted to a salt tendency.

       IF (convertFW2Salt .EQ. -1.) THEN
c- converts EmPmR to salinity tendency using surface local salinity
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &      + EmPmR(i,j,bi,bj)*salt(i,j,ks,bi,bj)
     &         *convertEmP2rUnit
         ENDDO
        ENDDO
       ELSE 
c- converts EmPmR to virtual salt flux using uniform salinity (default=35)
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &      + EmPmR(i,j,bi,bj)*convertFW2Salt
     &         *convertEmP2rUnit
         ENDDO
        ENDDO
       ENDIF

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
         CALL PTRACERS_FORCING_SURF(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime,myIter,myThid )
      ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef ATMOSPHERIC_LOADING

C-- Atmospheric surface Pressure loading :

      IF ( usingZCoords ) THEN
       IF ( useRealFreshWaterFlux ) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          phi0surf(i,j,bi,bj) = ( pload(i,j,bi,bj)
     &                        +sIceLoad(i,j,bi,bj)*gravity
     &                          )*recip_rhoConst
         ENDDO
        ENDDO
       ELSE
        DO j = jMin, jMax
         DO i = iMin, iMax
          phi0surf(i,j,bi,bj) = pload(i,j,bi,bj)*recip_rhoConst
         ENDDO
        ENDDO
       ENDIF
      ELSEIF ( usingPCoords ) THEN
C-- This is a hack used to read phi0surf from a file (ploadFile)
C   instead of computing it from bathymetry & density ref. profile.
C   The true atmospheric P-loading is not yet implemented for P-coord
C   (requires time varying dP(Nr) like dP(k-bottom) with NonLin FS).
        DO j = jMin, jMax
         DO i = iMin, iMax
          phi0surf(i,j,bi,bj) = pload(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#endif /* ATMOSPHERIC_LOADING */

#ifdef ALLOW_SHELFICE
      IF ( usingZCoords ) THEN
       IF ( useSHELFICE) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          phi0surf(i,j,bi,bj) = phi0surf(i,j,bi,bj) 
     &         + shelficeLoadAnomaly(i,j,bi,bj)*recip_rhoConst
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_EBM
c--    Values for surfaceForcingT, surfaceForcingS
c      are overwritten by those produced by EBM
cph    AD recomputation problems if these IF useEBM are used
cph      IF ( useEBM ) THEN
       CALL EBM_FORCING_SURF(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime,myIter,myThid )
cph      ENDIF
#endif

      RETURN
      END
