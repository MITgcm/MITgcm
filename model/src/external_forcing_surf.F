C $Header: /u/gcmpack/MITgcm/model/src/external_forcing_surf.F,v 1.19 2004/03/03 06:55:59 dimitri Exp $
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
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
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
      INTEGER kSurface
CEOP

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
       kSurface        = Nr 
      else
       kSurface        = 1
      endif

C--   Surface Fluxes :

      DO j = jMin, jMax
         DO i = iMin, iMax

c     Zonal wind stress fu:
          surfaceTendencyU(i,j,bi,bj) = 
     &      fu(i,j,bi,bj)*horiVertRatio*recip_rhoConst
     &           *recip_drF(kSurface)*recip_hFacW(i,j,kSurface,bi,bj)
c     Meridional wind stress fv:
          surfaceTendencyV(i,j,bi,bj) = 
     &      fv(i,j,bi,bj)*horiVertRatio*recip_rhoConst
     &           *recip_drF(kSurface)*recip_hFacS(i,j,kSurface,bi,bj)
c     Net heat flux Qnet:
          surfaceTendencyT(i,j,bi,bj) = 
     &      -Qnet(i,j,bi,bj)*recip_Cp*horiVertRatio*recip_rhoConst
     &           *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj)
C     Net Salt Flux : 
          surfaceTendencyS(i,j,bi,bj) = 
     &      -saltFlux(i,j,bi,bj)*horiVertRatio*recip_rhoConst
     &           *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj)

#ifdef ALLOW_PASSIVE_TRACER
c ***  define the tracer surface tendency here ***
#endif /* ALLOW_PASSIVE_TRACER */

         ENDDO
      ENDDO
        
C--   Surface restoring term :

      IF ( doThetaClimRelax .OR. doSaltClimRelax ) THEN
       DO j = jMin, jMax
        DO i = iMin, iMax
#ifdef ALLOW_SEAICE
C     Don't restore under sea-ice
C     Heat Flux (restoring term) : 
          surfaceTendencyT(i,j,bi,bj) = surfaceTendencyT(i,j,bi,bj)
     &      -lambdaThetaClimRelax * (1-AREA(i,j,1,bi,bj))
     &         *(theta(i,j,kSurface,bi,bj)-SST(i,j,bi,bj))
C     Salt Flux (restoring term) : 
          surfaceTendencyS(i,j,bi,bj) = surfaceTendencyS(i,j,bi,bj)
     &      -lambdaSaltClimRelax * (1-AREA(i,j,1,bi,bj))
     &         *(salt(i,j,kSurface,bi,bj)-SSS(i,j,bi,bj))
#else /* ifndef ALLOW_SEAICE */
C     Heat Flux (restoring term) : 
         IF ( abs(yC(i,j,bi,bj)).LE.latBandClimRelax ) THEN
          surfaceTendencyT(i,j,bi,bj) = surfaceTendencyT(i,j,bi,bj)
     &      -lambdaThetaClimRelax
     &         *(theta(i,j,kSurface,bi,bj)-SST(i,j,bi,bj))
C     Salt Flux (restoring term) : 
          surfaceTendencyS(i,j,bi,bj) = surfaceTendencyS(i,j,bi,bj)
     &      -lambdaSaltClimRelax
     &         *(salt(i,j,kSurface,bi,bj)-SSS(i,j,bi,bj))
         ENDIF
#endif /* ALLOW_SEAICE */
        ENDDO
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Fresh-water flux 

#ifdef NONLIN_FRSURF
      IF ( (nonlinFreeSurf.GT.0 .OR. buoyancyRelation.EQ.'OCEANICP')
     &     .AND. useRealFreshWaterFlux ) THEN

c-  NonLin_FrSurf and RealFreshWaterFlux : PmEpR effectively changes 
c   the water column height ; temp., salt, (tracer) flux associated 
c   with this input/output of water is added here to the surface tendency.
c
c NB: PmEpR lag 1 time step behind EmPmR ( PmEpR_n = - EmPmR_n-1 ) to stay 
c     consitent with volume change (=d/dt etaN).

       IF (temp_EvPrRn.NE.UNSET_RL) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyT(i,j,bi,bj) = surfaceTendencyT(i,j,bi,bj)
     &      + PmEpR(i,j,bi,bj)
     &         *( temp_EvPrRn - theta(i,j,kSurface,bi,bj) )
     &           *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj) 
     &         *convertEmP2rUnit
         ENDDO
        ENDDO
       ENDIF

       IF (salt_EvPrRn.NE.UNSET_RL) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyS(i,j,bi,bj) = surfaceTendencyS(i,j,bi,bj)
     &      + PmEpR(i,j,bi,bj)
     &         *( salt_EvPrRn - salt(i,j,kSurface,bi,bj) )
     &           *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj) 
     &         *convertEmP2rUnit
         ENDDO
        ENDDO
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
#else /* NONLIN_FRSURF */
      IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */

c- EmPmR does not really affect the water column height (for tracer budget)
c   and is converted to a salt tendency.

       IF (convertFW2Salt .EQ. -1.) THEN
c- converts EmPmR to salinity tendency using surface local salinity
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyS(i,j,bi,bj) = surfaceTendencyS(i,j,bi,bj)
     &      + EmPmR(i,j,bi,bj)*salt(i,j,kSurface,bi,bj)
     &           *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj)
     &         *convertEmP2rUnit
         ENDDO
        ENDDO
       ELSE 
c- converts EmPmR to virtual salt flux using uniform salinity (default=35)
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyS(i,j,bi,bj) = surfaceTendencyS(i,j,bi,bj)
     &      + EmPmR(i,j,bi,bj)*convertFW2Salt
     &           *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj) 
     &         *convertEmP2rUnit
         ENDDO
        ENDDO
       ENDIF

      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
         CALL PTRACERS_FORCING_SURF(
     I                              bi, bj, iMin, iMax, jMin, jMax,
     I                              myTime,myIter,myThid )
      ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef ATMOSPHERIC_LOADING

C-- Atmospheric surface Pressure loading :

      IF (buoyancyRelation .eq. 'OCEANIC' ) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          phi0surf(i,j,bi,bj) = pload(i,j,bi,bj)*recip_rhoConst
         ENDDO
        ENDDO
      ELSEIF ( buoyancyRelation .eq. 'OCEANICP' ) THEN 
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

#ifdef ALLOW_TIMEAVE
      IF ( taveFreq .NE. 0. _d 0 ) THEN
        CALL TIMEAVE_SURF_FLUX( bi, bj, myTime, myIter, myThid)
      ENDIF
#endif /* ALLOW_TIMEAVE */

      RETURN
      END
