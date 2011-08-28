C $Header: /u/gcmpack/MITgcm/model/src/external_forcing_surf.F,v 1.53 2011/08/28 21:57:35 jmc Exp $
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
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#endif /* ALLOW_SEAICE */
#ifdef ALLOW_SHELFICE
#include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi,bj  :: tile indices
C     iMin,iMax, jMin,jMax :: Range of points for calculation
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread no. that called this routine.
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j    :: loop indices
C     ks     :: index of surface interface layer
      INTEGER i,j
      INTEGER ks
CEOP
#ifdef ALLOW_PTRACERS
C     relaxForcingS :: Salt forcing due to surface relaxation
      _RL relaxForcingS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_PTRACERS */
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpFac
#endif /* ALLOW_DIAGNOSTICS */

      IF ( usingPCoords ) THEN
       ks        = Nr
      ELSE
       ks        = 1
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        surfaceForcingS(i,j,bi,bj) = 0. _d 0
        surfaceForcingT(i,j,bi,bj) = 0. _d 0
       ENDDO
      ENDDO
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( doThetaClimRelax .OR. doSaltClimRelax ) THEN
C--   Start with surface restoring term :

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE surfaceForcingS(:,:,bi,bj)  = comlev1_bibj,
CADJ &     key=ikey, kind = isbyte
# endif

#ifdef ALLOW_SEAICE
       IF ( useSEAICE .AND. (.NOT. SEAICErestoreUnderIce) ) THEN
C     Do not restore under sea-ice
        DO j = jMin, jMax
         DO i = iMin, iMax
C     Heat Flux (restoring term) :
          surfaceForcingT(i,j,bi,bj) =
     &      -lambdaThetaClimRelax(i,j,bi,bj)*(1.-AREA(i,j,bi,bj))
     &         *(theta(i,j,ks,bi,bj)-SST(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
C     Salt Flux (restoring term) :
          surfaceForcingS(i,j,bi,bj) =
     &      -lambdaSaltClimRelax(i,j,bi,bj) *(1.-AREA(i,j,bi,bj))
     &         *(salt(i,j,ks,bi,bj)-SSS(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
         ENDDO
        ENDDO
       ELSE
#endif /* ALLOW_SEAICE */
        DO j = jMin, jMax
         DO i = iMin, iMax
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
         ENDDO
        ENDDO
#ifdef ALLOW_SEAICE
       ENDIF
#endif /* ALLOW_SEAICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef NONLIN_FRSURF
C-    T,S surface forcing will be applied (thermodynamics) after the update
C     of surf.thickness (hFac): account for change in surf.thickness
       IF (staggerTimeStep.AND.nonlinFreeSurf.GT.0) THEN
        IF ( select_rStar.GT.0 ) THEN
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
        ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
         DO j=jMin,jMax
          DO i=iMin,iMax
            surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &        *( 1. _d 0 + dEtaHdt(i,j,bi,bj)*deltaTfreesurf
     &                    *dBHybSigF(ks)*recip_drF(ks)
     &                    *recip_hFacC(i,j,ks,bi,bj)
     &         )
            surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &        *( 1. _d 0 + dEtaHdt(i,j,bi,bj)*deltaTfreesurf
     &                    *dBHybSigF(ks)*recip_drF(ks)
     &                    *recip_hFacC(i,j,ks,bi,bj)
     &         )
          ENDDO
         ENDDO
# endif /* DISABLE_SIGMA_CODE */
        ELSE
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF (ks.EQ.kSurfC(i,j,bi,bj)) THEN
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
        tmpFac = HeatCapacity_Cp*rUnit2mass
        CALL DIAGNOSTICS_SCALE_FILL(
     &           surfaceForcingT(1-OLx,1-OLy,bi,bj),tmpFac,1,
     &                             'TRELAX  ',0, 1,2,bi,bj,myThid)

C     sRelax (salt relaxation [g/m2/s], positive <-> increasing Salt)
        tmpFac = rUnit2mass
        CALL DIAGNOSTICS_SCALE_FILL(
     &           surfaceForcingS(1-OLx,1-OLy,bi,bj),tmpFac,1,
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

#ifdef ALLOW_PTRACERS
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE surfaceForcingS(:,:,bi,bj)  = comlev1_bibj,
CADJ &     key=ikey, kind = isbyte
# endif
      IF ( usePTRACERS ) THEN
C--   save salt forcing due to surface relaxation
       DO j = jMin, jMax
        DO i = iMin, iMax
         relaxForcingS(i,j) = surfaceForcingS(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_PTRACERS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Surface Fluxes :

      DO j = jMin, jMax
         DO i = iMin, iMax

C     Zonal wind stress fu:
          surfaceForcingU(i,j,bi,bj) =
     &      fu(i,j,bi,bj)*mass2rUnit
C     Meridional wind stress fv:
          surfaceForcingV(i,j,bi,bj) =
     &      fv(i,j,bi,bj)*mass2rUnit
C     Net heat flux Qnet:
          surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &       - ( Qnet(i,j,bi,bj)
#ifdef SHORTWAVE_HEATING
     &          -Qsw(i,j,bi,bj)
#endif
     &         ) *recip_Cp*mass2rUnit
C     Net Salt Flux :
          surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &      -saltFlux(i,j,bi,bj)*mass2rUnit

         ENDDO
      ENDDO

#ifdef ALLOW_SALT_PLUME
C saltPlume is the amount of salt rejected by ice while freezing;
C it is here subtracted from surfaceForcingS and will be redistributed
C to multiple vertical levels later on as per Duffy et al. (GRL 1999)
      IF ( useSALT_PLUME ) THEN
         CALL SALT_PLUME_FORCING_SURF(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime,myIter,myThid )
      ENDIF
#endif /* ALLOW_SALT_PLUME */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Fresh-water flux

C-    Apply mask on Fresh-Water flux
C      (needed for SSH forcing, whether or not exactConserv is used)
      IF ( useRealFreshWaterFlux ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE surfaceForcingS(:,:,bi,bj)  = comlev1_bibj,
CADJ &     key=ikey, kind = isbyte
# endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)*maskInC(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#ifdef EXACT_CONSERV
C NB: synchronous time step: PmEpR lag 1 time step behind EmPmR
C     to stay consitent with volume change (=d/dt etaH).
      IF ( staggerTimeStep ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           PmEpR(i,j,bi,bj) = -EmPmR(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &     .AND. useRealFreshWaterFlux ) THEN

C--   NonLin_FrSurf and RealFreshWaterFlux : PmEpR effectively changes
C     the water column height ; temp., salt, (tracer) flux associated
C     with this input/output of water is added here to the surface tendency.

       IF (temp_EvPrRn.NE.UNSET_RL) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &      + PmEpR(i,j,bi,bj)
     &          *( temp_EvPrRn - theta(i,j,ks,bi,bj) )
     &          *mass2rUnit
         ENDDO
        ENDDO
       ENDIF

       IF (salt_EvPrRn.NE.UNSET_RL) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &      + PmEpR(i,j,bi,bj)
     &          *( salt_EvPrRn - salt(i,j,ks,bi,bj) )
     &          *mass2rUnit
         ENDDO
        ENDDO
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
#else /* EXACT_CONSERV */
      IF (.TRUE.) THEN
#endif /* EXACT_CONSERV */

C--   EmPmR does not really affect the water column height (for tracer budget)
C     and is converted to a salt tendency.

       IF (convertFW2Salt .EQ. -1.) THEN
C-    use local surface tracer field to calculate forcing term:

        IF (temp_EvPrRn.NE.UNSET_RL) THEN
C     account for Rain/Evap heat content (temp_EvPrRn) using local SST
         DO j = jMin, jMax
          DO i = iMin, iMax
           surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &       + EmPmR(i,j,bi,bj)
     &           *( theta(i,j,ks,bi,bj) - temp_EvPrRn )
     &           *mass2rUnit
          ENDDO
         ENDDO
        ENDIF
        IF (salt_EvPrRn.NE.UNSET_RL) THEN
C     converts EmPmR to salinity tendency using surface local salinity
         DO j = jMin, jMax
          DO i = iMin, iMax
           surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &       + EmPmR(i,j,bi,bj)
     &           *( salt(i,j,ks,bi,bj) - salt_EvPrRn )
     &           *mass2rUnit
          ENDDO
         ENDDO
        ENDIF

       ELSE
C-    use uniform tracer value to calculate forcing term:

        IF (temp_EvPrRn.NE.UNSET_RL) THEN
C     account for Rain/Evap heat content (temp_EvPrRn) assuming uniform SST (=tRef)
         DO j = jMin, jMax
          DO i = iMin, iMax
           surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &       + EmPmR(i,j,bi,bj)
     &           *( tRef(ks) - temp_EvPrRn )
     &           *mass2rUnit
          ENDDO
         ENDDO
        ENDIF
        IF (salt_EvPrRn.NE.UNSET_RL) THEN
C     converts EmPmR to virtual salt flux using uniform salinity (default=35)
         DO j = jMin, jMax
          DO i = iMin, iMax
           surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &       + EmPmR(i,j,bi,bj)
     &           *( convertFW2Salt - salt_EvPrRn )
     &           *mass2rUnit
          ENDDO
         ENDDO
        ENDIF

C-    end local-surface-tracer / uniform-value distinction
       ENDIF

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
         CALL PTRACERS_FORCING_SURF(
     I        relaxForcingS,
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime,myIter,myThid )
      ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef ATMOSPHERIC_LOADING
C-- Atmospheric surface Pressure loading : added to phi0surf when using Z-coord;
C   Not yet implemented for Ocean in P: would need to be applied to the other end
C   of the column, as a vertical velocity (omega); (meaningless for Atmos in P).
C- Note:
C   Using P-coord., a hack (now directly applied from S/R INI_FORCING)
C   is sometime used to read phi0surf from a file (pLoadFile) instead
C   of computing it from bathymetry & density ref. profile.

      IF ( usingZCoords ) THEN
C   The true atmospheric P-loading is not yet implemented for P-coord
C   (requires time varying dP(Nr) like dP(k-bottom) with NonLin FS).
       IF ( useRealFreshWaterFlux ) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          phi0surf(i,j,bi,bj) = ( pLoad(i,j,bi,bj)
     &                        +sIceLoad(i,j,bi,bj)*gravity
     &                          )*recip_rhoConst
         ENDDO
        ENDDO
       ELSE
        DO j = jMin, jMax
         DO i = iMin, iMax
          phi0surf(i,j,bi,bj) = pLoad(i,j,bi,bj)*recip_rhoConst
         ENDDO
        ENDDO
       ENDIF
c     ELSEIF ( usingPCoords ) THEN
C-- This is a hack used to read phi0surf from a file (pLoadFile)
C   instead of computing it from bathymetry & density ref. profile.
C   ==> now done only once, in S/R INI_FORCING
c       DO j = jMin, jMax
c        DO i = iMin, iMax
c         phi0surf(i,j,bi,bj) = pLoad(i,j,bi,bj)
c        ENDDO
c       ENDDO
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

      RETURN
      END
