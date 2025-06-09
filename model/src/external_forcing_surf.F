#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME_OPTIONS.h"
#endif
#undef CHECK_OVERLAP_FORCING

CBOP
C     !ROUTINE: EXTERNAL_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_SURF(
     I             iMin, iMax, jMin, jMax,
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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     iMin,iMax, jMin,jMax :: Range of points for calculation
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread no. that called this routine.
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     bi,bj  :: tile indices
C     i,j    :: loop indices
C     ks     :: index of surface interface layer
      INTEGER bi,bj
      INTEGER i,j
      INTEGER ks
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER tkey
#endif
      _RL recip_Cp
#ifdef ALLOW_BALANCE_FLUXES
      _RS tmpVar(1)
#endif
#ifdef CHECK_OVERLAP_FORCING
      _RS fixVal
#endif
CEOP

      IF ( usingPCoords ) THEN
       ks        = Nr
      ELSE
       ks        = 1
      ENDIF
      recip_Cp = 1. _d 0 / HeatCapacity_Cp

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Apply adjustment (balancing forcing) and exchanges
C     to oceanic surface forcing

#ifdef ALLOW_BALANCE_FLUXES
C     balance fluxes
# ifdef ALLOW_AUTODIFF
      tmpVar(1) = oneRS
#  ifdef ALLOW_AUTODIFF_TAMC
CADJ INCOMPLETE tmpVar
#  endif
# endif
      IF ( selectBalanceEmPmR.GE.1 .AND.
     &     (.NOT.useSeaice .OR. useThSIce) ) THEN
       IF ( selectBalanceEmPmR.EQ.1 ) THEN
        tmpVar(1) = oneRS
        CALL REMOVE_MEAN_RS( 1, EmPmR, maskInC, maskInC, rA,
     &                       tmpVar, 'EmPmR', myTime, myIter, myThid )
       ELSEIF ( selectBalanceEmPmR.EQ.2 ) THEN
        tmpVar(1) = -oneRS
        CALL REMOVE_MEAN_RS( 1, EmPmR, weight2BalanceFlx, maskInC, rA,
     &                       tmpVar, 'EmPmR', myTime, myIter, myThid )
       ENDIF
      ENDIF
      IF ( balanceQnet  .AND. (.NOT.useSeaice .OR. useThSIce) ) THEN
        tmpVar(1) = oneRS
        CALL REMOVE_MEAN_RS( 1, Qnet,  maskInC, maskInC, rA,
     &                       tmpVar, 'Qnet ', myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_BALANCE_FLUXES */

C-    Apply exchanges (if needed)

#ifdef CHECK_OVERLAP_FORCING
C     Put large value in overlap of forcing array to check if exch is needed
c     IF ( .NOT. useKPP ) THEN
       fixVal = 1.
       CALL RESET_HALO_RS ( EmPmR, fixVal, 1, myThid )
       fixVal = 400.
       CALL RESET_HALO_RS ( Qnet, fixVal, 1, myThid )
       fixVal = -200.
       CALL RESET_HALO_RS ( Qsw, fixVal, 1, myThid )
       fixVal = 40.
       CALL RESET_HALO_RS ( saltFlux, fixVal, 1, myThid )
c     ENDIF
#endif /* CHECK_OVERLAP_FORCING */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef EXACT_CONSERV
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE PmEpR = comlev1, key = ikey_dynamics,  kind = isbyte
CADJ STORE EmPmR = comlev1, key = ikey_dynamics,  kind = isbyte
# endif
#endif /* EXACT_CONSERV */
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C--   set surfaceForcingT,S to zero.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           surfaceForcingT(i,j,bi,bj) = 0. _d 0
           surfaceForcingS(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
c#ifdef ALLOW_EBM
        IF ( useRealFreshWaterFlux ) THEN
C--   in case some pkgs put non-zero values over land, clean this up:
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)*maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
c#endif /* ALLOW_EBM */
#ifdef EXACT_CONSERV
C NB: synchronous time step: PmEpR lags 1 time step behind EmPmR
C     to stay consistent with volume change (=d/dt etaH).
        IF ( staggerTimeStep ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           PmEpR(i,j,bi,bj) = -EmPmR(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* EXACT_CONSERV */
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Start with surface restoring term :
      IF ( doThetaClimRelax .OR. doSaltClimRelax ) THEN
       CALL FORCING_SURF_RELAX(
     I              iMin, iMax, jMin, jMax,
     I              myTime, myIter, myThid )
      ENDIF

#ifdef ALLOW_PTRACERS
C--   passive tracer surface forcing:
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE surfaceForcingS = comlev1, key = ikey_dynamics,
CADJ &    kind = isbyte
#ifdef ALLOW_BLING
CADJ STORE EmPmR = comlev1, key = ikey_dynamics,  kind = isbyte
#endif
#endif
      IF ( usePTRACERS ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         CALL PTRACERS_FORCING_SURF(
     I        surfaceForcingS(1-OLx,1-OLy,bi,bj),
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime, myIter, myThid )
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_PTRACERS */

C- Notes: setting of PmEpR and pTracers surface forcing could have been
C         moved below, inside a unique bi,bj block. However this results
C         in tricky dependencies for TAF (and recomputations).
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE EmPmR(:,:,bi,bj) = comlev1_bibj, key=tkey,  kind = isbyte
#ifdef EXACT_CONSERV
CADJ STORE PmEpR(:,:,bi,bj) = comlev1_bibj, key=tkey,  kind = isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Surface Fluxes :
        DO j = jMin, jMax
         DO i = iMin, iMax

C     Zonal wind stress fu:
          surfaceForcingU(i,j,bi,bj) = fu(i,j,bi,bj)*mass2rUnit
C     Meridional wind stress fv:
          surfaceForcingV(i,j,bi,bj) = fv(i,j,bi,bj)*mass2rUnit
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
C-- for the case of SALT_PLUME_VOLUME, need to call this S/R right
C-- before kpp in do_oceanic_phys.F due to recent moved of
C-- external_forcing_surf.F outside bi,bj loop.
#ifndef SALT_PLUME_VOLUME
        IF ( useSALT_PLUME ) THEN
         CALL SALT_PLUME_FORCING_SURF(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime, myIter, myThid )
        ENDIF
#endif /* SALT_PLUME_VOLUME */
#endif /* ALLOW_SALT_PLUME */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Fresh-water flux

C-    Apply mask on Fresh-Water flux (if useRealFreshWaterFlux)
C     <== removed: maskInC is applied directly in S/R SOLVE_FOR_PRESSURE

#ifdef EXACT_CONSERV
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
     &                          +sIceLoad(i,j,bi,bj)*gravity*sIceLoadFac
     &                            )*recip_rhoConst
           ENDDO
          ENDDO
         ELSE
          DO j = jMin, jMax
           DO i = iMin, iMax
            phi0surf(i,j,bi,bj) = pLoad(i,j,bi,bj)*recip_rhoConst
           ENDDO
          ENDDO
         ENDIF
c       ELSEIF ( usingPCoords ) THEN
C-- This is a hack used to read phi0surf from a file (pLoadFile)
C   instead of computing it from bathymetry & density ref. profile.
C   ==> now done only once, in S/R INI_FORCING
c         DO j = jMin, jMax
c          DO i = iMin, iMax
c           phi0surf(i,j,bi,bj) = pLoad(i,j,bi,bj)
c          ENDDO
c         ENDDO
        ENDIF
#endif /* ATMOSPHERIC_LOADING */

#ifdef ALLOW_SHELFICE
        IF ( useSHELFICE) THEN
          CALL SHELFICE_FORCING_SURF(
     I                  bi, bj, iMin, iMax, jMin, jMax,
     I                  myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_SHELFICE */

C--   end bi,bj loops.
       ENDDO
      ENDDO

      RETURN
      END
