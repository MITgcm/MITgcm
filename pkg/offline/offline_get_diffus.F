#include "OFFLINE_OPTIONS.h"
#ifdef ALLOW_DIC
# include "DIC_OPTIONS.h"
#endif
#ifdef ALLOW_DARWIN
# include "DARWIN_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: OFFLINE_GET_SURFFORCING
C     !INTERFACE:
      SUBROUTINE OFFLINE_GET_DIFFUS( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OFFLINE_GET_DIFFUS
C     | o Interpolate in time diffusivity fields that have
C     |   been loaded from file
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "OFFLINE.h"
#include "OFFLINE_SWITCH.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif
#ifdef ALLOW_KPP
# include "KPP.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime  :: current time in simulation
C     myIter  :: current iteration number in simulation
C     myThid  :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j,k
      INTEGER bi,bj
      _RL aWght, bWght
#ifdef ALLOW_AUTODIFF
      _RL locTime
      INTEGER intimeP, intime0, intime1
#endif /* ALLOW_AUTODIFF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF
C--   Re-compute the weights (bWght, aWght) to simplify dependencies
C     (since they are not stored on tapes)
      locTime = myTime - offlineTimeOffset
      CALL GET_PERIODIC_INTERVAL(
     O                  intimeP, intime0, intime1, bWght, aWght,
     I                  offlineForcingCycle, offlineForcingPeriod,
     I                  deltaToffline, locTime, myThid )
#endif /* ALLOW_AUTODIFF */

C--   Interpolate Diffusivity Components:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifndef ALLOW_AUTODIFF
        bWght = offline_Wght(1,bi,bj)
        aWght = offline_Wght(2,bi,bj)
#endif /* ndef ALLOW_AUTODIFF */

        IF ( Wvelfile .NE. ' '  .AND. myIter.EQ.nIter0 ) THEN
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            wVel(i,j,k,bi,bj) = bWght*wvel0(i,j,k,bi,bj)
     &                        + aWght*wvel1(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        IF ( offlineLoadConvec ) THEN
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IVDConvCount(i,j,k,bi,bj) = bWght*conv0(i,j,k,bi,bj)
     &                                + aWght*conv1(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
#ifdef ALLOW_GMREDI
        IF ( offlineLoadGMRedi ) THEN
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            Kwx(i,j,k,bi,bj)  = bWght*gmkx0(i,j,k,bi,bj)
     &                        + aWght*gmkx1(i,j,k,bi,bj)
            Kwy(i,j,k,bi,bj)  = bWght*gmky0(i,j,k,bi,bj)
     &                        + aWght*gmky1(i,j,k,bi,bj)
            Kwz(i,j,k,bi,bj)  = bWght*gmkz0(i,j,k,bi,bj)
     &                        + aWght*gmkz1(i,j,k,bi,bj)
#ifndef GM_NON_UNITY_DIAGONAL
            Kux(i,j,k,bi,bj) = GM_isopycK
            Kvy(i,j,k,bi,bj) = GM_isopycK
#endif
           ENDDO
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_GMREDI */
#ifdef ALLOW_KPP
        IF ( offlineLoadKPP ) THEN
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            KPPdiffKzS(i,j,k,bi,bj) = bWght*kdfs0(i,j,k,bi,bj)
     &                              + aWght*kdfs1(i,j,k,bi,bj)
C-- Note: for convenience, the array KPPghat will contain
C         the product ghat*diffKzS (and not ghat alone).
            KPPghat(i,j,k,bi,bj) = bWght*kght0(i,j,k,bi,bj)
     &                           + aWght*kght1(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_KPP */

C--   Interpolate surface forcing
#if ( (defined ALLOW_DIC) || (defined ALLOW_DARWIN) )
#ifdef ALLOW_OLD_VIRTUALFLUX
        IF ( SFluxFile.NE.' ' ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            surfaceForcingS(i,j,bi,bj) = bWght*sflx0(i,j,bi,bj)
     &                                 + aWght*sflx1(i,j,bi,bj)
            surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &                                  *mass2rUnit
           ENDDO
          ENDDO
        ENDIF
#endif /* ALLOW_OLD_VIRTUALFLUX */
#endif /* ALLOW_DIC or ALLOW_DARWIN */

C--    kept from older version:
c           surfaceForcingT(i,j,bi,bj) = bWght*hflx0(i,j,bi,bj)
c    &                                 + aWght*hflx1(i,j,bi,bj)
c           surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
c    &                         *recip_Cp*mass2rUnit
c           ICEM(i,j,bi,bj) = bWght*icem0(i,j,bi,bj)
c    &                      + aWght*icem1(i,j,bi,bj)

C--   end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
