#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_PREFORMED_RESET

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_PREFORMED_RESET(
     I                    bi, bj, iTracer, myTime, myIter, myThid )

C !DESCRIPTION:
C     Relax a preformed passive tracer towards either a fixed value or
C     another passive tracer (its "mate"), in the surface layer only
C     or throughout the mixed layer (PTRACERS_preformedUseMLD).
C     The relaxation is applied as a separate step after the tracer
C     has been updated, with weight dTLev/tauRelaxPreformed (capped at
C     1, i.e. an instantaneous reset, when tau <= dTLev).

C !USES: ===============================================================
#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_KPP
# include "KPP.h"
#endif
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj           :: tile indices
C  iTracer          :: tracer number
C  myTime           :: model time
C  myIter           :: time-step number
C  myThid           :: thread number
      INTEGER bi, bj, iTracer
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none (pTracer is updated)

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  kSrf    :: surface level index
C  kIfSrf  :: index of the interface at the surface
C  kDeep   :: index of the deeper interface of level k
C             (RF(k+1) for z-coordinates, RF(k) for p-coordinates)
C  wt      :: relaxation weight, dt/tau (<=1)
C  wgt     :: relaxation weight at each grid-point of level k
C  dMix    :: depth (in r-units) below the surface of the deeper
C             interface of level k
C  locMLD  :: mixed-layer depth: KPPhbl if useKPP, otherwise hMixLayer
      INTEGER i,j,k
      INTEGER kSrf, kIfSrf, kDeep, itrMate
      LOGICAL doReset
      _RL wt, dMix
      _RL wgt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL locMLD(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('PTRACERS_PREFORMED_RESET',myThid)
#endif

      IF ( usingPCoords ) THEN
       kSrf   = Nr
       kIfSrf = Nr+1
      ELSE
       kSrf   = 1
       kIfSrf = 1
      ENDIF
      itrMate = PTRACERS_preformedMate(iTracer)

      IF ( PTRACERS_preformedUseMLD(iTracer) ) THEN
#ifdef ALLOW_KPP
       IF ( useKPP ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          locMLD(i,j) = KPPhbl(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
#else
       IF ( .TRUE. ) THEN
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          locMLD(i,j) = hMixLayer(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDIF
      ENDIF

      DO k=1,Nr

       IF ( PTRACERS_tauRelaxPreformed(iTracer).GT.PTRACERS_dTLev(k) )
     &  THEN
        wt = PTRACERS_dTLev(k)/PTRACERS_tauRelaxPreformed(iTracer)
       ELSE
        wt = 1. _d 0
       ENDIF

       doReset = k.EQ.kSrf .OR. PTRACERS_preformedUseMLD(iTracer)
       IF ( doReset ) THEN

C-    Relaxation weight: surface level, or levels fully within the MLD
        IF ( k.EQ.kSrf ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           wgt(i,j) = wt*maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ELSE
         IF ( usingPCoords ) THEN
          kDeep = k
         ELSE
          kDeep = k+1
         ENDIF
         dMix = ABS( RF(kDeep) - RF(kIfSrf) )
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( dMix.LT.locMLD(i,j) ) THEN
            wgt(i,j) = wt*maskC(i,j,k,bi,bj)
           ELSE
            wgt(i,j) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDIF

        IF ( itrMate.EQ.UNSET_I ) THEN
C-    Relax towards a fixed value
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           pTracer(i,j,k,bi,bj,iTracer) = pTracer(i,j,k,bi,bj,iTracer)
     &      + wgt(i,j)*( PTRACERS_preformedFixedValue(iTracer)
     &                 - pTracer(i,j,k,bi,bj,iTracer) )
          ENDDO
         ENDDO
        ELSE
C-    Relax towards another tracer carried by the model
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           pTracer(i,j,k,bi,bj,iTracer) = pTracer(i,j,k,bi,bj,iTracer)
     &      + wgt(i,j)*( pTracer(i,j,k,bi,bj,itrMate)
     &                 - pTracer(i,j,k,bi,bj,iTracer) )
          ENDDO
         ENDDO
        ENDIF

       ENDIF
      ENDDO

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('PTRACERS_PREFORMED_RESET',myThid)
#endif
#endif /* ALLOW_PTRACERS */
      RETURN
      END
