#include "PTRACERS_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: PTRACERS_PREFORMED_RESET
C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_PREFORMED_RESET( bi, bj, 
     &          myTime, myIter, myThid )

C !DESCRIPTION:
C     calls subroutine that will update passive preformed tracer values
C     with a separate timestep. 

C !USES: ===============================================================
#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP_PARAMS.h"
#endif
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"
#include "PTRACERS_FIELDS.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi, bj           :: tile indices
C  recip_hFac       :: reciprocal of cell open-depth factor (@ next iter)
C  myTime           :: model time
C  myIter           :: time-step number
C  myThid           :: thread number
CEOP

      INTEGER bi, bj, iTracer
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif /* ALLOW_DIAGNOSTICS */

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j,k,kt,itrMate
      _RL recip_tau
      INTEGER kmld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER ktop(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('PTRACERS_PREFORMED_RESET',myThid)
#endif

C Get arrays of mixed layer depth for preformed tracers
C     Single surface
      IF ( usingPCoords ) THEN
       kt = Nr
      ELSE
       kt = 1
      ENDIF
      DO j=jMin,jMax
       DO i=iMin,iMax
        ktop(i,j) = 0
        IF ( maskC(i,j,k,bi,bj).NE.0. _d 0 ) ktop(i,j) = kt
       ENDDO
      ENDDO
C Entire mixed layer
      DO k=1,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
C     for z-coordinates, RF(k+1) is below the C-point
C     for p-coordinates, RF(k+1) is above the C-point
         IF ( maskC(i,j,k,bi,bj).NE.0. _d 0
     &        .AND. ABS(RF(k+1)).LT.hMixLayer(i,j,bi,bj)
     &        ) THEN
          kmld(i,j)=k
         ELSE
          kmld(i,j)=0
        ENDDO
       ENDDO
      ENDDO

C--   Loop over tracers
      DO iTracer=1,PTRACERS_numInUse
       IF (PTRACERS_isPreformed(iTracer)) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) THEN
         WRITE(msgBuf,'(A)')
     &    'EVALUATING PASSIVE TRACER',PTRACERS_names(iTracer)
         CALL DEBUG_MSG(msgBuf, myThid)
        ENDIF
#endif 

        IF (PTRACERS_preformedUseMLD(iTracer)) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           kind(i,j) = kmld(i,j)
          ENDDO
         ENDDO
        ELSE
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           kind(i,j) = ktop(i,j)
          ENDDO
         ENDDO
        ENDIF
        
C Preformed tracer tracer set, or relaxed to a constant concentration at the surface
        recip_tau = 1. _d 0/PTRACERS_tauRelaxPreformed(iTracer)
C If PTRACERS_preformedMate is not set then use the fixed value, which
C   is set to zero by default
        IF (PTRACERS_preformedMate(iTracer).EQ.UNSET_I) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           DO k=1,kind(i,j)    
C Reset preformed tracer compared to a fixed value
             pTracer(i,j,k,bi,bj,iTracer) = 
     &         pTracer(i,j,k,bi,bj,iTracer)
     &        + (
     &          (PTRACERS_preformedFixedValue(iTracer)
     &        - pTracer(i,j,k,bi,bj,iTracer))     
     &        * (_hFacC(i,j,k,bi,bj)
     &        * PTRACERS_dTLev(k) * recip_tau)
     &          )
           ENDDO 
          ENDDO
         ENDDO
        ELSE
C If PTRACERS_preformedMate is set then reset preformed tracer 
C  towards another tracer carried by the model    
         itrMate = PTRACERS_preformedMate(iTracer)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           DO k=1,kind(i,j)   
             pTracer(i,j,k,bi,bj,iTracer) =
     &         pTracer(i,j,k,bi,bj,iTracer)
     &        + (
     &          (pTracer(i,j,k,bi,bj,itrMate)
     &        - pTracer(i,j,k,bi,bj,iTracer))
     &        * (_hFacC(i,j,k,bi,bj)
     &        * PTRACERS_dTLev(k) * recip_tau)
     &          )
           ENDDO
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
