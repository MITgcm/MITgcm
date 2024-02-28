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
      INTEGER iMin,iMax,jMin,jMax,i,j,k
      INTEGER kmld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER ktop(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('PTRACERS_PREFORMED_RESET',myThid)
#endif

      iMin=1-Olx
      iMax=sNx+Olx
      jMin=1-Oly
      jMax=sNy+Oly

C Get arrays of mixed layer depth for preformed tracers
C     DO bj = myByLo(myThid), myByHi(myThid)
C        DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=jMin,jMax
               DO i=iMin,iMax
C Single surface          
                  IF ( usingPCoords ) THEN
                     ktop(i,j) = Nr
                  ELSE
                     ktop(i,j) = 1
                  ENDIF
C Entire mixed layer       
                  kmld(i,j) = 0
                  
                  DO k=1,Nr
                     IF ( maskC(i,j,k,bi,bj).NE.0. _d 0 
     &                  .AND. abs(RF(1+kmld(i,j))) 
     &                  .LT.hMixLayer(i,j,bi,bj) 
     &                  ) 
     &                  kmld(i,j)=kmld(i,j)+1
                  ENDDO
               ENDDO
            ENDDO
C        ENDDO
C     ENDDO


C--   Loop over tracers
      DO iTracer=1,PTRACERS_numInUse
         IF (PTRACERS_isPreformed(iTracer)) THEN
#ifdef ALLOW_DEBUG
            IF (debugMode) THEN
               WRITE(msgBuf,'(A)')
     &           'EVALUATING PASSIVE TRACER',PTRACERS_names(iTracer)
               CALL DEBUG_MSG(msgBuf, myThid)
            ENDIF
#endif 

            DO j=jMin,jMax
               DO i=iMin,iMax
                  IF (PTRACERS_preformedUseMLD(iTracer)) THEN
                     kind(i,j) = kmld(i,j)
                  ELSE
                     kind(i,j) = ktop(i,j)
                  ENDIF
               ENDDO
            ENDDO

C Preformed tracer tracer set, or relaxed to a constant concentration at the surface
            DO j=jMin,jMax
               DO i=iMin,iMax
                  DO k=1,kind(i,j)
                     IF ( maskC(i,j,k,bi,bj).NE.0. _d 0 ) THEN
                        IF (PTRACERS_preformedMate(iTracer)
     &                                          .EQ.UNSET_I) THEN
C Reset preformed tracer compared to a fixed value
                           pTracer(i,j,k,bi,bj,iTracer) = 
     &                          pTracer(i,j,k,bi,bj,iTracer)
     &                        + (
     &                           (PTRACERS_preformedFixedValue(iTracer)
     &                         - pTracer(i,j,k,bi,bj,iTracer))     
     &                         *_hFacC(i,j,k,bi,bj)
     &                         * (PTRACERS_dTLev(k)
     &                             /PTRACERS_tauRelaxPreformed(iTracer))
     &                           )
                        ELSEIF (PTRACERS_preformedMate(iTracer)
     &                                         .EQ.UNSET_RL) THEN
C Reset preformed tracer compared to another tracer carried by the model                    
                           pTracer(i,j,k,bi,bj,iTracer) = 
     &                          pTracer(i,j,k,bi,bj,iTracer)
     &                        + (
     &                           (pTracer(i,j,k,bi,bj,
     &                                  PTRACERS_preformedMate(iTracer))
     &                         - pTracer(i,j,k,bi,bj,iTracer))     
     &                         *_hFacC(i,j,k,bi,bj)
     &                         * (PTRACERS_dTLev(k)
     &                             /PTRACERS_tauRelaxPreformed(iTracer))
     &                           )
                        ENDIF
                     ELSE
                        pTracer(i,j,k,bi,bj,iTracer)=0. _d 0
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
      ENDDO
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('PTRACERS_PREFORMED_RESET',myThid)
#endif
#endif /* ALLOW_PTRACERS */
      RETURN
      END
