C $Header: /u/gcmpack/MITgcm/model/src/tracers_correction_step.F,v 1.17 2014/07/22 12:04:09 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TRACERS_CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE TRACERS_CORRECTION_STEP(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE TRACERS_CORRECTION_STEP
C     *==========================================================*
C     |1rst Part : Update T,S.
C     |
C     | The arrays used for time stepping are cycled.
C     | Tracers:
C     |           T(n) = Gt(n)
C     |
C     |part1: update T,S
C     |  T* (contained in gT) is copied to T (theta)
C     |  S* (contained in gS) is copied to S (salt)
C     |
C     |part2: Adjustments & Diagnostics
C     |   o Filter  T,S (Shapiro Filter, Zonal_Filter)
C     |   o Convective Adjustment
C     |   o Diagmnostic of state variables (Time average)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     == Local variables
      INTEGER iMin,iMax
      INTEGER jMin,jMax
      INTEGER bi,bj
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER m2
#endif
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_ADAMSBASHFORTH_3
        IF ( AdamsBashforth_T ) THEN
C-      Save current tracer field (for AB on tracer) and then update tracer
c         iterNb = myIter - 1
c         m1 = 1 + MOD(iterNb+1,2)
c         m2 = 1 + MOD( iterNb ,2)
          m2 = 1 + MOD(myIter-1,2)
          CALL CYCLE_AB_TRACER(
     I               bi, bj,
     U               theta, gT, gtNm(1-OLx,1-OLy,1,1,1,m2),
     I               myTime, myIter, myThid )
        ELSEIF ( tempStepping ) THEN
#else /* ALLOW_ADAMSBASHFORTH_3 */
        IF ( tempStepping ) THEN
#endif /* ALLOW_ADAMSBASHFORTH_3 */
C-      Update tracer fields:  T(n) = T**
          CALL CYCLE_TRACER(
     I               bi, bj,
     U               theta, gT,
     I               myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_ADAMSBASHFORTH_3
        IF ( AdamsBashforth_S ) THEN
C-      Save current tracer field (for AB on tracer) and then update tracer
          m2 = 1 + MOD(myIter-1,2)
          CALL CYCLE_AB_TRACER(
     I               bi, bj,
     U               salt, gS, gsNm(1-OLx,1-OLy,1,1,1,m2),
     I               myTime, myIter, myThid )
        ELSEIF ( saltStepping ) THEN
#else /* ALLOW_ADAMSBASHFORTH_3 */
        IF ( saltStepping ) THEN
#endif /* ALLOW_ADAMSBASHFORTH_3 */
C-      Update tracer fields:  T(n) = T**
          CALL CYCLE_TRACER(
     I               bi, bj,
     U               salt, gS,
     I               myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
C-      Update passive tracer fields:  T(n) = T**
        IF (usePTRACERS)
     &      CALL PTRACERS_CYCLE(bi,bj,myTime,myIter,myThid)
#endif /* ! ALLOW_LONGSTEP */
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_NONHYDROSTATIC
        IF ( implicitIntGravWave ) THEN
          CALL TRACERS_IIGW_CORRECTION(bi,bj, myTime,myIter,myThid)
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

C--    End of 1rst bi,bj loop
       ENDDO
      ENDDO

C--- 2nd Part : Adjustment.
C
C       Static stability is calculated and the tracers are
C       convective adjusted where statically unstable.

C--   Filter (and exchange)
#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT) THEN
        CALL TIMER_START('SHAP_FILT_TS       [TRC_CORR_STEP]',myThid)
        CALL SHAP_FILT_APPLY_TS( theta,salt, myTime, myIter, myThid )
        CALL TIMER_STOP ('SHAP_FILT_TS       [TRC_CORR_STEP]',myThid)
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT) THEN
        CALL TIMER_START('ZONAL_FILT_TS      [TRC_CORR_STEP]',myThid)
        CALL ZONAL_FILT_APPLY_TS( theta, salt, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_TS      [TRC_CORR_STEP]',myThid)
# ifdef ALLOW_PTRACERS
C-      apply to passive tracer fields
        IF (usePTRACERS) THEN
          CALL PTRACERS_ZONAL_FILT_APPLY( myThid )
        ENDIF
# endif
      ENDIF
#endif /* ALLOW_ZONAL_FILT */
C -- Fill negative tracer values (for now, only if Fizhi is running)
#ifdef ALLOW_FIZHI
      IF (useFIZHI .and. saltAdvScheme.eq.2) THEN
        CALL TIMER_START('SALT_FILL       [TRC_CORR_STEP]',myThid)
        CALL SALT_FILL( uVel,vVel,salt,1,myTime,myIter,myThid )
        CALL TIMER_STOP ('SALT_FILL       [TRC_CORR_STEP]',myThid)
      ENDIF
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--     Convectively adjust new fields to be statically stable
        iMin = 1
        iMax = sNx
        jMin = 1
        jMax = sNy
#ifdef ALLOW_OPPS
        IF ( useOPPS ) THEN
         CALL OPPS_INTERFACE(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime, myIter, myThid )
        ELSEIF ( cAdjFreq .NE. 0. ) THEN
#else /* ALLOW_OPPS */
        IF ( cAdjFreq .NE. 0. ) THEN
#endif /* ALLOW_OPPS */
         CALL CONVECTIVE_ADJUSTMENT(
     I        bi, bj, myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_MATRIX
        IF (useMATRIX)
     &  CALL MATRIX_STORE_TENDENCY_IMP( bi, bj, myTime, myIter, myThid )
#endif

C--    End of 2nd bi,bj loop
       ENDDO
      ENDDO

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
