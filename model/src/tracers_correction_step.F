C $Header: /u/gcmpack/MITgcm/model/src/tracers_correction_step.F,v 1.5 2004/09/17 22:57:12 heimbach Exp $
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables
      INTEGER iMin,iMax
      INTEGER jMin,jMax
      INTEGER bi,bj
      INTEGER k,i,j

CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C       Loop range: Gradients of Eta are evaluated so valid
C       range is all but first row and column in overlaps.
        iMin = 1-OLx+1
        iMax = sNx+OLx
        jMin = 1-OLy+1
        jMax = sNy+OLy

C--     Loop over all layers, top to bottom
        DO K=1,Nr

C-        Update tracer fields:  T(n) = T**, Gt(n-1) = Gt(n)
#ifndef ALLOW_OFFLINE
          IF (tempStepping)
     &      CALL CYCLE_TRACER(
     I           bi,bj,iMin,iMax,jMin,jMax,K,
     U           theta,gT,gTNm1,
     I           myTime,myThid )
          IF (saltStepping)
     &      CALL CYCLE_TRACER(
     I           bi,bj,iMin,iMax,jMin,jMax,K,
     U           salt,gS,gSNm1,
     I           myTime,myThid )
#endif
#ifdef ALLOW_PTRACERS
C-        Update passive tracer fields:  T(n) = T**, Gt(n-1) = Gt(n)
          IF (usePTRACERS)
     &        CALL PTRACERS_CYCLE(bi,bj,k,myIter,myTime,myThid)
#endif /* ALLOW_PTRACERS */

C--     End DO K=1,Nr
        ENDDO

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
        CALL SHAP_FILT_APPLY_TS( theta,salt, myTime, myIter, myThid )
      ENDIF
#endif 
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT) THEN
        CALL ZONAL_FILT_APPLY_TS( theta, salt, myThid )  
      ENDIF
#endif 

#ifndef ALLOW_OFFLINE
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
        ELSE
#endif /* ALLOW_OPPS */
         CALL CONVECTIVE_ADJUSTMENT(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime, myIter, myThid )
#ifdef ALLOW_OPPS
        ENDIF
#endif /* ALLOW_OPPS */

C--    End of 2nd bi,bj loop
       ENDDO
      ENDDO
#endif

      RETURN
      END
