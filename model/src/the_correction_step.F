C $Header: /u/gcmpack/MITgcm/model/src/Attic/the_correction_step.F,v 1.6 2001/03/08 20:32:35 jmc Exp $
C Tag $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE THE_CORRECTION_STEP(myTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE THE_CORRECTION_STEP                           |
C     |==========================================================|
C     |                                                          |
C     | U*,V* (contained in gUnm1,gVnm1) have the surface        |
C     |    pressure gradient term added and the result stored in |
C     |    U,V (contained in uVel, vVel)                         |
C     |                                                          |
C     | T* (contained in gTnm1) is copied to T (theta)           |
C     |                                                          |
C     | S* (contained in gSnm1) is copied to S (salt)            |
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables
      _RL phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax
      INTEGER jMin,jMax
      INTEGER bi,bj
      INTEGER k,i,j

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--     Set up work arrays that need valid initial values
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          phiSurfX(i,j)=0.
          phiSurfY(i,j)=0.
         ENDDO
        ENDDO

C       The arrays used for time stepping are cycled.
C
C       Tracers:
C                 T(n) = Gt(n-1)
C                 Gt(n-1) = Gt(n)
C       Momentum:
C                 V(n) = Gv(n-1) - dt * grad Eta
C                 Gv(n-1) = Gv(n)
C
C       Static stability is calculated and the tracers are
C       convective adjusted where statically unstable.

C       Loop range: Gradients of Eta are evaluated so valid
C       range is all but first row and column in overlaps.
        iMin = 1-OLx+1
        iMax = sNx+OLx
        jMin = 1-OLy+1
        jMax = sNy+OLy

C-      Calculate gradient of surface Potentiel
        CALL CALC_GRAD_PHI_SURF(
     I       bi,bj,iMin,iMax,jMin,jMax,
     I       etaN,
     O       phiSurfX,phiSurfY,
     I       myThid )

C--     Loop over all layers, top to bottom
        DO K=1,Nr

C-        Update velocity fields:  V(n) = V** - dt * grad Eta
          IF (momStepping)
     &      CALL CORRECTION_STEP(
     I         bi,bj,iMin,iMax,jMin,jMax,K,
     I         phiSurfX,phiSurfY,myTime,myThid )

C-        Update tracer fields:  T(n) = T**, Gt(n-1) = Gt(n)
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

#ifdef    ALLOW_OBCS
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uvel (:,:,k,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE vvel (:,:,k,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          IF (useOBCS) THEN
            CALL OBCS_APPLY_UV(bi,bj,K,uVel,vVel,myThid)
          ENDIF
#endif    /* ALLOW_OBCS */

C--     End DO K=1,Nr
        ENDDO

C--     Convectively adjust new fields to be statically stable
        CALL CONVECTIVE_ADJUSTMENT(
     I       bi, bj, iMin, iMax, jMin, jMax,
     I       myTime, myIter, myThid )

#ifdef ALLOW_TIMEAVE
        IF (taveFreq.GT.0.) THEN 
          CALL TIMEAVE_STATVARS(myTime, myIter, bi, bj, myThid)
        ENDIF
#endif /* ALLOW_TIMEAVE */

       ENDDO
      ENDDO

      RETURN
      END
