C $Header: /u/gcmpack/MITgcm/model/src/timestep_tracer.F,v 1.3 2001/07/31 15:01:33 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE TIMESTEP_TRACER(
     I                     bi, bj, iMin, iMax, jMin, jMax, K, tauAB,
     U                     tracer, gTracer, gTrNm1,
     I                     myIter, myThid )
C     /==========================================================\
C     | S/R TIMESTEP_TRACER                                      |
C     | o Step model tracer field forward in time                |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine Arguments ==
C     tauAB - Adams-Bashforth timestepping weight: 0=forward ; 1/2=Adams-Bashf.
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL  tauAB
      _RL  tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter, myThid
C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05

C     Adams-Bashforth timestepping weights
      IF (myIter .EQ. 0) THEN
       ab15=1.0
       ab05=0.0
      ELSE
       ab15=1.+tauAB
       ab05=-tauAB
      ENDIF

C     Step forward temperature
      DO j=jMin,jMax
        DO i=iMin,iMax
          gTrNm1(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &             +deltaTtracer*(
     &                              ab15*gTracer(i,j,k,bi,bj)
     &                             +ab05*gTrNm1(i,j,k,bi,bj)  )
        ENDDO
      ENDDO

      RETURN
      END
