C $Header: /u/gcmpack/MITgcm/model/src/timestep_tracer.F,v 1.1 2001/02/02 21:35:19 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R TIMESTEP_TRACER                                      |
C     | o Step model tracer field forward in time                |
C     \==========================================================/
      SUBROUTINE TIMESTEP_TRACER(
     I                     bi, bj, iMin, iMax, jMin, jMax, K,
     U                     tracer, gTracer, gTrNm1,
     I                     myIter, myThid )
      implicit none
! Common
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL  tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter, myThid
C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05

C     Adams-Bashforth timestepping weights
Caja  IF (myIter .EQ. 0) THEN
Caja   ab15=1.0
Caja   ab05=0.0
Caja  ELSE
       ab15=1.5+abeps
       ab05=-0.5-abeps
Caja  ENDIF

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
