C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/Attic/timestep_tracer.F,v 1.4 2001/08/15 15:51:46 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE TIMESTEP_TRACER(
     I                     bi, bj, iMin, iMax, jMin, jMax, K,
     I                     advection_scheme,
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
#include "GAD.h"

C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      INTEGER advection_scheme
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
       ab15=1.5+abEps
       ab05=-(0.5+abEps)
      ENDIF

C     Step forward temperature
      IF ( advection_scheme.EQ.ENUM_CENTERED_2ND
     & .OR.advection_scheme.EQ.ENUM_UPWIND_3RD
     & .OR.advection_scheme.EQ.ENUM_CENTERED_4TH ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
          gTrNm1(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &             +deltaTtracer*(
     &                              ab15*gTracer(i,j,k,bi,bj)
     &                             +ab05*gTrNm1(i,j,k,bi,bj)  )
        ENDDO
       ENDDO
      ELSEIF (advection_scheme.EQ.ENUM_FLUX_LIMIT) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
          gTrNm1(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &             +deltaTtracer*gTracer(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSE
       STOP 'TIMESTEP_TRACER: Bad advection_scheme'
      ENDIF

      RETURN
      END
