C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/Attic/timestep_tracer.F,v 1.2 2001/07/12 00:31:59 jmc Exp $
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
#include "GAD.h"

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
      ab15 = 1.+ tauAB
      ab05 = -tauAB

C     Step forward temperature
      IF ( gad_advection_scheme.EQ.ENUM_CENTERED_2ND
     & .OR.gad_advection_scheme.EQ.ENUM_UPWIND_3RD
     & .OR.gad_advection_scheme.EQ.ENUM_CENTERED_4TH ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
          gTrNm1(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &             +deltaTtracer*(
     &                              ab15*gTracer(i,j,k,bi,bj)
     &                             +ab05*gTrNm1(i,j,k,bi,bj)  )
        ENDDO
       ENDDO
      ELSEIF (gad_advection_scheme.EQ.ENUM_FLUX_LIMIT) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
          gTrNm1(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &             +deltaTtracer*gTracer(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSE
       STOP 'TIMESTEP_TRACER: Bad gad_advection_scheme'
      ENDIF

      RETURN
      END
