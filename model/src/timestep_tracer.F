C $Header: /u/gcmpack/MITgcm/model/src/timestep_tracer.F,v 1.6 2001/09/04 14:53:10 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE TIMESTEP_TRACER(
     I                     bi, bj, iMin, iMax, jMin, jMax, K,
     I                     advection_scheme,
     I                     tracer, gTracer,
     O                     gTrNm1,
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
#include "GRID.h"
#include "SURFACE.h"

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
      _RL gTrtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     Adams-Bashforth timestepping weights
      IF (myIter .EQ. 0) THEN
       ab15=1.0
       ab05=0.0
      ELSE
       ab15=1.5+abEps
       ab05=-(0.5+abEps)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Compute effective G-term:
      IF ( advection_scheme.EQ.ENUM_CENTERED_2ND
     & .OR.advection_scheme.EQ.ENUM_UPWIND_3RD
     & .OR.advection_scheme.EQ.ENUM_CENTERED_4TH ) THEN

C-    with Adams-Bashforth weights:
       DO j=jMin,jMax
        DO i=iMin,iMax
         gTrtmp(i,j) = ab15*gTracer(i,j,k,bi,bj) 
     &               + ab05*gTrNm1(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF (advection_scheme.EQ.ENUM_FLUX_LIMIT
     &    .OR.advection_scheme.EQ.ENUM_DST3
     &    .OR.advection_scheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN

C-    with no Adams-Bashforth weights:
       DO j=jMin,jMax
        DO i=iMin,iMax
         gTrtmp(i,j) = gTracer(i,j,k,bi,bj) 
        ENDDO
       ENDDO

      ELSE
       STOP 'TIMESTEP_TRACER: Bad advection_scheme'
      ENDIF

#ifdef NONLIN_FRSURF
      IF (nonlinFreeSurf.GT.0) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF (k.EQ.ksurfC(i,j,bi,bj)) THEN
          gTrtmp(i,j) = gTrtmp(i,j)
     &          *hFacC(i,j,k,bi,bj)/hFac_surfC(i,j,bi,bj) 
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif /* NONLIN_FRSURF */

C-    Step forward tracer
      DO j=jMin,jMax
       DO i=iMin,iMax
         gTrNm1(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &            +deltaTtracer*gTrtmp(i,j)
       ENDDO
      ENDDO

      RETURN
      END
