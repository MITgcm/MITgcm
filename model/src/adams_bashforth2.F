C $Header: /u/gcmpack/MITgcm/model/src/adams_bashforth2.F,v 1.1 2001/09/19 02:43:27 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE ADAMS_BASHFORTH2(
     I                     bi, bj, K,
     U                     gTracer, gTrNm1,
     I                     myIter, myThid )
C     /==========================================================\
C     | S/R ADAMS_BASHFORTH2                                     |
C     | o Extrapolate tendancies forward in time using           |
C     |   quasi-second order Adams-Bashforth method.             |
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
      INTEGER bi,bj,K
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter, myThid
C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05
      _RL gTrtmp

C     Adams-Bashforth timestepping weights
      IF (myIter .EQ. 0) THEN
       ab15=1.0
       ab05=0.0
      ELSE
       ab15=1.5+abEps
       ab05=-(0.5+abEps)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Compute effective G-term with Adams-Bashforth weights:
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        gTrtmp = ab15*gTracer(i,j,k,bi,bj) 
     &         + ab05*gTrNm1(i,j,k,bi,bj)
        gTrNm1(i,j,k,bi,bj) = gTracer(i,j,k,bi,bj)
        gTracer(i,j,k,bi,bj) = gTrtmp
       ENDDO
      ENDDO

      RETURN
      END
