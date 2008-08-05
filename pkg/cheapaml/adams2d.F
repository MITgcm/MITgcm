C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/Attic/adams2d.F,v 1.1 2008/08/05 21:49:31 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ADAMS2d
C     !INTERFACE:
      SUBROUTINE ADAMS2d(
     I                     bi, bj, 
     U                     gTracer, gTrNm1,
     I                     startAB, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R ADAMS2d                                      
C     | o Extrapolate tendencies forward in time using            
C     |   quasi-second order Adams-Bashforth method.              
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj :: Tile indices
C     gTracer :: Tendency at current time  ( generally units of quantity/sec )
C     gTrNm1  :: Tendency at previous time ( generally units of quantity/sec )
C     startAB :: number of previous time level available to start/restart AB
C     myIter  :: Current time step number
C     myThid  :: Thread number of this thread
      INTEGER bi,bj
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER startAB
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j        :: Loop counters
C     ab15, ab05 :: Adams bashforth extrapolation weights.
      INTEGER i,j
      _RL ab15,ab05
      _RL gTrtmp
CEOP

C     Adams-Bashforth timestepping weights
      IF ( myIter.EQ.nIter0 .AND. startAB.EQ.0 ) THEN
        ab15 = 1. _d 0
        ab05 = 0. _d 0
      ELSE
        ab15 = 1.5 _d 0 + abEps
        ab05 = -( 0.5 _d 0 + abEps )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+---- 

C-    Compute effective G-term with Adams-Bashforth weights:
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        gTrtmp = ab15*gTracer(i,j,bi,bj) 
     &         + ab05*gTrNm1(i,j,bi,bj)
        gTrNm1(i,j,bi,bj) = gTracer(i,j,bi,bj)
        gTracer(i,j,bi,bj) = gTrtmp
       ENDDO
      ENDDO

      RETURN
      END
