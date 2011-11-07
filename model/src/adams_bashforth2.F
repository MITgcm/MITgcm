C $Header: /u/gcmpack/MITgcm/model/src/adams_bashforth2.F,v 1.8 2011/11/07 20:20:31 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ADAMS_BASHFORTH2
C     !INTERFACE:
      SUBROUTINE ADAMS_BASHFORTH2(
     I                     bi, bj, k,
     U                     gTracer, gTrNm1,
     O                     AB_gTr,
     I                     startAB, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R ADAMS_BASHFORTH2
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
C     bi,bj,k :: Tile and level indices
C     gTracer ::  in: Tendency at current time  ( generally units of quantity/sec )
C             :: out: Extrapolated Tendency at current time ( same units )
C     gTrNm1  ::  in: Tendency at previous time             ( same units )
C             :: out: Save tendency at current time         ( same units )
C     AB_gTr  :: Adams-Bashforth tendency increment
C     startAB :: number of previous time level available to start/restart AB
C     myIter  :: Current time step number
C     myThid  :: my Thread Id. number
      INTEGER bi,bj,k
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  AB_gTr (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER startAB
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j        :: Loop counters
C     abFac      :: Adams bashforth extrapolation factor
      INTEGER i,j
      _RL abFac
CEOP

C     Adams-Bashforth extrapolation factor
      IF ( myIter.EQ.nIter0 .AND. startAB.EQ.0 ) THEN
       abFac = 0. _d 0
      ELSE
       abFac = 0.5 _d 0 + abEps
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Compute effective G-term with Adams-Bashforth weights:
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        AB_gTr(i,j) = abFac
     &              *( gTracer(i,j,k,bi,bj)- gTrNm1(i,j,k,bi,bj) )
        gTrNm1(i,j,k,bi,bj) = gTracer(i,j,k,bi,bj)
        gTracer(i,j,k,bi,bj) = gTracer(i,j,k,bi,bj) + AB_gTr(i,j)
       ENDDO
      ENDDO

      RETURN
      END
