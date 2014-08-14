C $Header: /u/gcmpack/MITgcm/model/src/adams_bashforth2.F,v 1.10 2014/08/14 16:49:19 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ADAMS_BASHFORTH2
C     !INTERFACE:
      SUBROUTINE ADAMS_BASHFORTH2(
     I                     bi, bj, kArg, kSize,
     U                     gTracer, gTrNm1,
     O                     AB_gTr,
     I                     startAB, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R ADAMS_BASHFORTH2
C     | o Extrapolate forward in time using quasi-second order
C     |   Adams-Bashforth method.
C     *==========================================================*
C     | Either apply to tendency (kArg>0) at level k=kArg,
C     |     or apply to state variable (kArg=0) for all levels
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
C     bi,bj   :: Tile indices
C     kArg    :: if >0: apply AB on tendency at level k=kArg
C             :: if =0: apply AB on state variable and process all levels
C     kSize   :: 3rd dimension of tracer and tendency arrays
C     gTracer ::  in: Tendency/State at current time
C             :: out (kArg >0): Extrapolated Tendency at current time
C     gTrNm1  ::  in: Tendency/State at previous time
C             :: out: (kArg >0) Save tendency at current time
C             :: out: (kArg =0) Extrapolated State at current time
C     AB_gTr  :: Adams-Bashforth tendency increment
C     startAB :: number of previous time level available to start/restart AB
C     myIter  :: Current time step number
C     myThid  :: my Thread Id. number
      INTEGER bi, bj, kArg, kSize
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize)
      _RL  gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize)
      _RL  AB_gTr (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER startAB
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     k          :: level index
C     i,j        :: Loop counters
C     abFac      :: Adams bashforth extrapolation factor
      INTEGER i, j, k
      _RL abFac
CEOP

C     Adams-Bashforth extrapolation factor
      IF ( myIter.EQ.nIter0 .AND. startAB.EQ.0 ) THEN
       abFac = 0. _d 0
      ELSE
       abFac = 0.5 _d 0 + abEps
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( kArg.EQ.0 ) THEN
C-    Extrapolate forward in time the state variable, with AB weights:
        DO k=1,kSize
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           AB_gTr(i,j) = abFac*( gTracer(i,j,k) - gTrNm1(i,j,k) )
           gTrNm1(i,j,k) = gTracer(i,j,k) + AB_gTr(i,j)
          ENDDO
         ENDDO
        ENDDO
      ELSE
C-    Extrapolate forward in time the tendency, with AB weights:
        k = kArg
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           AB_gTr(i,j) = abFac*( gTracer(i,j,k) - gTrNm1(i,j,k) )
           gTrNm1(i,j,k) = gTracer(i,j,k)
           gTracer(i,j,k) = gTracer(i,j,k) + AB_gTr(i,j)
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
