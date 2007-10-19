C $Header: /u/gcmpack/MITgcm/model/src/adams_bashforth3.F,v 1.3 2007/10/19 14:41:39 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ADAMS_BASHFORTH3
C     !INTERFACE:
      SUBROUTINE ADAMS_BASHFORTH3(
     I                     bi, bj, kArg,
     U                     gTracer, gTrNm,
     I                     startAB, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R ADAMS_BASHFORTH3
C     | o Extrapolate forward in time using third order
C     |   Adams-Bashforth method.
C     *==========================================================*
C     | Either apply to tendency (kArg>0) at level k=kArg,
C     |     or apply to state variable (kArg=0) for all levels
C     *==========================================================*
C     \ev
C Extrapolate forward in time using 2 A.B. parameters (alpha,beta),
C either tendency gX :
C \begin{equation*}
C gX^{n+1/2} = (1 + \alpha + \beta) gX^{n}
C              - (\alpha + 2 \beta) gX^{n-1}
C                          + \beta  gX^{n-2}
C \end{equation*}
C     or state variable X :
C \begin{equation*}
C  X^{n+1/2} = (1 + \alpha + \beta) X^{n}
C              - (\alpha + 2 \beta) X^{n-1}
C                          + \beta  X^{n-2}
C \end{equation*}
C with:
C (alpha,beta)=(1/2,5/12) : AB-3, stable until CFL = 0.724
C     (note: beta=0.281105 give the Max stability: 0.78616)
C (alpha,beta)=(1/2+abEps,0) : return to previous quasi AB-2.
C (alpha,beta)=(0,0)         : 1rst.O forward time stepping

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
C     gTracer :: (kArg>0) Tendency at current time     ( units of quantity/sec )
C             :: (kArg=0) state variable at current time
C     gTrNm   :: (kArg>0) Tendencies at previous times ( units of quantity/sec )
C             :: (kArg=0) state variable at previous times
C     startAB :: number of previous time level available to start/restart AB
C     myIter  :: Current time step number
C     myThid  :: my Thread number Id
      INTEGER bi,bj,kArg
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTrNm  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      INTEGER startAB
      INTEGER myIter, myThid

#ifdef ALLOW_ADAMSBASHFORTH_3
C     !LOCAL VARIABLES:
C     == Local variables ==
C     k           :: level index
C     i,j         :: Loop counters
C     m1,m2       :: indices for the 2 previous time-step Tendency
C     ab1,ab2,ab3 :: Adams bashforth extrapolation weights.
      INTEGER i,j, k, m1,m2
      _RL ab0, ab1, ab2
      _RL gTrtmp
CEOP

      m1 = 1 + mod(myIter+1,2)
      m2 = 1 + mod( myIter ,2)

C     Adams-Bashforth timestepping weights
      IF ( myIter.EQ.nIter0 .AND. startAB.EQ.0 ) THEN
       ab0 = 1. _d 0
       ab1 = 0. _d 0
       ab2 = 0. _d 0
      ELSEIF ( (myIter.EQ.nIter0   .AND. startAB.EQ.1)
     &    .OR. (myIter.EQ.1+nIter0 .AND. startAB.EQ.0) ) THEN
       ab0 = 1. _d 0 + alph_AB
       ab1 = -alph_AB
       ab2 = 0. _d 0
      ELSE
       ab0 = 1. _d 0 + alph_AB + beta_AB
       ab1 = -alph_AB - 2.*beta_AB
       ab2 =  beta_AB
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( kArg.EQ.0 ) THEN
C-    Extrapolate forward in time the state variable, with AB weights:
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           gTrtmp = ab0*gTracer(i,j,k,bi,bj)
     &            + ab1*gTrNm(i,j,k,bi,bj,m1)
     &            + ab2*gTrNm(i,j,k,bi,bj,m2)
           gTrNm(i,j,k,bi,bj,m2) = gTracer(i,j,k,bi,bj)
           gTracer(i,j,k,bi,bj)  = gTrtmp
          ENDDO
         ENDDO
        ENDDO
      ELSE
C-    Extrapolate forward in time the tendency, with AB weights:
        k = kArg
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           gTrtmp = ab0*gTracer(i,j,k,bi,bj)
     &            + ab1*gTrNm(i,j,k,bi,bj,m1)
     &            + ab2*gTrNm(i,j,k,bi,bj,m2)
           gTrNm(i,j,k,bi,bj,m2) = gTracer(i,j,k,bi,bj)
           gTracer(i,j,k,bi,bj)  = gTrtmp
         ENDDO
        ENDDO
C---
      ENDIF

#endif /* ALLOW_ADAMSBASHFORTH_3 */

      RETURN
      END
