#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ADAMS_BASHFORTH3
C     !INTERFACE:
      SUBROUTINE ADAMS_BASHFORTH3(
     I                     bi, bj, kArg, kSize,
     U                     gTracer, gTrNm,
     O                     AB_gTr,
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
C (alpha,beta)=(0,0)         : 1rst.Order forward time stepping

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
C     kSize   :: 3rd dimension of gTracer
C     gTracer ::  in: Tendency/State at current time
C             :: out(kArg >0): Extrapolated Tendency at current time
C     gTrNm   ::  in: Tendency/State at previous time
C             :: out(kArg >0): Save tendency at current time
C             :: out(kArg =0): Extrapolated State at current time
C     AB_gTr  :: Adams-Bashforth tendency increment
C     startAB :: number of previous time level available to start/restart AB
C     myIter  :: Current time step number
C     myThid  :: my Thread Id. number
      INTEGER bi, bj, kArg, kSize
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize)
      _RL  gTrNm  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL  AB_gTr (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER startAB
      INTEGER myIter, myThid

#ifdef ALLOW_ADAMSBASHFORTH_3
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j        :: Loop counters
C     k, kl       :: level indices
C     m1,m2       :: indices for the 2 previous time-step Tendency
C     ab1,ab2,ab3 :: Adams bashforth extrapolation weights.
      INTEGER i,j, k, kl, m1,m2
      _RL ab0, ab1, ab2
CEOP

      m1 = 1 + MOD(myIter+1,2)
      m2 = 1 + MOD( myIter ,2)

C     Adams-Bashforth timestepping weights
      IF ( myIter.EQ.nIter0 .AND. startAB.EQ.0 ) THEN
       ab0 = 0. _d 0
       ab1 = 0. _d 0
       ab2 = 0. _d 0
      ELSEIF ( (myIter.EQ.nIter0   .AND. startAB.EQ.1)
     &    .OR. (myIter.EQ.1+nIter0 .AND. startAB.EQ.0) ) THEN
       ab0 =  alph_AB
       ab1 = -alph_AB
       ab2 = 0. _d 0
      ELSE
       ab0 =  alph_AB + beta_AB
       ab1 = -alph_AB - 2.*beta_AB
       ab2 =  beta_AB
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( kArg.EQ.0 ) THEN
C-    Extrapolate forward in time the state variable, with AB weights:
        DO k=1,kSize
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           AB_gTr(i,j) = ab0*gTracer(i,j,k)
     &                 + ab1*gTrNm(i,j,k,bi,bj,m1)
     &                 + ab2*gTrNm(i,j,k,bi,bj,m2)
           gTrNm(i,j,k,bi,bj,m2) = gTracer(i,j,k) + AB_gTr(i,j)
          ENDDO
         ENDDO
        ENDDO
      ELSE
C-    Extrapolate forward in time the tendency, with AB weights:
        kl = kArg
        k = MIN( kArg, kSize )
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           AB_gTr(i,j) = ab0*gTracer(i,j,k)
     &                 + ab1*gTrNm(i,j,kl,bi,bj,m1)
     &                 + ab2*gTrNm(i,j,kl,bi,bj,m2)
           gTrNm(i,j,kl,bi,bj,m2) = gTracer(i,j,k)
           gTracer(i,j,k) = gTracer(i,j,k) + AB_gTr(i,j)
         ENDDO
        ENDDO
C---
      ENDIF

#endif /* ALLOW_ADAMSBASHFORTH_3 */

      RETURN
      END
