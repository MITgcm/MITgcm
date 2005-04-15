C $Header: /u/gcmpack/MITgcm/model/src/adams_bashforth3.F,v 1.1 2005/04/15 14:13:04 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ADAMS_BASHFORTH3
C     !INTERFACE:
      SUBROUTINE ADAMS_BASHFORTH3(
     I                     bi, bj, k,
     U                     gTracer, gTrNm,
     I                     myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R ADAMS_BASHFORTH3                                      
C     | o Extrapolate tendencies forward in time using            
C     |   third order Adams-Bashforth method.              
C     *==========================================================*
C     \ev
C Extrapolate using 2 parameters (alpha,beta) :
C \begin{equation*}
C gX^{n+1/2} = (1 + \alpha + \beta) gX^{n}
C              - (\alpha + 2 \beta) gX^{n-1}
C                          + \beta  gX^{n-2}
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
C     bi,bj,k :: Tile and level indices
C     gTracer :: Tendency at current time     ( units of quantity/sec )
C     gTrNm   :: Tendencies at previous times ( units of quantity/sec )
C     myIter  :: Current time step number
C     myThid  :: my Thread number Id
      INTEGER bi,bj,k
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTrNm  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      INTEGER myIter, myThid

#ifdef ALLOW_ADAMSBASHFORTH_3
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j         :: Loop counters
C     m1,m2       :: indices for the 2 previous time-step Tendency
C     ab1,ab2,ab3 :: Adams bashforth extrapolation weights.
      INTEGER i,j, m1,m2
      _RL ab0, ab1, ab2
      _RL gTrtmp
CEOP

      m1 = 1 + mod(myIter+1,2)
      m2 = 1 + mod( myIter ,2)

C     Adams-Bashforth timestepping weights
      IF ( myIter.EQ.0 ) THEN
       ab0 = 1. _d 0
       ab1 = 0. _d 0
       ab2 = 0. _d 0
      ELSEIF ( myIter.EQ.1
     &   .OR. (myIter.EQ.nIter0 .AND. startFromPickupAB2) ) THEN
       ab0 = 1. _d 0 + alph_AB
       ab1 = -alph_AB
       ab2 = 0. _d 0
      ELSE
       ab0 = 1. _d 0 + alph_AB + beta_AB
       ab1 = -alph_AB - 2.*beta_AB
       ab2 =  beta_AB
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Compute effective G-term with Adams-Bashforth weights:
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        gTrtmp = ab0*gTracer(i,j,k,bi,bj) 
     &         + ab1*gTrNm(i,j,k,bi,bj,m1)
     &         + ab2*gTrNm(i,j,k,bi,bj,m2)
        gTrNm(i,j,k,bi,bj,m2) = gTracer(i,j,k,bi,bj)
        gTracer(i,j,k,bi,bj) = gTrtmp
       ENDDO
      ENDDO

#endif /* ALLOW_ADAMSBASHFORTH_3 */

      RETURN
      END
