#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
CStartOfInterface
      SUBROUTINE SEAICE_CALC_VISCOSITIES(
     I     e11, e22, e12, zMin, zMax, HEFFM, press0, tnsFac,
     O     eta, etaZ, zeta, zetaZ, press, deltaC,
     I     iStep, myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE  SEAICE_CALC_VISCOSITIES                      |
C     | o compute shear and bulk viscositites eta, zeta and the  |
C     |   corrected ice strength P                               |
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997)    |
C     *==========================================================*
C     | written by Martin Losch, Mar 2006                        |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

C     === Routine arguments ===
C     iStep  :: Sub-time-step number
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: My Thread Id. number
      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     strain rate tensor
      _RL e11   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
      _RL zMin  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zMax  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HEFFM (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
      _RL press0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL press (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
      _RL deltaC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     factor k to compute tensile strength from k*press0
      _RL tnsFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     bulk viscosity
      _RL  eta  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etaZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     shear viscosity
      _RL zeta  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEndOfInterface

#if ( defined (SEAICE_CGRID) && defined (SEAICE_ALLOW_DYNAMICS) )
C     === Local variables ===
C     i,j,bi,bj - Loop counters
C     e11, e12, e22 - components of strain rate tensor
C     recip_e2      - inverse of square of ratio of yield curve main axes
C     ep            - e11+e22 (abbreviation)
C     em            - e11-e22 (abbreviation)
      INTEGER i, j, bi, bj
      _RL recip_e2, deltaCsq, deltaMinSq, tmp, ep, em
      _RL recip_efr4, recip_efr2, oneThird, oneNinth
      _RL e12Csq   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltaCreg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shearDefSq, shearDef
#ifdef SEAICE_ALLOW_TEARDROP
      _RL k,ksq,x,a,ap1,etamax_TD,zetamax_TD,cyc,xma
      _RL tnsFacTD, recip_eTD2
#endif /* SEAICE_ALLOW_TEARDROP */
#if (defined SEAICE_ALLOW_MCE || defined SEAICE_ALLOW_TEM)
      _RL etaMaxFac
#endif /* SEAICE_ALLOW_MCE or SEAICE_ALLOW_TEM */
      _RL sumNorm, maskZ
#ifdef SEAICE_ZETA_SMOOTHREG
      _RL argTmp
#endif /* SEAICE_ZETA_SMOOTHREG */
CEOP

C--   basic constants
      oneThird = 1. _d 0 / 3. _d 0
      oneNinth = 1. _d 0 / 9. _d 0

      recip_e2=0. _d 0
      IF ( SEAICE_eccen .NE. 0. _d 0 ) recip_e2=ONE/(SEAICE_eccen**2)
      recip_efr2=0. _d 0
      IF( SEAICE_eccfr .NE. 0. _d 0) recip_efr2=ONE/(SEAICE_eccfr**2)
      recip_efr4=0. _d 0
      IF( SEAICE_eccfr .NE. 0. _d 0) recip_efr4=(SEAICE_eccen**2)
     &   / (SEAICE_eccfr**4)
      deltaMinSq = SEAICE_deltaMin**2
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          deltaCreg(i,j) = SEAICE_deltaMin
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */
C     need to do this beforehand for easier vectorization after
C     TAFization
        IF ( SEAICEetaZmethod .EQ. 0 ) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           tmp = 0.25 *
     &          ( e12(I,J  ,bi,bj) + e12(I+1,J  ,bi,bj)
     &          + e12(I,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
           e12Csq(i,j) = tmp*tmp
          ENDDO
         ENDDO
        ELSEIF ( SEAICEetaZmethod .EQ. 3 ) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
C     area weighted average of the squares of e12 is more accurate
C     (and energy conserving)
           e12Csq(i,j) = 0.25 _d 0 * recip_rA(I,J,bi,bj) *
     &          ( rAz(I  ,J  ,bi,bj)*e12(I  ,J  ,bi,bj)**2
     &          + rAz(I+1,J  ,bi,bj)*e12(I+1,J  ,bi,bj)**2
     &          + rAz(I  ,J+1,bi,bj)*e12(I  ,J+1,bi,bj)**2
     &          + rAz(I+1,J+1,bi,bj)*e12(I+1,J+1,bi,bj)**2 )
          ENDDO
         ENDDO
        ENDIF

C     select between different yield curves; default: elliptical yield curve
        IF ( .FALSE. ) THEN
C     do nothing (hack to be able add code with different cpp-flag)
#ifdef SEAICE_ALLOW_TEARDROP
        ELSEIF ( SEAICEuseTD ) THEN

         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           ep = e11(i,j,bi,bj) + e22(i,j,bi,bj)
           em = e11(i,j,bi,bj) - e22(i,j,bi,bj)
           shearDef = SQRT(em*em + 4. _d 0 * e12Csq(i,j))

           k   = ep / (shearDef+ deltaMinSq)
           ksq = k * k
           a   = tnsFac(I,J,bi,bj)

C      CYC Coordinate of the highest point of the yield curve, here eI changes sign
           cyc=(TWO - tnsFac(I,J,bi,bj) ) * oneThird
           ap1 = a + ONE

C      Handling errors when k is negative and very large 
           IF ( k < -1e3 ) THEN 
            x = (3. _d 0 * ap1) 
     &        * ( - 1. _d 0 + 0.125 _d 0 *(3. _d 0 * ap1 ) / ksq )
           ELSE
            x = TWO * ksq + TWO *k*sqrt(ksq + 3 _d 0 * ap1 )
           endif
           x = (x - (6 _d 0 * ap1) ) * oneNinth + a
C      Original formulation of Zhang 2005:
C            x = ( TWO * ksq + TWO *k*sqrt(ksq + 3 _d 0 * ap1 )  
C      &         - (6 _d 0 * ap1) ) * oneNinth + a
           
C      Replacing tip by an ellipse for tensile stresses
           IF (x .GT. 0 _d 0 .AND. SEAICE_TD_EllTip ) THEN

            tnsFacTD = a / TWO
            
            IF (tnsFacTD .GT. 0 _d 0 ) THEN 
             recip_eTD2 = a*a / tnsFacTD
            ELSE
             recip_eTD2 = ONE  
            ENDIF

            shearDefSq = em*em + 4. _d 0*e12Csq(i,j)
            deltaC(I,J,bi,bj) = SQRT(ep*ep + recip_eTD2*shearDefSq)
            deltaCreg(I,J) = MAX(deltaC(I,J,bi,bj),SEAICE_deltaMin)
            zeta(I,J,bi,bj) = HALF*( press0(I,J,bi,bj)
     &          * ( ONE + tnsFacTD ) )/deltaCreg(I,J)
            zeta(I,J,bi,bj)= MIN(zeta(I,J,bi,bj),ZMAX(I,J,bi,bj))
            eta(I,J,bi,bj) = recip_eTD2 * zeta(I,J,bi,bj)
            press(I,J,bi,bj) =
     &          ( press0(I,J,bi,bj)*( ONE - SEAICEpressReplFac )
     &          + TWO*zeta(I,J,bi,bj)*deltaC(I,J,bi,bj)
     &          * SEAICEpressReplFac/( ONE + tnsFacTD )
     &          ) * ( ONE - tnsFacTD )
        
C   Teardrop yield curve
           ELSE 
C     capping x at a is probably better that setting it to a for x>1,
C     but still not ideal (because non-differentiable)
C     possible alternative: log(exp(rho*x)+exp(rho*a))/rho, with rho=O(100)
            x = MIN( x, a )
C             x = MAX( x,-1 _d 0 ) ! to be sure

            zeta(I,J,bi,bj) = (x + cyc) * press0(I,J,bi,bj) 
     &          / SIGN( MAX( ABS(ep),deltaMinSq ), ep)
C      Original formulation of Zhang 2005    
C            zeta(I,J,bi,bj) = (x + 0.5 _d 0) * press0(I,J,bi,bj)
C      &          / SIGN( ABS(ep) + SEAICE_deltaMin, ep)
C      &                    / SIGN( MAX( ABS(ep),deltaMinSq ), 0.5 _d 0 * ep)

            eta(I,J,bi,bj) = -(x-a) * SQRT(ONE+x) * press0(I,J,bi,bj)
     &                     / (shearDef + deltaMinSq)

C      Defining the max values for eta and zeta
           etamax_TD = ZMAX(I,J,bi,bj) * MIN(eta(I,J,bi,bj)
     &               /(ABS(zeta(I,J,bi,bj)) + deltaMinSq), ONE)
           zetamax_TD =ZMAX(I,J,bi,bj) * MIN(zeta(I,J,bi,bj)
     &               /(ABS(eta(I,J,bi,bj)) + deltaMinSq), ONE )    

C      Applying the max values
           zeta(I,J,bi,bj) = MIN(zeta(I,J,bi,bj), zetamax_TD)
           eta(I,J,bi,bj) =  MIN( eta(I,J,bi,bj),  etamax_TD)
C      Original formulation of Zhang 2005
C            zeta(I,J,bi,bj) = MIN(zeta(I,J,bi,bj), ZMAX(I,J,bi,bj))
C            eta(I,J,bi,bj) =  MIN( eta(I,J,bi,bj), ZMAX(I,J,bi,bj))

C      To be sure that viscosities are positive (should not be needed)
C          zeta(I,J,bi,bj) = MAX(zeta(I,J,bi,bj),deltaMinSq)
C          eta(I,J,bi,bj)  = MAX( eta(I,J,bi,bj),deltaMinSq)

C      Compute the replacement pressure
           press(I,J,bi,bj) = TWO * cyc * (
     &          press0(I,J,bi,bj)*( ONE - SEAICEpressReplFac )
     &         + SEAICEpressReplFac * ep * zeta(I,J,bi,bj)
     &         / SIGN(MAX(ABS(x+cyc), deltaMinSq), x+cyc) )

C      Reducing eta with the replacement pressure
           eta(I,J,bi,bj) = eta(I,J,bi,bj) * (ONE - SEAICEpressReplFac) 
     &         + eta(I,J,bi,bj) * press(I,J,bi,bj) * SEAICEpressReplFac 
     &         / (press0(I,J,bi,bj) * TWO  * cyc + deltaMinSq)
  
           ENDIF ! ellipse for tensile deformation stress 

          ENDDO ! x loop
         ENDDO ! y loop

        ELSEIF ( SEAICEusePL ) THEN

         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           ep = ( e11(i,j,bi,bj) + e22(i,j,bi,bj) )
           em = e11(i,j,bi,bj)-e22(i,j,bi,bj)
           shearDef = SQRT(em*em + 4. _d 0 * e12Csq(i,j))
           k = ep / (shearDef+SEAICE_deltaMin) 
           a = tnsFac(I,J,bi,bj)
           IF     (ksq .GE.  1e16) THEN 
            x =  a
           ELSEIF (ksq .LE. -1e16) THEN
            x = -1 _d 0
           ELSE 
            x = 0.5 _d 0 * (k - 1. _d 0 + a)
           ENDIF
           x = MAX( x,-1. _d 0 )
           x = MIN( x, a )
           xma = x + 0.5 _d 0 *(1. _d 0 - a )
C           compute ZETA
           zeta(I,J,bi,bj) = xma * press0(I,J,bi,bj)
     &                    / SIGN( ABS(ep) + SEAICE_deltaMin , ep)
C           compute ETA
           eta(I,J,bi,bj) = -(x-a) * (ONE + x) * press0(I,J,bi,bj)
     &                     / (shearDef+SEAICE_deltaMin)

C          Maximum of eta and zeta
           etamax_TD = ZMAX(I,J,bi,bj) * MIN(eta(I,J,bi,bj)
     &               /(ABS(zeta(I,J,bi,bj)) + SEAICE_deltaMin),ONE )
           zetamax_TD =ZMAX(I,J,bi,bj) * MIN(zeta(I,J,bi,bj)
     &               /(ABS(eta(I,J,bi,bj))  + SEAICE_deltaMin),ONE )

C          Apply the maxiums on zeta and eta
           zeta(I,J,bi,bj) = MIN(zeta(I,J,bi,bj),zetamax_TD)
           eta(I,J,bi,bj)  = MIN( eta(I,J,bi,bj), etamax_TD)

C          Replacement pressure
           press(I,J,bi,bj) =
     &          ( press0(I,J,bi,bj)*( ONE - SEAICEpressReplFac )
     &          + zeta(I,J,bi,bj) * ep * SEAICEpressReplFac 
     &          / SIGN(ABS(xma) + SEAICE_deltaMin, xma ) )  
     &          * ( ONE - a )
          
C          Eta change for the replacement pressure
           eta(I,J,bi,bj) = eta(I,J,bi,bj) 
     &          * ( ( ONE - SEAICEpressReplFac ) 
     &          + SEAICEpressReplFac * press(I,J,bi,bi) 
                / ( ( ONE - a ) * press0(I,J,bi,bj) )
          
          ENDDO !i loop
         ENDDO !j loop
#endif /* SEAICE_ALLOW_TEARDROP */
#ifdef SEAICE_ALLOW_FULLMC
        ELSEIF ( SEAICEuseFULLMC ) THEN
CDR
CDR FULL MC following IP et al. 1991
CDR
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1

C     Computing e1 and e2
           ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)
           em = e11(i,j,bi,bj)-e22(i,j,bi,bj)
           shearDefSq=em*em+4. _d 0*e12Csq(i,j)

C     Computing ZETA
           zeta(I,J,bi,bj) = press0(I,J,bi,bj)*(ONE+tnsFac(I,J,bi,bj))
     &                      / (TWO * SQRT(ep*ep+deltaMinSq))

C     Replacement Pressure
           press(I,J,bi,bj) = ( ONE - tnsFac(I,J,bi,bj) )
     &      * (press0(I,J,bi,bj) * ( ONE - SEAICEpressReplFac )
     &      + SEAICEpressReplFac * TWO * zeta(I,J,bi,bj) * ABS(ep)
     &      / ( ONE + tnsFac(I,J,bi,bj) ) )

C     Computing ETA  (EMAX=ZMAX)
           eta(I,J,bi,bj) = SEAICEmcMU * (0.5 _d 0 * press(I,J,bi,bj)
     &      - zeta(I,J,bi,bj)*ep+press0(I,J,bi,bj)*tnsFac(I,J,bi,bj))
     &      / (SQRT(shearDefSq + deltaMinSq))

          ENDDO
         ENDDO
#endif /* SEAICE_ALLOW_FULLMC */
        ELSE
C     For all elliptic yield curves, computation of deltaC and zeta
C     identical so we do it first
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)
           em = e11(i,j,bi,bj)-e22(i,j,bi,bj)
           shearDefSq = em*em + 4. _d 0*e12Csq(i,j)
           deltaCsq   = ep*ep + recip_efr4*shearDefSq
CML The old formulation does not ensure that deltaC**2 is always positive,
CML but in case you need it to reproduce old results, here it is:
CML          deltaCsq =
CML     &         (e11(i,j,bi,bj)**2+e22(i,j,bi,bj)**2)*(ONE+recip_e2)
CML     &         + 4. _d 0*recip_e2*e12Csq(i,j)
CML     &         + 2. _d 0*e11(i,j,bi,bj)*e22(i,j,bi,bj)*(ONE-recip_e2)
#ifdef ALLOW_AUTODIFF_TAMC
C     avoid sqrt of 0
           deltaC(I,J,bi,bj) = 0. _d 0
           IF ( deltaCsq .GT. 0. _d 0 )
     &          deltaC(I,J,bi,bj) = SQRT(deltaCsq)
#else
           deltaC(I,J,bi,bj) = SQRT(deltaCsq)
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef SEAICE_DELTA_SMOOTHREG
C     smooth regularization (without max-function) of delta for
C     better differentiability
           deltaCreg(I,J) = SQRT(deltaCsq + deltaMinSq)
CML          deltaCreg(I,J) = deltaC(I,J,bi,bj) + SEAICE_deltaMin
#else
           deltaCreg(I,J) = MAX(deltaC(I,J,bi,bj),SEAICE_deltaMin)
#endif /* SEAICE_DELTA_SMOOTHREG */
#ifdef SEAICE_ZETA_SMOOTHREG
C     regularize zeta to zmax with a smooth tanh-function instead
C     of a min(zeta,zmax). This improves convergence of iterative
C     solvers (Lemieux and Tremblay 2009, JGR). No effect on EVP
           argTmp = exp(-1. _d 0/(deltaCreg(I,J)*SEAICE_zetaMaxFac))
           zeta (I,J,bi,bj) = ZMAX(I,J,bi,bj)
     &          *(1. _d 0 - argTmp)/(1. _d 0 + argTmp)
     &          *(1. _d 0 + tnsFac(I,J,bi,bj) )
#else
           zeta (I,J,bi,bj) = HALF*( press0(I,J,bi,bj)
     &          * ( 1. _d 0 + tnsFac(I,J,bi,bj) )
     &          )/deltaCreg(I,J)
C     put min and max viscosities in
           zeta (I,J,bi,bj) = MIN(ZMAX(I,J,bi,bj),zeta(I,J,bi,bj))
#endif /*  SEAICE_ZETA_SMOOTHREG */
           zeta (I,J,bi,bj) = MAX(ZMIN(I,J,bi,bj),zeta(I,J,bi,bj))
C     set viscosities to zero at HEFFM flow pts
           zeta (I,J,bi,bj) = zeta(I,J,bi,bj)*HEFFM(I,J,bi,bj)
C     "replacement pressure"
           press(I,J,bi,bj) =
     &          ( press0(I,J,bi,bj)*( 1. _d 0 - SEAICEpressReplFac )
     &          + TWO*zeta(I,J,bi,bj)*deltaC(I,J,bi,bj)
     &          * SEAICEpressReplFac/( 1. _d 0 + tnsFac(I,J,bi,bj) )
     &          ) * ( 1. _d 0 - tnsFac(I,J,bi,bj) )
CML          press(I,J,bi,bj) = press0(I,J,bi,bj) *
CML     &         ( 1. _d 0 + SEAICEpressReplFac
CML     &                  * ( deltaC(I,J,bi,bj)/deltaCreg(I,J) - 1. _d 0 )
CML     &         ) * ( 1. _d 0 - tnsFac(I,J,bi,bj) )
          ENDDO
         ENDDO
C     The elliptical yield curves differ in the way eta is computed
         IF (.FALSE.) THEN
#if (defined SEAICE_ALLOW_MCE) || (defined SEAICE_ALLOW_TEM)
         ELSEIF ( SEAICEuseMCE .OR. SEAICEuseTEM ) THEN
C     MC yield curve with flow rule from the elliptical yield curve
C     or Truncated Ellipse method with Coulombic limbs
          DO j=1-OLy+1,sNy+OLy-1
           DO i=1-OLx+1,sNx+OLx-1
C     Computing e1 and e2
            ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)
            em = e11(i,j,bi,bj)-e22(i,j,bi,bj)
            shearDefSq = em*em + 4. _d 0*e12Csq(i,j)
#ifdef SEAICE_DELTA_SMOOTHREG
            shearDef   = SQRT(deltaMinSq + shearDefSq)
#else
            shearDef   = SQRT(MAX(deltaMinSq,shearDefSq))
#endif /* SEAICE_DELTA_SMOOTHREG */
C     Computing ETA
C     In principle, we need to recompute zeta here, because we need the
C     unlimited version
C          zetaLoc = HALF*( press0(I,J,bi,bj)
C    &          * ( 1. _d 0 + tnsFac(I,J,bi,bj) )
C    &          )/deltaCreg(I,J)
C           eta(I,J,bi,bj) = SEAICEmcMU * (0.5 * press0(I,J,bi,bj)
C    &           * ( 1. _d 0 - tnsFac(I,J,bi,bj) ) - zetaLoc * ep
C    &           + press0(I,J,bi,bj)*tnsFac(I,J,bi,bj)) / shearDef
C     but in this formulation, only ep/deltaCreg(I,J) remains
            eta(I,J,bi,bj) = SEAICEmcMU * HALF * press0(I,J,bi,bj)
     &           * ( 1. _d 0 + tnsFac(I,J,bi,bj) )
     &           * ( 1. _d 0 - ep/deltaCreg(I,J) ) / shearDef
C     Computing etaMaxFac = ZMAX/zetaLoc
CDR            etaMaxFac = SEAICE_zetaMaxFac * 2. _d 0 * deltaCreg(I,J)
CDR     &           / ( 1. _d 0 + tnsFac(I,J,bi,bj) )
C     Applying maximum
CDR            eta(I,J,bi,bj) = eta(I,J,bi,bj) * MIN( 1. _d 0, etaMaxFac )
            eta(I,J,bi,bj)=MIN(eta(I,J,bi,bj)
     &                    , ZMAX(I,J,bi,bj)*recip_efr2)
           ENDDO
          ENDDO
C     closing the coulombic limbs with the ellipse for compression
          IF ( SEAICEuseTEM) THEN
           DO j=1-OLy+1,sNy+OLy-1
            DO i=1-OLx+1,sNx+OLx-1
             eta(I,J,bi,bj) = MIN( eta(I,J,bi,bj),
     &            recip_efr2*zeta(I,J,bi,bj) )
            ENDDO
           ENDDO
          ENDIF
#endif /* SEAICE_ALLOW_MCE or SEAICE_ALLOW_TEM */
         ELSE
C     default elliptical yield curve
          DO j=1-OLy+1,sNy+OLy-1
           DO i=1-OLx+1,sNx+OLx-1
            eta(I,J,bi,bj) = recip_efr2*zeta(I,J,bi,bj)
           ENDDO
          ENDDO
C     end of elliptical yield curve options
         ENDIF
C     end of yield curve options
        ENDIF

C     compute eta at Z-points by simple averaging
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          sumNorm  = HEFFM(I,J,  bi,bj)+HEFFM(I-1,J,  bi,bj)
     &             + HEFFM(I,J-1,bi,bj)+HEFFM(I-1,J-1,bi,bj)
          IF ( sumNorm.GT.0. _d 0 ) sumNorm = 1. _d 0 / sumNorm
          etaZ(I,J,bi,bj) = sumNorm *
     &         ( eta (I,J  ,bi,bj)  + eta (I-1,J  ,bi,bj)
     &         + eta (I,J-1,bi,bj)  + eta (I-1,J-1,bi,bj) )
          zetaZ(I,J,bi,bj) = sumNorm *
     &         ( zeta(I,J  ,bi,bj)  + zeta(I-1,J  ,bi,bj)
     &         + zeta(I,J-1,bi,bj)  + zeta(I-1,J-1,bi,bj) )
         ENDDO
        ENDDO
C     free-slip means no lateral stress, which is best achieved by masking
C     eta on vorticity(=Z)-points; from now on we only need to worry
C     about the no-slip boundary conditions
        IF (.NOT.SEAICE_no_slip) THEN
         DO J=1-OLy+1,sNy+OLy-1
          DO I=1-OLx+1,sNx+OLx-1
           maskZ = HEFFM(I,J,  bi,bj)*HEFFM(I-1,J,  bi,bj)
     &          *  HEFFM(I,J-1,bi,bj)*HEFFM(I-1,J-1,bi,bj)
           etaZ (I,J,bi,bj) =  etaZ(I,J,bi,bj) * maskZ
           zetaZ(I,J,bi,bj) = zetaZ(I,J,bi,bj) * maskZ
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID */
      RETURN
      END
