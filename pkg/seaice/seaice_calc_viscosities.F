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
C     | started by Martin Losch, Mar 2006                        |
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
      _RL recip_e2, deltaCsq, deltaMinSq, tmp, ep, em, smallNbr
      _RL recip_efr4, recip_efr2, oneThird, oneNinth, smallNbrSq
      _RL e12Csq     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltaCreg  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_shear(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shearDefSq, shearDef
#ifdef SEAICE_ALLOW_TEARDROP
      _RL k,ksq,x,a,ap1,etamax_TD,zetamax_TD,cyc,xma, etapr
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
      smallNbr = 1. _d -20
      smallNbrSq = smallNbr * smallNbr

      recip_e2=0. _d 0
      IF ( SEAICE_eccen .NE. 0. _d 0 ) recip_e2=ONE/(SEAICE_eccen**2)
      recip_efr2=0. _d 0
      recip_efr4=0. _d 0
      IF( SEAICE_eccfr .NE. 0. _d 0) THEN
       recip_efr2=ONE/(SEAICE_eccfr**2)
       recip_efr4=(SEAICE_eccen**2) / (SEAICE_eccfr**4)
      ENDIF
      deltaMinSq = SEAICE_deltaMin**2
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          deltaCreg  (i,j) = SEAICE_deltaMin
          e12Csq     (i,j) = 0. _d 0
          recip_shear(i,j) = 0. _d 0
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */
C     need to do this beforehand for easier vectorization after
C     TAFization
        IF ( SEAICEetaZmethod .EQ. 0 ) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           tmp = 0.25 *
     &          ( e12(i,j  ,bi,bj) + e12(i+1,j  ,bi,bj)
     &          + e12(i,j+1,bi,bj) + e12(i+1,j+1,bi,bj) )
           e12Csq(i,j) = tmp*tmp
          ENDDO
         ENDDO
        ELSEIF ( SEAICEetaZmethod .EQ. 3 ) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
C     area weighted average of the squares of e12 is more accurate
C     (and energy conserving)
           e12Csq(i,j) = 0.25 _d 0 * recip_rA(i,j,bi,bj) *
     &          ( rAz(i  ,j  ,bi,bj)*e12(i  ,j  ,bi,bj)**2
     &          + rAz(i+1,j  ,bi,bj)*e12(i+1,j  ,bi,bj)**2
     &          + rAz(i  ,j+1,bi,bj)*e12(i  ,j+1,bi,bj)**2
     &          + rAz(i+1,j+1,bi,bj)*e12(i+1,j+1,bi,bj)**2 )
          ENDDO
         ENDDO
        ENDIF
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          em = e11(i,j,bi,bj)-e22(i,j,bi,bj)
          shearDefSq = em*em + 4. _d 0 * e12Csq(i,j)
#ifdef SEAICE_DELTA_SMOOTHREG
          recip_shear(i,j) = 1. _d 0
     &                / SQRT( shearDefSq + smallNbrSq )
C         recip_shear(i,j) = 1. _d 0 / ( shearDef + smallNbr )
#else
# ifdef ALLOW_AUTODIFF_TAMC
C     avoid sqrt of 0
          shearDef = 0. _d 0
          IF ( shearDefSq .GT. 0. _d 0 ) shearDef = SQRT(shearDefSq)
# else
          shearDef = SQRT(shearDefSq)
# endif /* ALLOW_AUTODIFF_TAMC */
          recip_shear(i,j) = 1. _d 0 / MAX( ShearDef, smallNbr )
#endif /* SEAICE_DELTA_SMOOTHREG */
         ENDDO
        ENDDO

C     select between different yield curves; default: elliptical yield curve
        IF ( .FALSE. ) THEN
C     do nothing (hack to be able add code with different cpp-flag)
#ifdef SEAICE_ALLOW_TEARDROP
        ELSEIF ( SEAICEuseTD ) THEN

         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)
           em = e11(i,j,bi,bj)-e22(i,j,bi,bj)
C            shearDef = SQRT(em*em + 4. _d 0 * e12Csq(i,j))

           k   = ep * recip_shear(i,j)
           ksq = k * k
           a   = tnsFac(i,j,bi,bj)

C     cyc: coordinate of the maximum point of the yield curve,
C     where eI changes sign
           cyc=( TWO - tnsFac(i,j,bi,bj) ) * oneThird
           ap1 = a + ONE

C     handle floating point errors when k is negative and very large
           IF ( k .LT. -1. _d 3 ) THEN
            x = (3. _d 0 * ap1)
     &        * ( - 1. _d 0 + 0.125 _d 0 *(3. _d 0 * ap1 ) / ksq )
           ELSE
            x = TWO * ksq + TWO *k*SQRT(ksq + 3. _d 0 * ap1 )
           ENDIF
           x = (x - (6. _d 0 * ap1) ) * oneNinth + a
C     original formulation of Zhang 2005:
C          x = ( TWO * ksq + TWO *k*sqrt(ksq + 3. _d 0 * ap1 )
C      &          - (6. _d 0 * ap1) ) * oneNinth + a

C     Teardrop yield curve:
C     capping x at a is probably better that setting it to a for x>1,
C     but still not ideal (because non-differentiable)
C     possible alternative: log(exp(rho*x)+exp(rho*a))/rho, with rho=O(100)
           x = MIN( x, a )
C          x = MAX( x,-1 _d 0 ) ! to be sure

#ifdef SEAICE_DELTA_SMOOTHREG
           zeta(i,j,bi,bj) = (x + cyc) * press0(i,j,bi,bj)
     &          / SIGN( SQRT(ep*ep+smallNbrSq), ep)
#else
           zeta(i,j,bi,bj) = (x + cyc) * press0(i,j,bi,bj)
     &          / SIGN( MAX( ABS(ep),smallNbr ), ep)
#endif /* SEAICE_DELTA_SMOOTHREG */
C      original formulation of Zhang 2005
C          zeta(i,j,bi,bj) = (x + 0.5 _d 0) * press0(i,j,bi,bj)
C      &        / SIGN( ABS(ep) + SEAICE_deltaMin, ep)
C      &        / SIGN( MAX( ABS(ep),deltaMinSq ), 0.5 _d 0 * ep)

           eta(i,j,bi,bj) = -(x-a) * SQRT(ONE+x) * press0(i,j,bi,bj)
     &                     * recip_shear(i,j)

C      define the max values for eta and zeta
           etamax_TD = ZMAX(i,j,bi,bj) * MIN(eta(i,j,bi,bj)
     &               /(ABS(zeta(i,j,bi,bj)) + smallNbr), ONE)
           zetamax_TD =ZMAX(i,j,bi,bj) * MIN(zeta(i,j,bi,bj)
     &               /(ABS(eta(i,j,bi,bj)) + smallNbr), ONE )

C      apply the max values
           zeta(i,j,bi,bj) = MIN(zeta(i,j,bi,bj), zetamax_TD)
           eta (i,j,bi,bj) = MIN( eta(i,j,bi,bj),  etamax_TD)
C      original formulation of Zhang 2005
C          zeta(i,j,bi,bj) = MIN(zeta(i,j,bi,bj), ZMAX(i,j,bi,bj))
C          eta (i,j,bi,bj) = MIN( eta(i,j,bi,bj), ZMAX(i,j,bi,bj))

C      compute the replacement pressure
           press(i,j,bi,bj) = TWO * cyc * (
     &          press0(i,j,bi,bj)*( ONE - SEAICEpressReplFac )
     &         + SEAICEpressReplFac * ep * zeta(i,j,bi,bj)
     &         / SIGN(MAX(ABS(x+cyc), smallNbr), x+cyc) )

C      reduce eta according to replacement pressure
           etapr = -(x-a) * SQRT(ONE + x) * press(i,j,bi,bj)/(ONE-a)
     &                    * recip_shear(i,j)
           etapr = MIN( etapr, etamax_TD )
           eta(i,j,bi,bj) =  SEAICEpressReplFac * etapr
     &          + eta(i,j,bi,bj) * ( ONE - SEAICEpressReplFac )

          ENDDO ! x loop
         ENDDO ! y loop

        ELSEIF ( SEAICEusePL ) THEN

         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1

           ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)
           k = ep * recip_shear(i,j)
           a = tnsFac(i,j,bi,bj)
           x = 0.5 _d 0 * (k - 1. _d 0 + a)
           x = MAX( x, -1. _d 0 )
           x = MIN( x, a )
           xma = x + 0.5 _d 0 *(1. _d 0 - a )
C     compute zeta
#ifdef SEAICE_DELTA_SMOOTHREG
           zeta(i,j,bi,bj) = xma * press0(i,j,bi,bj)
     &                    / SIGN( SQRT(ep*ep + smallNbrSq) , ep)
#else
           zeta(i,j,bi,bj) = xma * press0(i,j,bi,bj)
     &                    / SIGN( MAX(ABS(ep),smallNbr) , ep)
#endif /* SEAICE_DELTA_SMOOTHREG */
C      original formulation of Zhang 2005
C          zeta(i,j,bi,bj) = (x + 0.5 _d 0) * press0(i,j,bi,bj)
C      &        / SIGN( ABS(ep) + SEAICE_deltaMin, ep)
C      &        / SIGN( MAX( ABS(ep),deltaMinSq ), 0.5 _d 0 * ep)

C     compute eta
           eta(i,j,bi,bj) = -(x-a) * (ONE + x) * press0(i,j,bi,bj)
     &                    * recip_shear(i,j)

C     maximum of eta and zeta
           etamax_TD = ZMAX(i,j,bi,bj) * MIN(eta(i,j,bi,bj)
     &               /(ABS(zeta(i,j,bi,bj)) + smallNbr),ONE )
           zetamax_TD =ZMAX(i,j,bi,bj) * MIN(zeta(i,j,bi,bj)
     &               /(ABS(eta(i,j,bi,bj))  + smallNbr),ONE )

C     apply the maxiums on zeta and eta
           zeta(i,j,bi,bj) = MIN(zeta(i,j,bi,bj),zetamax_TD)
           eta (i,j,bi,bj) = MIN( eta(i,j,bi,bj), etamax_TD)

C     replacement pressure
           press(i,j,bi,bj) =
     &          ( press0(i,j,bi,bj)*( ONE - SEAICEpressReplFac )
     &          + zeta(i,j,bi,bj) * ep * SEAICEpressReplFac
     &          / SIGN(ABS(xma) + smallNbr, xma ) )
     &          * ( ONE - a )

C     change eta according to replacement pressure
           etapr = -(x-a) * (ONE + x) * press(i,j,bi,bj)/(ONE-a)
     &                    * recip_shear(i,j)
           etapr = MIN( etapr, etamax_TD )
           eta(i,j,bi,bj) =  SEAICEpressReplFac * etapr
     &          + eta(i,j,bi,bj) * ( ONE - SEAICEpressReplFac )
          ENDDO !i loop
         ENDDO !j loop
#endif /* SEAICE_ALLOW_TEARDROP */
#ifdef SEAICE_ALLOW_MCS
        ELSEIF ( SEAICEuseMCS ) THEN
C     Full Mohr-Coulomb following IP et al. 1991
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1

C     compute eI = ep
           ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)

C     compute zeta
#ifdef SEAICE_DELTA_SMOOTHREG
           zeta(i,j,bi,bj) = press0(i,j,bi,bj)*(ONE+tnsFac(i,j,bi,bj))
     &                      / (TWO * SQRT(ep*ep+deltaMinSq) )
#else
           zeta(i,j,bi,bj) = press0(i,j,bi,bj)*(ONE+tnsFac(i,j,bi,bj))
     &                      / (TWO * MAX(ABS(ep),SEAICE_deltaMin) )
#endif /* SEAICE_DELTA_SMOOTHREG */

C     replacement pressure
           press(i,j,bi,bj) = ( ONE - tnsFac(i,j,bi,bj) )
     &      * ( press0(i,j,bi,bj) * ( ONE - SEAICEpressReplFac )
     &      + SEAICEpressReplFac * TWO * zeta(i,j,bi,bj) * ABS(ep)
     &      / ( ONE + tnsFac(i,j,bi,bj) ) )

C     compute eta  (eMax=ZMAX)
           eta(i,j,bi,bj) = SEAICEmcMU * (0.5 _d 0 * press(i,j,bi,bj)
     &      - zeta(i,j,bi,bj)*ep+press0(i,j,bi,bj)*tnsFac(i,j,bi,bj))
     &       * recip_shear(i,j)

C     maximum for eta (high compressive stresses)
           eta(i,j,bi,bj) = MIN(eta(i,j,bi,bj) , ZMAX(i,j,bi,bj))

          ENDDO
         ENDDO
#endif /* SEAICE_ALLOW_MCS */
        ELSE
C     For all elliptic yield curves, the computation of deltaC and zeta
C     is identical so we do it first.
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
           deltaC(i,j,bi,bj) = 0. _d 0
           IF ( deltaCsq .GT. 0. _d 0 )
     &          deltaC(i,j,bi,bj) = SQRT(deltaCsq)
#else
           deltaC(i,j,bi,bj) = SQRT(deltaCsq)
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef SEAICE_DELTA_SMOOTHREG
C     smooth regularization (without max-function) of delta for
C     better differentiability
           deltaCreg(i,j) = SQRT(deltaCsq + deltaMinSq)
CML          deltaCreg(i,j) = deltaC(i,j,bi,bj) + SEAICE_deltaMin
#else
           deltaCreg(i,j) = MAX(deltaC(i,j,bi,bj),SEAICE_deltaMin)
#endif /* SEAICE_DELTA_SMOOTHREG */
#ifdef SEAICE_ZETA_SMOOTHREG
C     regularize zeta to zmax with a smooth tanh-function instead
C     of a min(zeta,zmax). This improves convergence of iterative
C     solvers (Lemieux and Tremblay 2009, JGR). No effect on EVP
           argTmp = exp(-1. _d 0/(deltaCreg(i,j)*SEAICE_zetaMaxFac))
           zeta (i,j,bi,bj) = ZMAX(i,j,bi,bj)
     &          *(1. _d 0 - argTmp)/(1. _d 0 + argTmp)
     &          *(1. _d 0 + tnsFac(i,j,bi,bj) )
#else
           zeta (i,j,bi,bj) = HALF*( press0(i,j,bi,bj)
     &          * ( 1. _d 0 + tnsFac(i,j,bi,bj) )
     &          )/deltaCreg(i,j)
C     put min and max viscosities in
           zeta (i,j,bi,bj) = MIN(ZMAX(i,j,bi,bj),zeta(i,j,bi,bj))
#endif /*  SEAICE_ZETA_SMOOTHREG */
           zeta (i,j,bi,bj) = MAX(ZMIN(i,j,bi,bj),zeta(i,j,bi,bj))
C     set viscosities to zero at HEFFM flow pts
           zeta (i,j,bi,bj) = zeta(i,j,bi,bj)*HEFFM(i,j,bi,bj)
C     "replacement pressure"
           press(i,j,bi,bj) =
     &          ( press0(i,j,bi,bj)*( 1. _d 0 - SEAICEpressReplFac )
     &          + TWO*zeta(i,j,bi,bj)*deltaC(i,j,bi,bj)
     &          * SEAICEpressReplFac/( 1. _d 0 + tnsFac(i,j,bi,bj) )
     &          ) * ( 1. _d 0 - tnsFac(i,j,bi,bj) )
CML          press(i,j,bi,bj) = press0(i,j,bi,bj) *
CML     &         ( 1. _d 0 + SEAICEpressReplFac
CML     &                  * ( deltaC(i,j,bi,bj)/deltaCreg(i,j) - 1. _d 0 )
CML     &         ) * ( 1. _d 0 - tnsFac(i,j,bi,bj) )
          ENDDO
         ENDDO
C     The elliptical yield curves differ in the way eta is computed.
#if (defined SEAICE_ALLOW_MCE) || (defined SEAICE_ALLOW_TEM)
         IF ( SEAICEuseMCE .OR. SEAICEuseTEM ) THEN
C     MC yield curve with non-normal flow rule (elliptical plastic potential)
C     or Truncated Ellipse method with Coulombic limbs
          DO j=1-OLy+1,sNy+OLy-1
           DO i=1-OLx+1,sNx+OLx-1
            ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)
C     compute eta:
C     In principle, we need to recompute zeta here, because we need the
C     unlimited version
C          zetaLoc = HALF*( press0(i,j,bi,bj)
C    &          * ( 1. _d 0 + tnsFac(i,j,bi,bj) )
C    &          )/deltaCreg(i,j)
C           eta(i,j,bi,bj) = SEAICEmcMU * (0.5 * press0(i,j,bi,bj)
C    &           * ( 1. _d 0 - tnsFac(i,j,bi,bj) ) - zetaLoc * ep
C    &           + press0(i,j,bi,bj)*tnsFac(i,j,bi,bj)) / shearDef
C     but in this formulation, only ep/deltaCreg(i,j) remains
            eta(i,j,bi,bj) = SEAICEmcMU * HALF * press0(i,j,bi,bj)
     &           * ( 1. _d 0 + tnsFac(i,j,bi,bj) )
     &           * ( 1. _d 0 - ep/deltaCreg(i,j) ) * recip_shear(i,j)
C     compute etaMaxFac = ZMAX/zetaLoc
            etaMaxFac = SEAICE_zetaMaxFac * 2. _d 0 * deltaCreg(i,j)
     &           / ( 1. _d 0 + tnsFac(i,j,bi,bj) )
C     apply maximum from Mohr-Coulomb limbs
            eta(i,j,bi,bj) = eta(i,j,bi,bj) * MIN( 1. _d 0, etaMaxFac )
C     apply maximum from Elliptical formulation (to be sure)
            eta(i,j,bi,bj) =
     &           MIN( eta(i,j,bi,bj), ZMAX(i,j,bi,bj)*recip_efr2 )
           ENDDO
          ENDDO
C     close the coulombic limbs with the ellipse for compression
          IF ( SEAICEuseTEM) THEN
           DO j=1-OLy+1,sNy+OLy-1
            DO i=1-OLx+1,sNx+OLx-1
             eta(i,j,bi,bj) = MIN( eta(i,j,bi,bj),
     &                             zeta(i,j,bi,bj) * recip_efr2 )
            ENDDO
           ENDDO
          ENDIF
         ELSE
#endif /* SEAICE_ALLOW_MCE or SEAICE_ALLOW_TEM */
C     default elliptical yield curve
          DO j=1-OLy+1,sNy+OLy-1
           DO i=1-OLx+1,sNx+OLx-1
            eta(i,j,bi,bj) = zeta(i,j,bi,bj) * recip_efr2
           ENDDO
          ENDDO
#if (defined SEAICE_ALLOW_MCE) || (defined SEAICE_ALLOW_TEM)
C     end of elliptical yield curve options
         ENDIF
#endif /* SEAICE_ALLOW_MCE or SEAICE_ALLOW_TEM */
C     end of yield curve options
        ENDIF

C     compute eta at Z-points by simple averaging
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          sumNorm  = HEFFM(i,j,  bi,bj)+HEFFM(i-1,j,  bi,bj)
     &             + HEFFM(i,j-1,bi,bj)+HEFFM(i-1,j-1,bi,bj)
          IF ( sumNorm.GT.0. _d 0 ) sumNorm = 1. _d 0 / sumNorm
          etaZ(i,j,bi,bj) = sumNorm *
     &         ( eta (i,j  ,bi,bj)  + eta (i-1,j  ,bi,bj)
     &         + eta (i,j-1,bi,bj)  + eta (i-1,j-1,bi,bj) )
          zetaZ(i,j,bi,bj) = sumNorm *
     &         ( zeta(i,j  ,bi,bj)  + zeta(i-1,j  ,bi,bj)
     &         + zeta(i,j-1,bi,bj)  + zeta(i-1,j-1,bi,bj) )
         ENDDO
        ENDDO
C     free-slip means no lateral stress, which is best achieved by masking
C     eta on vorticity(=Z)-points; from now on we only need to worry
C     about the no-slip boundary conditions
        IF (.NOT.SEAICE_no_slip) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           maskZ = HEFFM(i,j,  bi,bj)*HEFFM(i-1,j,  bi,bj)
     &          *  HEFFM(i,j-1,bi,bj)*HEFFM(i-1,j-1,bi,bj)
           etaZ (i,j,bi,bj) =  etaZ(i,j,bi,bj) * maskZ
           zetaZ(i,j,bi,bj) = zetaZ(i,j,bi,bj) * maskZ
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID */
      RETURN
      END
