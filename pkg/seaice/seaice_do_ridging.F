C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_do_ridging.F,v 1.1 2014/04/23 12:38:40 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C !ROUTINE: SEAICE_DO_RIDGING
C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_DO_RIDGING(
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_DO_RIDGING
C     | o compute mechanical redistribution of thin (undeformed) into
C     |   thick (deformed, i.e. ridged) ice categories
C     |   according to Thorndyke et al (1975) and Hibler (1980)
C     |   or Bitz et al (2001) and Lipscomb et al (2007)
C     |
C     | Martin Losch, Apr. 2014, Martin.Losch@awi.de
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     bi, bj    :: outer loop counters
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      _RL myTime
      INTEGER bi,bj
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,k       :: inner loop counters
C     openWater   :: open water area fraction
C
      INTEGER i, j
      INTEGER iMin, iMax, jMin, jMax
#ifdef SEAICE_ITD
      INTEGER k, l, n
      _RL FW2ObyRidge     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ridgingModeNorm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL partFunc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,0:nITD)
C     ridgeRatio :: mean ridge thickness/ thickness of ridging ice
C     hrMin      :: min ridge thickness
C     hrMax      :: max ridge thickness   (SEAICEredistFunc = 0)
C     hrExp      :: ridge e-folding scale (SEAICEredistFunc = 1)
      _RL hrMin           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL hrMax           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL hrExp           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL ridgeRatio      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
C
      _RL openWater       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL netArea         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     variables related to ridging schemes
      _RL openingRate     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL closingRate     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL grossClosing    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     amount of ice that participates in ridging (according to partFunc)
      _RL ridgingArea     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ridgingHeff     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ridgingHsnw     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     fractions of deformed/ridged ice
      _RL areaFraction    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL volFraction     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL areaScaling     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     absolute area/concentration of deformed/ridged ice
      _RL ridgedArea      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL doRidging   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL doRidgeAgain, areaTooLarge
C
      _RL recip_deltaT, convergence, divergence, shear, divAdv
      _RL ridgingIce
      _RL tmp, tmpFac, hL, hR, expL, expR
      _RL areaPR          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD) 
#endif /* SEAICE_ITD */
CEOP


C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      iMin=1
      iMax=sNx
      jMin=1
      jMax=sNy
#ifndef SEAICE_ITD 
C     Hiblers "ridging function" for single category ice
      DO j=jMin,jMax
       DO i=iMin,iMax
        AREA(I,J,bi,bj) = MIN(AREA(I,J,bi,bj),SEAICE_area_max)
       ENDDO
      ENDDO
#else
C     calculate area of open water
      DO j=jMin,jMax
       DO i=iMin,iMax
        openWater(i,j) = ONE
       ENDDO
      ENDDO
      DO k=1,nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         openWater(i,j) = openWater(i,j) - AREAITD(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      IF ( SEAICEsimpleRidging ) THEN
C--   Hibler-type "ridging", i.e. cut back excessive ice area fraction ---
C     in case ice concentration exceeds 100% assume that
C     convergence of floe field has eliminated all open water
C     and eventual rafting occured in thinnest category:      
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF (openWater(i,j) .lt. 0.0)
     &        AREAITD(i,j,1,bi,bj) = openWater(i,j)+AREAITD(i,j,1,bi,bj)
        ENDDO
       ENDDO
C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
       CALL SEAICE_PREPARE_RIDGING(
     O      hrMin, hrMax, hrExp, ridgeRatio, ridgingModeNorm, partFunc,
     I      iMin, iMax, jMin, jMax, bi, bj, myTime, myIter, myThid )
C     Compute the first strain rate invariant epsilonI (divergence)
C     energy dissipation by convergence = -min (divergence, 0)
C     energy dissipation by shearing    = (1/2) * (Delta - abs(divergence))
      DO j=jMin,jMax
       DO i=iMin,iMax
        divergence  = e11(i,j,bi,bj) + e22(i,j,bi,bj)
        shear       = 0.5 _d 0 * ( deltaC(i,j,bi,bj) - ABS(divergence) )
        convergence = - MIN(divergence, 0.D0)
        closingRate(i,j) = SEAICEshearParm*shear + convergence
       ENDDO
      ENDDO
C     we need a new estimate of the total AREA (including the open water 
C     fraction, but for computational reason it is not included here)
      DO j=jMin,jMax
       DO i=iMin,iMax
        netArea(i,j) = 0. _d 0
       ENDDO
      ENDDO
      DO k=1,nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         netArea(i,j) = netArea(i,j) + AREAITD(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      recip_DeltaT = 1. _d 0/SEAICE_deltaTtherm
      DO j=jMin,jMax
       DO i=iMin,iMax
C     divergence rate due to advection; this term need not be zero due 
C     to numerical effects 
C     (this is copied from CICE but I am not sure about that)
        divAdv = (1. _d 0-netArea(i,j)-opnWtrFrac(i,j,bi,bj))
     &       *recip_deltaT
        IF (divAdv .LT. 0. _d 0) 
     &       closingRate(i,j) = MAX(closingRate(i,j), -divAdv)
        openingRate(i,j) = closingRate(i,j) + divAdv
       ENDDO
      ENDDO
C
C     start of the ridging loop
C
      doRidgeAgain = .TRUE.
      n = 1
      DO WHILE (doRidgeAgain)
C     save pre-ridging ice concentration and ridged ice volume
      DO k=1,nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         areaPR(i,j,k) = AREAITD(i,j,k,bi,bj)
C        ridgeFrac(i,j,k) = 0. _d 0 
C        IF (HEFFITD(i,j,k,bi,bj) .GT. 0. _d 0 )
C   &     ridgeFrac(i,j,k) = ridgedHeff/HEFF
        ENDDO
       ENDDO
      ENDDO
C     
      DO j=jMin,jMax
       DO i=iMin,iMax
C     Based on the ITD of ridging and ridged ice, convert the net
C     closing rate to a gross closing rate times deltaT.
C     NOTE: 0 < ridgingModeNorm <= 1
        grossClosing(i,j) = closingRate(i,j)*SEAICE_deltaTtherm
     &       /ridgingModeNorm(i,j)
C     reduce rates in case more than 100% of open water would be removed
        IF ( partFunc(i,j,0) .GT. 0. _d 0 ) THEN
         tmp = partFunc(i,j,0)*grossClosing(i,j)
         IF ( tmp .GT. opnWtrFrac(i,j,bi,bj) ) THEN
          tmpFac = opnWtrFrac(i,j,bi,bj)/tmp
          grossClosing(i,j) = grossClosing(i,j) * tmpFac
          openingRate(i,j)  =  openingRate(i,j) * tmpFac
         ENDIF
        ENDIF
       ENDDO
      ENDDO
      DO k=1,nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
C     reduce rates in case more than 100% of any ice categroy would be removed
         IF ( areaPR(i,j,k) > SEAICE_area_reg 
     &        .AND. partFunc(i,j,k) .GT. 0. _d 0 ) THEN
          tmp = partFunc(i,j,k)*grossClosing(i,j)
          IF ( tmp .GT. AREAITD(i,j,k,bi,bj) ) THEN
           tmpFac = AREAITD(i,j,k,bi,bj)/tmp
           grossClosing(i,j) = grossClosing(i,j) * tmpFac
           openingRate(i,j)  =  openingRate(i,j) * tmpFac
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
C     start redistribution
C       
      DO j=jMin,jMax
       DO i=iMin,iMax
C     open water first
        opnWtrFrac(i,j,bi,bj) = opnWtrFrac(i,j,bi,bj) 
     &       - partFunc(i,j,0)*grossClosing(i,j)
     &       + openingRate(i,j)*SEAICE_deltaTtherm
       ENDDO
      ENDDO
C     need to catch openWater < 0 properly
C     
C     After reducing the open water fraction new net area = opnWtrFrac 
C     + sum(AREAITD) may still be larger than one. If this is the case,
C     the ridged ice that is taken away from category k is still distributed
C     to categories l, but the distributed area should be reduced so that 
C     the net area after ridging equals one (or SEAICE_area_max):
C     opnWtrFrac+sum(AREAITD) - ridgingIce + ridgedIce/Norm = 1
C     since ridgingIce = ridgedIce, the latter does not need to be computed
C     and 
C     1/Norm = 1 - (opnWtrFrac+sum(AREAITD)-1)/ridgingIce
      DO j=jMin,jMax
       DO i=iMin,iMax
        areaScaling(i,j) = 1. _d 0
C     use divAdv again as a temporary variable now with a different sign
        divAdv = netArea(i,j) + opnWtrFrac(i,j,bi,bj) - 1. _d 0
        ridgingIce = grossClosing(i,j)*(1. _d 0 - partFunc(i,j,0))
        areaTooLarge = divAdv .GT. 0. _d 0 
     &       .AND. netArea(i,j) .GT. SEAICE_area_reg
     &       .AND. ridgingIce .GT. 0. _d 0
CML        IF ( SEAICE_area_max .LT. 1. _d 0 )
CML     &       areaTooLarge = areaTooLarge.OR.netArea(i,j).GT.SEAICE_area_max 
        IF (areaTooLarge) areaScaling(i,j) = 1. _d 0 - divAdv/ridgingIce
       ENDDO
      ENDDO
C     
      FW2ObyRidge(i,j,bi,bj) = 0. _d 0
      DO k=1,nITD
C     need to catch partFunc*grossClosing > AREAITD (or areaPR)
       DO j=jMin,jMax
        DO i=iMin,iMax
         doRidging(i,j) = areaPR(i,j,k) .GT. SEAICE_area_reg
     &        .AND. partFunc(i,j,k) .GT. 0. _d 0 
     &        .AND. grossClosing(i,j) .GT. 0. _d 0
     &        .AND. HEFFM(i,j,bi,bj) .GT. 0. _d 0
C     this would be safety catch only
C     &        .AND. netArea(i,j) .GT. 1. _d 0
         IF ( doRidging(i,j) ) THEN
CML          ridgingArea(i,j) = MIN(partFunc(i,j,k)*grossClosing(i,j),
CML     &         areaPR(i,j,k))
          ridgingArea(i,j) = partFunc(i,j,k)*grossClosing(i,j)
          IF ( ridgingArea(i,j)>areaPR(i,j,k) ) THEN
           ridgingArea(i,j) = areaPR(i,j,k)
          ENDIF
          areaFraction(i,j) = ridgingArea(i,j)/areaPR(i,j,k)
          ridgedArea(i,j)   = ridgingArea(i,j)/ridgeRatio(i,j,k)
C     compute ice volume (HEFF) and snow volume to be removed from this
C     ridging category
          ridgingHEFF(i,j) =  HEFFITD(i,j,k,bi,bj) * areaFraction(i,j)
          ridgingHsnw(i,j) = HSNOWITD(i,j,k,bi,bj) * areaFraction(i,j)
C     part of the snow mass is pushed into the ocean during ridging;
C     this freshwater flux will be added to the net feshwater flux into 
C     the ocean in seaice_growth
          FW2ObyRidge(i,j,bi,bj) = FW2ObyRidge(i,j,bi,bj) 
     &         + SEAICE_rhoSnow*ridgingHsnw(i,j)
     &         *(1. _d 0 - SEAICEsnowFracRidge)
C     reduce the snow volume that is left for redistribution
          ridgingHsnw(i,j) = ridgingHsnw(i,j) * SEAICEsnowFracRidge
C     remove ice concentration, volume (HEFF), and snow volume from
C     this ridging category
          AREAITD(i,j,k,bi,bj) = AREAITD(i,j,k,bi,bj) - ridgingArea(i,j)
          HEFFITD(i,j,k,bi,bj) = HEFFITD(i,j,k,bi,bj) - ridgingHeff(i,j)
          HSNOWITD(i,j,k,bi,bj)=HSNOWITD(i,j,k,bi,bj) - ridgingHsnw(i,j)
         ENDIF
        ENDDO
       ENDDO
C     inner loop over categories: distribute what has been removed from the 
C     kth category to all categories according to area/volFraction
       areaTest=0. _d 0
       volTest =0. _d 0
       DO l=1,nITD
        DO j=jMin,jMax
         DO i=iMin,iMax
          areaFraction(i,j) = 0. _d 0
          volFraction (i,j) = 0. _d 0
         ENDDO
        ENDDO
        IF ( SEAICEredistFunc .EQ. 0 ) THEN
C     Assume ridged ice is uniformly distributed between hrmin and hrmax
C     (Hibler, 1980), see also s/r seaice_prepare_ridging.
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( doRidging(i,j) ) THEN
            IF ( hrMin(i,j,k) .GE. hLimit(l) .OR.
     &           hrMax(i,j,k) .LE. hLimit(l-1) ) THEN
CML             hL = 0. _d 0
CML             hR = 0. _d 0
             areaFraction(i,j) = 0. _d 0
             volFraction (i,j) = 0. _d 0
            ELSE
             hL = MAX(hrMin(i,j,k), hLimit(l-1))
             hR = MIN(hrMax(i,j,k), hLimit(l))
             areaFraction(i,j) = ( hR - hL ) 
     &            / ( hrMax(i,j,k) - hrMin(i,j,k) )
CML             volFraction (i,j) = ( hR*hR - hL*hL ) 
CML     &            / ( hrMax(i,j,k)**2 - hrMin(i,j,k)**2 )
             volFraction (i,j) = areaFraction(i,j)*( hR + hL ) 
     &            / ( hrMax(i,j,k) + hrMin(i,j,k) )
            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ELSEIF ( SEAICEredistFunc .EQ. 1 ) THEN
C     Follow Lipscomb et al. (2007) and model ridge ITD as an exponentially
C     decaying function, see also s/r seaice_prepare_ridging.
         IF ( l.LT.nITD ) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
            IF ( doRidging(i,j) 
     &           .AND. hrMin(i,j,k) .LT. hLimit(l) 
     &           .AND. hrExp(i,j,k) .NE. 0. _d 0 ) THEN
             hL   = MAX( hrMin(i,j,k), hLimit(l-1) )
             hR   = hLimit(l)
             expL = EXP(-( hL - hrMin(i,j,k) )/hrExp(i,j,k) )
             expR = EXP(-( hR - hrMin(i,j,k) )/hrExp(i,j,k) )
             areaFraction(i,j) = expL - expR
             volFraction (i,j) =
     &            ( ( hL + hrExp(i,j,k) ) * expL
     &            - ( hR + hrExp(i,j,k) ) * expR )
     &            / ( hrMin(i,j,k) + hrExp(i,j,k) )
            ENDIF
           ENDDO
          ENDDO
         ELSE
          DO j=jMin,jMax
           DO i=iMin,iMax
            IF ( doRidging(i,j) .AND. hrExp(i,j,k) .NE. 0. _d 0 ) THEN
              hL   = MAX( hrMin(i,j,k), hLimit(l-1) )
              expL = EXP(-( hL - hrMin(i,j,k) )/hrExp(i,j,k) )
              areaFraction(i,j) = expL
              volFraction (i,j) = ( hL + hrExp(i,j,k) ) * expL
     &             / ( hrMin(i,j,k) + hrExp(i,j,k) )
             ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDIF 
        areaTest=areaTest + areaFraction(ii,jj)
        volTest =volTest  + volFraction(ii,jj)
C     after computing the fraction ridged ice for this category, apply it 
        DO j=jMin,jMax
         DO i=iMin,iMax
          AREAITD(i,j,l,bi,bj) = AREAITD(i,j,l,bi,bj)
     &         +areaFraction(i,j)*ridgingArea(i,j)*areaScaling(i,j)
          HEFFITD(i,j,l,bi,bj) = HEFFITD(i,j,l,bi,bj)
     &         +volFraction(i,j)*ridgingHeff(i,j)
          HSNOWITD(i,j,l,bi,bj) = HSNOWITD(i,j,l,bi,bj)
     &         +volFraction(i,j)*ridgingHsnw(i,j)
         ENDDO
        ENDDO
C     category l-loop
       ENDDO
C     category k-loop
      ENDDO
C     determine if the ridging process needs to be repeated
C     we need a new estimate of the total AREA
      DO j=jMin,jMax
       DO i=iMin,iMax
        netArea(i,j) = 0. _d 0
       ENDDO
      ENDDO
      DO k=1,nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         netArea(i,j) = netArea(i,j) + AREAITD(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      doRidging(i,j) = .FALSE.
      doRidgeAgain   = .FALSE.
      DO j=jMin,jMax
       DO i=iMin,iMax
        tmp = netArea(i,j)+opnWtrFrac(i,j,bi,bj)
        areaTooLarge = tmp - 1. _d 0 .GT. 1. _d -11
        IF ( HEFFM(i,j,bi,bj) .GT. 0. _d 0 .AND. areaTooLarge ) THEN
         doRidging(i,j) = .TRUE.
         doRidgeAgain   = .TRUE.
         divAdv = (1. _d 0-tmp)*recip_deltaT
         closingRate(I,J) = MAX( 0.D0, -divAdv)
         openingRate(I,J) = MAX( 0.D0,  divAdv)
        ENDIF
       ENDDO
      ENDDO
      doRidgeAgain = doRidgeAgain .AND. n.LT.SEAICEridgingIterMax
      IF ( doRidgeAgain ) CALL SEAICE_PREPARE_RIDGING(
     O     hrMin, hrMax, hrExp, ridgeRatio, ridgingModeNorm, partFunc,
     I     iMin, iMax, jMin, jMax, bi, bj, myTime, myIter, myThid )
      n = n + 1
C     ridging iteration
      ENDDO
C     .not. SEAICEsimpleRidging
      ENDIF
#endif /* SEAICE_ITD */

C     after ridging is complete, the critical variables need to be 
C     regularized and checked for consistency. This is done in a separate 
C     routine
CML      CALL SEAICE_REGULARIZE( bi, bj, myTime, myIter, myThid )

      RETURN
      END
