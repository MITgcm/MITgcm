C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_calc.F,v 1.6 2014/08/25 21:50:02 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_CALC
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_CALC(
     O                  statArr,
     I                  inpArr, frcArr, scaleFact, power, useFract,
     I                  useReg, regMskVal,
     I                  nStats,sizI1,sizI2,sizJ1,sizJ2, iRun,jRun,
     I                  regMask, arrMask, arrhFac, arrArea,
     I                  arrDr, specialVal, exclSpVal, useWeight,
     I                  myThid )

C     !DESCRIPTION:
C     Compute statistics for this tile, level, region

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     statArr     :: output statistics array
C     inpArr      :: input field array to process (compute stats & add to statFld)
C     frcArr      :: fraction used for weighted-average diagnostics
C     scaleFact   :: scaling factor
C     power       :: option to fill-in with the field square (power=2)
C     useFract    :: if True, use fraction-weight
C     useReg      :: how to use region-mask: =0 : not used ;
C                    =1 : grid-center location ; =2 : U location ; =3 : V location
C     regMskVal   :: region-mask identificator value
C                    (point i,j belong to region <=> regMask(i,j) = regMskVal)
C     nStats      :: size of output array: statArr
C     sizI1,sizI2 :: size of inpArr array: 1rst index range (min,max)
C     sizJ1,sizJ2 :: size of inpArr array: 2nd  index range (min,max)
C     iRun,jRun   :: range of 1rst & 2nd index to process
C     regMask     :: regional mask
C     arrMask     :: mask for this input array
C     arrhFac     :: weight factor (horizontally varying)
C     arrArea     :: Area weighting factor
C     arrDr       :: uniform weighting factor
C     specialVal  :: special value in input array (to exclude if exclSpVal=T)
C     exclSpVal   :: if T, exclude "specialVal" in input array
C     useWeight   :: use weight factor "arrhFac"
Cc    k,bi,bj     :: level and tile indices used for weighting (mask,area ...)
Cc    parsFld     :: parser field with characteristics of the diagnostics
C     myThid      :: my Thread Id number
      INTEGER nStats,sizI1,sizI2,sizJ1,sizJ2
      INTEGER iRun, jRun
      _RL statArr(0:nStats)
      _RL inpArr (sizI1:sizI2,sizJ1:sizJ2)
      _RL frcArr (sizI1:sizI2,sizJ1:sizJ2)
      _RL scaleFact
      INTEGER power
      LOGICAL useFract
      INTEGER useReg
      _RS regMskVal
      _RS regMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL arrDr
      _RL specialVal
      LOGICAL exclSpVal
      LOGICAL useWeight
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j    :: loop indices
      INTEGER i, j, n
      INTEGER im, ix
#ifndef TARGET_NEC_SX
      LOGICAL inside(sNx+1,sNy+1)
      _RL     tmpFld(sNx+1,sNy+1)
      _RL     tmpVol(sNx+1,sNy+1)
#else
C     Extra variables and fields to support vectorization.
C     This code also uses the intrinsic F90 routines SUM, MINVAL, MAXVAL
C     and thus will not compile with a F77 compiler.
      _RL     arrMaskL(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpVol  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

      im = nStats - 1
      ix = nStats
      DO n=0,nStats
        statArr(n) = 0.
      ENDDO

#ifndef TARGET_NEC_SX

C-    Apply Scaling Factor and power option to Input Field (-> tmpFld):
      IF ( power.EQ.2 ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          tmpFld(i,j) = scaleFact*inpArr(i,j)
          tmpFld(i,j) = tmpFld(i,j)*tmpFld(i,j)
        ENDDO
       ENDDO
      ELSE
       DO j = 1,jRun
        DO i = 1,iRun
          tmpFld(i,j) = scaleFact*inpArr(i,j)
        ENDDO
       ENDDO
      ENDIF

C-    Set weight factor "tmpVol" (area and hFac and/or fraction field)
C     and part of domain (=inside) where to compute stats
      IF ( useFract .AND. useWeight ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          inside(i,j) = arrMask(i,j).NE.0.
     &            .AND. arrhFac(i,j).NE.0.
     &            .AND. frcArr(i,j) .NE.0.
          tmpVol(i,j) = arrArea(i,j)*arrhFac(i,j)*frcArr(i,j)
        ENDDO
       ENDDO
      ELSEIF ( useFract ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          inside(i,j) = arrMask(i,j).NE.0.
     &            .AND. arrhFac(i,j).NE.0.
     &            .AND. frcArr(i,j) .NE.0.
          tmpVol(i,j) = arrArea(i,j)*frcArr(i,j)
        ENDDO
       ENDDO
      ELSEIF ( useWeight ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          inside(i,j) = arrMask(i,j).NE.0.
     &            .AND. arrhFac(i,j).NE.0.
          tmpVol(i,j) = arrArea(i,j)*arrhFac(i,j)
        ENDDO
       ENDDO
      ELSE
       DO j = 1,jRun
        DO i = 1,iRun
          inside(i,j) = arrMask(i,j).NE.0.
     &            .AND. arrhFac(i,j).NE.0.
          tmpVol(i,j) = arrArea(i,j)
        ENDDO
       ENDDO
      ENDIF

C-    Exclude (setting inside=F) Special Value:
      IF ( exclSpVal ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          inside(i,j) = inside(i,j) .AND. inpArr(i,j).NE.specialVal
        ENDDO
       ENDDO
      ENDIF
C-    Account for Region-mask (refine "inside"):
      IF ( useReg.EQ.1 ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          inside(i,j) = inside(i,j) .AND. regMask(i,j).EQ.regMskVal
        ENDDO
       ENDDO
      ELSEIF ( useReg.EQ.2 ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          inside(i,j) = inside(i,j) .AND.( regMask(i,j).EQ.regMskVal
     &                              .OR. regMask(i-1,j).EQ.regMskVal )
        ENDDO
       ENDDO
      ELSEIF ( useReg.EQ.3 ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          inside(i,j) = inside(i,j) .AND.( regMask(i,j).EQ.regMskVal
     &                              .OR. regMask(i,j-1).EQ.regMskVal )
        ENDDO
       ENDDO
      ENDIF

C-    Calculate Stats
      DO j = 1,jRun
       DO i = 1,iRun
        IF ( inside(i,j) ) THEN
          statArr(im) = tmpFld(i,j)
          statArr(0) = statArr(0) + tmpVol(i,j)
          statArr(1) = statArr(1) + tmpVol(i,j)*tmpFld(i,j)
          statArr(2) = statArr(2) + tmpVol(i,j)*tmpFld(i,j)*tmpFld(i,j)
        ENDIF
       ENDDO
      ENDDO
      statArr(ix) = statArr(im)
      DO j = 1,jRun
       DO i = 1,iRun
        IF ( inside(i,j) ) THEN
          statArr(im) = MIN(tmpFld(i,j),statArr(im))
          statArr(ix) = MAX(tmpFld(i,j),statArr(ix))
        ENDIF
       ENDDO
      ENDDO
      statArr(0) = statArr(0)*arrDr
      statArr(1) = statArr(1)*arrDr
      statArr(2) = statArr(2)*arrDr

#else /* TARGET_NEC_SX defined */

      arrMaskL = 0. _d 0

      IF ( useFract .AND. exclSpVal ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
         IF ( arrMask(i,j).NE.0. .AND. frcArr(i,j).NE.0.
     &             .AND. arrhFac(i,j).NE.0.
     &             .AND. inpArr(i,j).NE.specialVal )
     &        arrMaskL(i,j) = 1. _d 0
        ENDDO
       ENDDO
       IF ( useWeight ) THEN
        tmpVol = arrhFac*arrArea*frcArr
       ELSE
        tmpVol = arrArea*frcArr
       ENDIF

      ELSEIF ( useFract ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
         IF ( arrMask(i,j).NE.0. .AND. frcArr(i,j).NE.0.
     &             .AND. arrhFac(i,j).NE.0. )
     &        arrMaskL(i,j) = 1. _d 0
        ENDDO
       ENDDO
       IF ( useWeight ) THEN
        tmpVol = arrhFac*arrArea*frcArr
       ELSE
        tmpVol = arrArea*frcArr
       ENDIF

      ELSEIF ( exclSpVal ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
         IF ( arrMask(i,j).NE.0.
     &             .AND. arrhFac(i,j).NE.0.
     &             .AND. inpArr(i,j).NE.specialVal )
     &        arrMaskL(i,j) = 1. _d 0
        ENDDO
       ENDDO
       IF ( useWeight ) THEN
        tmpVol = arrhFac*arrArea
       ELSE
        tmpVol = arrArea
       ENDIF

      ELSE

       DO j = 1,jRun
        DO i = 1,iRun
         IF ( arrMask(i,j).NE.0.
     &             .AND. arrhFac(i,j).NE.0. )
     &        arrMaskL(i,j) = 1. _d 0
        ENDDO
       ENDDO
       IF ( useWeight ) THEN
        tmpVol = arrhFac*arrArea
       ELSE
        tmpVol = arrArea
       ENDIF

      ENDIF

C-    Account for Region-mask:
      IF ( useReg.EQ.1 ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          IF ( regMask(i,j).NE.regMskVal ) arrMaskL(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ELSEIF ( useReg.EQ.2 ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          IF ( regMask(i,j).NE.regMskVal .AND.
     &       regMask(i-1,j).NE.regMskVal ) arrMaskL(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ELSEIF ( useReg.EQ.3 ) THEN
       DO j = 1,jRun
        DO i = 1,iRun
          IF ( regMask(i,j).NE.regMskVal .AND.
     &       regMask(i,j-1).NE.regMskVal ) arrMaskL(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

C     inpArr can be undefined/non-initialised in overlaps, so we need
C     to clean this fields first by copying the defined range to tmpFld
      tmpFld = 0. _d 0
      DO j = 1,jRun
       DO i = 1,iRun
        tmpFld(i,j) = inpArr(i,j)*scaleFact
       ENDDO
      ENDDO
      IF ( power.EQ.2) THEN
       tmpFld = tmpFld*tmpFld
      ENDIF
C     sum up the volume
      tmpVol = tmpVol*arrMaskL
      statArr(0)  = SUM(tmpVol)*arrDr
C     compute and sum up volume*field
      tmpVol = tmpVol*tmpFld
      statArr(1)  = SUM(tmpVol)*arrDr
C     compute and sum up volume*field**2
      tmpVol = tmpVol*tmpFld
      statArr(2)  = SUM(tmpVol)*arrDr
      statArr(im) = MINVAL(tmpFld, MASK = arrMaskL>0.)
      statArr(ix) = MAXVAL(tmpFld, MASK = arrMaskL>0.)

#endif /* TARGET_NEC_SX */

      RETURN
      END
