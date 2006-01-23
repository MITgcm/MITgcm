C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_calc.F,v 1.1 2006/01/23 22:31:11 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_CALC
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_CALC(
     O                  statArr,
     I                  inpArr, frcArr, scaleFact, power, useFract,
     I                  regId, regMskVal,
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
C     regId       :: region number Id
C     regMskVal   :: region-mask identificator value
C                (point i,j belong to region "regId" <=> regMask(i,j) = regMskVal)
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
      INTEGER regId
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
      _RL tmpVol
      _RL tmpFld
      _RL tmpFac

      im = nStats - 1
      ix = nStats
      DO n=0,nStats
        statArr(n) = 0.
      ENDDO
      tmpFac = scaleFact
      IF ( power.EQ.2) tmpFac = scaleFact*scaleFact

      IF ( regId.EQ.0 .AND. useFract .AND. exclSpVal ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0. .AND. frcArr(i,j).NE.0.
     &                     .AND. inpArr(i,j).NE.specialVal ) THEN
            IF ( power.EQ.2) THEN
              tmpFld = tmpFac*inpArr(i,j)*inpArr(i,j)
            ELSE
              tmpFld = tmpFac*inpArr(i,j)
            ENDIF
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = tmpFld
              statArr(ix) = tmpFld
            ELSE
              statArr(im) = MIN(tmpFld,statArr(im))
              statArr(ix) = MAX(tmpFld,statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)*frcArr(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)*frcArr(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*tmpFld
            statArr(2) = statArr(2) + tmpVol*tmpFld*tmpFld
          ENDIF
        ENDDO
       ENDDO

      ELSEIF ( regId.EQ.0 .AND. useFract ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0. .AND. frcArr(i,j).NE.0. ) THEN
            IF ( power.EQ.2) THEN
              tmpFld = tmpFac*inpArr(i,j)*inpArr(i,j)
            ELSE
              tmpFld = tmpFac*inpArr(i,j)
            ENDIF
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = tmpFld
              statArr(ix) = tmpFld
            ELSE
              statArr(im) = MIN(tmpFld,statArr(im))
              statArr(ix) = MAX(tmpFld,statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)*frcArr(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)*frcArr(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*tmpFld
            statArr(2) = statArr(2) + tmpVol*tmpFld*tmpFld
          ENDIF
        ENDDO
       ENDDO

      ELSEIF ( regId.EQ.0 .AND. exclSpVal ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0.
     &                     .AND. inpArr(i,j).NE.specialVal ) THEN
            IF ( power.EQ.2) THEN
              tmpFld = tmpFac*inpArr(i,j)*inpArr(i,j)
            ELSE
              tmpFld = tmpFac*inpArr(i,j)
            ENDIF
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = tmpFld
              statArr(ix) = tmpFld
            ELSE
              statArr(im) = MIN(tmpFld,statArr(im))
              statArr(ix) = MAX(tmpFld,statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*tmpFld
            statArr(2) = statArr(2) + tmpVol*tmpFld*tmpFld
          ENDIF
        ENDDO
       ENDDO

      ELSEIF ( regId.EQ.0 ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0. ) THEN
            IF ( power.EQ.2) THEN
              tmpFld = tmpFac*inpArr(i,j)*inpArr(i,j)
            ELSE
              tmpFld = tmpFac*inpArr(i,j)
            ENDIF
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = tmpFld
              statArr(ix) = tmpFld
            ELSE
              statArr(im) = MIN(tmpFld,statArr(im))
              statArr(ix) = MAX(tmpFld,statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*tmpFld
            statArr(2) = statArr(2) + tmpVol*tmpFld*tmpFld
          ENDIF
        ENDDO
       ENDDO

      ELSEIF ( useFract .AND. exclSpVal ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0. .AND. frcArr(i,j).NE.0.
     &                     .AND. inpArr(i,j).NE.specialVal
     &                     .AND. regMask(i,j).EQ.regMskVal ) THEN
            IF ( power.EQ.2) THEN
              tmpFld = tmpFac*inpArr(i,j)*inpArr(i,j)
            ELSE
              tmpFld = tmpFac*inpArr(i,j)
            ENDIF
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = tmpFld
              statArr(ix) = tmpFld
            ELSE
              statArr(im) = MIN(tmpFld,statArr(im))
              statArr(ix) = MAX(tmpFld,statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)*frcArr(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)*frcArr(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*tmpFld
            statArr(2) = statArr(2) + tmpVol*tmpFld*tmpFld
          ENDIF
        ENDDO
       ENDDO

      ELSEIF ( useFract ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0. .AND. frcArr(i,j).NE.0.
     &                     .AND. regMask(i,j).EQ.regMskVal ) THEN
            IF ( power.EQ.2) THEN
              tmpFld = tmpFac*inpArr(i,j)*inpArr(i,j)
            ELSE
              tmpFld = tmpFac*inpArr(i,j)
            ENDIF
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = tmpFld
              statArr(ix) = tmpFld
            ELSE
              statArr(im) = MIN(tmpFld,statArr(im))
              statArr(ix) = MAX(tmpFld,statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)*frcArr(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)*frcArr(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*tmpFld
            statArr(2) = statArr(2) + tmpVol*tmpFld*tmpFld
          ENDIF
        ENDDO
       ENDDO

      ELSEIF ( exclSpVal ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0.
     &                     .AND. inpArr(i,j).NE.specialVal
     &                     .AND. regMask(i,j).EQ.regMskVal ) THEN
            IF ( power.EQ.2) THEN
              tmpFld = tmpFac*inpArr(i,j)*inpArr(i,j)
            ELSE
              tmpFld = tmpFac*inpArr(i,j)
            ENDIF
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = tmpFld
              statArr(ix) = tmpFld
            ELSE
              statArr(im) = MIN(tmpFld,statArr(im))
              statArr(ix) = MAX(tmpFld,statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*tmpFld
            statArr(2) = statArr(2) + tmpVol*tmpFld*tmpFld
          ENDIF
        ENDDO
       ENDDO

      ELSE

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0.
     &                     .AND. regMask(i,j).EQ.regMskVal ) THEN
            IF ( power.EQ.2) THEN
              tmpFld = tmpFac*inpArr(i,j)*inpArr(i,j)
            ELSE
              tmpFld = tmpFac*inpArr(i,j)
            ENDIF
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = tmpFld
              statArr(ix) = tmpFld
            ELSE
              statArr(im) = MIN(tmpFld,statArr(im))
              statArr(ix) = MAX(tmpFld,statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*tmpFld
            statArr(2) = statArr(2) + tmpVol*tmpFld*tmpFld
          ENDIF
        ENDDO
       ENDDO

      ENDIF

      RETURN
      END
