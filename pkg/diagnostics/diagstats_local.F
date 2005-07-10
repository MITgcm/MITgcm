C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_local.F,v 1.3 2005/07/10 00:57:18 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_LOCAL
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_LOCAL(
     U                  statFld,
     I                  inpFld, frcFld, scaleFact, useFract,sizF,
     I                  sizI1,sizI2,sizJ1,sizJ2,sizK,sizTx,sizTy,
     I                  iRun,jRun,kIn,biIn,bjIn,
     I                  k,bi,bj, region2fill, ndId, parsFld,
     I                  myThid)

C     !DESCRIPTION:
C     Update array statFld
C     by adding statistics over the range [1:iRun],[1:jRun]
C     from input field array inpFld
C- note:
C   a) this S/R should not see DIAGNOSTICS pkg commons blocks (in DIAGNOSTICS.h)
C   b) for main grid variables, get area & weigting factors (to compute global mean)
C      from the main common blocks.
C   c) for other type of grids, call a specifics S/R which include its own
C      grid common blocks

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
c #include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     statFld     :: cumulative statistics array (updated)
C     inpFld      :: input field array to process (compute stats & add to statFld)
C     frcFld      :: fraction used for weighted-average diagnostics
C     scaleFact   :: scaling factor
C     useFract    :: if True, use fraction-weight
C     sizF        :: size of frcFld array: 3rd  dimension
C     sizI1,sizI2 :: size of inpFld array: 1rst index range (min,max)
C     sizJ1,sizJ2 :: size of inpFld array: 2nd  index range (min,max)
C     sizK        :: size of inpFld array: 3rd  dimension
C     sizTx,sizTy :: size of inpFld array: tile dimensions
C     iRun,jRun   :: range of 1rst & 2nd index
C     kIn         :: level index of inpFld array to porcess
C     biIn,bjIn   :: tile indices of inpFld array to process
C     k,bi,bj     :: level and tile indices used for weighting (mask,area ...)
C     region2fill :: indicates whether to compute statistics over this region
C     ndId        :: Diagnostics Id Number (in available diag. list)
C     parsFld     :: parser field with characteristics of the diagnostics
C     myThid      :: my Thread Id number
      _RL     statFld(0:nStats,0:nRegions)
      INTEGER sizI1,sizI2,sizJ1,sizJ2
      INTEGER sizF,sizK,sizTx,sizTy
      _RL     inpFld(sizI1:sizI2,sizJ1:sizJ2,sizK,sizTx,sizTy)
      _RL     frcFld(sizI1:sizI2,sizJ1:sizJ2,sizF,sizTx,sizTy)
      _RL     scaleFact
      LOGICAL useFract
      INTEGER iRun, jRun, kIn, biIn, bjIn
      INTEGER k, bi, bj, ndId
      INTEGER region2fill(0:nRegions)
      CHARACTER*16 parsFld
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j    :: loop indices
      INTEGER i, n, km, kFr
      INTEGER im, ix, iv
      PARAMETER ( iv = nStats - 2 , im = nStats - 1 , ix = nStats )
      LOGICAL exclSpVal
      LOGICAL useWeight
      _RL statLoc(0:nStats)
      _RL drLoc
      _RL specialVal
      _RL getcon
      EXTERNAL getcon

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      useWeight = .FALSE.
      exclSpVal = .FALSE.
      specialVal = 0.
      IF ( useFIZHI ) THEN
        exclSpVal = .TRUE.
        specialVal = getcon('UNDEF')
      ENDIF
      kFr = MIN(kIn,sizF)

      DO n=0,nRegions
       IF (region2fill(n).NE.0) THEN
C---   Compute statistics for this tile, level and region:

C-     case of a special region (no specific regional mask)
        IF ( n.EQ.0 ) THEN

         IF ( parsFld(10:10) .EQ. 'R' ) THEN

          drLoc = drF(k)
          IF ( parsFld(9:9).EQ.'L') drLoc = drC(k)
          IF ( parsFld(9:9).EQ.'U') drLoc = drC(MIN(k+1,Nr))
          IF ( parsFld(9:9).EQ.'M') useWeight = .TRUE.

          IF     ( parsFld(2:2).EQ.'U' ) THEN
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskW(1-Olx,1-Oly,k,bi,bj),
     I            hFacW(1-Olx,1-Oly,k,bi,bj), rAw(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
c    I            drLoc, k,bi,bj, parsFld, myThid )
          ELSEIF ( parsFld(2:2).EQ.'V' ) THEN
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskS(1-Olx,1-Oly,k,bi,bj),
     I            hFacS(1-Olx,1-Oly,k,bi,bj), rAs(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSE
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskC(1-Olx,1-Oly,k,bi,bj),
     I            hFacC(1-Olx,1-Oly,k,bi,bj), rA(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ENDIF

         ELSEIF ( useFIZHI .AND.
     &           (parsFld(10:10).EQ.'L' .OR. parsFld(10:10).EQ.'M')
     &          ) THEN
           CALL  DIAGSTATS_LM_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskH(1-Olx,1-Oly,bi,bj),
     I            rA(1-Olx,1-Oly,bi,bj),
     I            specialVal, exclSpVal,
     I            k,bi,bj, parsFld, myThid )
         ELSEIF ( useLand .AND.
     &           (parsFld(10:10).EQ.'G' .OR. parsFld(10:10).EQ.'g')
     &          ) THEN
           CALL  DIAGSTATS_G_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj),
     I            rA(1-Olx,1-Oly,bi,bj),
     I            specialVal, exclSpVal,
     I            k,bi,bj, parsFld, myThid )
c        ELSEIF ( parsFld(10:10) .EQ. 'I' ) THEN
c        ELSEIF ( parsFld(10:10) .EQ. '1' ) THEN
         ELSE

          km = 1
          IF ( usingPCoords ) km = Nr
          drLoc = 1. _d 0
          IF     ( parsFld(2:2).EQ.'U' ) THEN
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskW(1-Olx,1-Oly,km,bi,bj),
     I            maskW(1-Olx,1-Oly,km,bi,bj),rAw(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSEIF ( parsFld(2:2).EQ.'V' ) THEN
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskS(1-Olx,1-Oly,km,bi,bj),
     I            maskS(1-Olx,1-Oly,km,bi,bj),rAs(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSE
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskH(1-Olx,1-Oly,bi,bj),
     I            maskH(1-Olx,1-Oly,bi,bj), rA(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ENDIF

         ENDIF

C     Update cumulative statistics array
          IF ( statLoc(0).GT.0. ) THEN
            IF ( statFld(0,n).LE.0. ) THEN
              statFld(im,n) = statLoc(im)
              statFld(ix,n) = statLoc(ix)
            ELSE
              statFld(im,n) = MIN( statFld(im,n), statLoc(im) )
              statFld(ix,n) = MAX( statFld(ix,n), statLoc(ix) )
            ENDIF
            DO i=0,iv
              statFld(i,n) = statFld(i,n) + statLoc(i)
            ENDDO
          ENDIF

        ENDIF

C---   processing region "n" ends here.
       ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: DIAGSTATS_LM_CALC
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_LM_CALC(
     O                  statArr,
     I                  inpArr, frcArr, scaleFact, useFract,
     I                  nStats,sizI1,sizI2,sizJ1,sizJ2, iRun,jRun,
     I                  regMask, arrMask, arrArea,
     I                  specialVal, exclSpVal,
     I                  k,bi,bj, parsFld, myThid )

C     !DESCRIPTION:
C     Compute statistics for this tile, level, region
C     using FIZHI level thickness

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#include "gridalt_mapping.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     statArr     :: output statistics array
C     inpArr      :: input field array to process (compute stats & add to statFld)
C     frcArr      :: fraction used for weighted-average diagnostics
C     useFract    :: if True, use fraction-weight
C     scaleFact   :: scaling factor
C     nStats      :: size of output statArr
C     sizI1,sizI2 :: size of inpArr array: 1rst index range (min,max)
C     sizJ1,sizJ2 :: size of inpArr array: 2nd  index range (min,max)
C     iRun,jRun   :: range of 1rst & 2nd index to process
C     regMask     :: regional mask
C     arrMask     :: mask for this input array
C     arrArea     :: Area weighting factor
C     specialVal  :: special value in input array (to exclude if exclSpVal=T)
C     exclSpVal   :: if T, exclude "specialVal" in input array
C     k,bi,bj     :: level and tile indices used for weighting (mask,area ...)
C     parsFld     :: parser field with characteristics of the diagnostics
C     myThid      :: my Thread Id number
      INTEGER nStats,sizI1,sizI2,sizJ1,sizJ2
      INTEGER iRun, jRun
      _RL statArr(0:nStats)
      _RL inpArr (sizI1:sizI2,sizJ1:sizJ2)
      _RL frcArr (sizI1:sizI2,sizJ1:sizJ2)
      _RL scaleFact
      LOGICAL useFract
      _RS regMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL specialVal
      LOGICAL exclSpVal
      INTEGER k, bi, bj
      CHARACTER*16 parsFld
      INTEGER myThid
CEOP

#ifdef ALLOW_FIZHI
C     !LOCAL VARIABLES:
      LOGICAL useWeight
      INTEGER kl
      _RL drLoc

c     IF ( useFIZHI ) THEN

        IF ( parsFld(10:10).EQ.'L' ) THEN
          kl = 1 + Nrphys - k
          useWeight = .TRUE.
        ELSE
          kl = 1
          useWeight = .FALSE.
        ENDIF
        drLoc = 1. _d 0

C- jmc: here we have a Problem if RL & RS are not the same:
C    since dpphys is RL but argument is RS. leave it like this for now
C    and will change it once the Regions are fully implemented.

        CALL  DIAGSTATS_CALC(
     O            statArr,
     I            inpArr, frcArr, scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            regMask, arrMask,
     I            dpphys(1-Olx,1-Oly,kl,bi,bj), arrArea,
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )

c     ENDIF
#endif /* ALLOW_FIZHI */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: DIAGSTATS_G_CALC
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_G_CALC(
     O                  statArr,
     I                  inpArr, frcArr, scaleFact, useFract,
     I                  nStats,sizI1,sizI2,sizJ1,sizJ2, iRun,jRun,
     I                  regMask, arrArea,
     I                  specialVal, exclSpVal,
     I                  k,bi,bj, parsFld, myThid )

C     !DESCRIPTION:
C     Compute statistics for this tile, level, region
C     using "ground" (land) type fraction

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#ifdef ALLOW_LAND
# include "LAND_SIZE.h"
# include "LAND_PARAMS.h"
# ifdef ALLOW_AIM
#  include "AIM_FFIELDS.h"
# endif
#else
# include "SIZE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     statArr     :: output statistics array
C     inpArr      :: input field array to process (compute stats & add to statFld)
C     frcArr      :: fraction used for weighted-average diagnostics
C     useFract    :: if True, use fraction-weight
C     scaleFact   :: scaling factor
C     nStats      :: size of output statArr
C     sizI1,sizI2 :: size of inpArr array: 1rst index range (min,max)
C     sizJ1,sizJ2 :: size of inpArr array: 2nd  index range (min,max)
C     iRun,jRun   :: range of 1rst & 2nd index to process
C     regMask     :: regional mask
C     arrArea     :: Area weighting factor
C     specialVal  :: special value in input array (to exclude if exclSpVal=T)
C     exclSpVal   :: if T, exclude "specialVal" in input array
C     k,bi,bj     :: level and tile indices used for weighting (mask,area ...)
C     parsFld     :: parser field with characteristics of the diagnostics
C     myThid      :: my Thread Id number
      INTEGER nStats,sizI1,sizI2,sizJ1,sizJ2
      INTEGER iRun, jRun
      _RL statArr(0:nStats)
      _RL inpArr (sizI1:sizI2,sizJ1:sizJ2)
      _RL frcArr (sizI1:sizI2,sizJ1:sizJ2)
      _RL scaleFact
      LOGICAL useFract
      _RS regMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL specialVal
      LOGICAL exclSpVal
      INTEGER k, bi, bj
      CHARACTER*16 parsFld
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND
C     !LOCAL VARIABLES:
      LOGICAL useWeight
      INTEGER kl
      _RL drLoc

c     IF ( useLand ) THEN

        IF ( parsFld(10:10).EQ.'G' ) THEN
          kl = MIN(k,land_nLev)
          drLoc = land_dzF(kl)
        ELSE
          drLoc = 1. _d 0
        ENDIF
        useWeight = .TRUE.

        CALL  DIAGSTATS_CALC(
     O            statArr,
     I            inpArr, frcArr, scaleFact, useFract,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            regMask, aim_landFr(1-Olx,1-Oly,bi,bj),
     I            aim_landFr(1-Olx,1-Oly,bi,bj), arrArea,
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )

c     ENDIF
#endif /* ALLOW_LAND */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: DIAGSTATS_CALC
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_CALC(
     O                  statArr,
     I                  inpArr, frcArr, scaleFact, useFract,
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
C     useFract    :: if True, use fraction-weight
C     scaleFact   :: scaling factor
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
      LOGICAL useFract
      _RS regMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL arrDr
      _RL specialVal
      LOGICAL exclSpVal
      LOGICAL useWeight
c     INTEGER k, bi, bj
c     CHARACTER*16 parsFld
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j    :: loop indices
      INTEGER i, j, n
      INTEGER im, ix
      _RL tmpVol
      _RL tmpFld

      im = nStats - 1
      ix = nStats
      DO n=0,nStats
        statArr(n) = 0.
      ENDDO

      IF ( useFract .AND. exclSpVal ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF ( arrMask(i,j).NE.0. .AND. frcArr(i,j).NE.0.
     &                     .AND. inpArr(i,j).NE.specialVal ) THEN
            tmpFld = scaleFact*inpArr(i,j)
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
          IF ( arrMask(i,j).NE.0. .AND. frcArr(i,j).NE.0. ) THEN
            tmpFld = scaleFact*inpArr(i,j)
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
     &                     .AND. inpArr(i,j).NE.specialVal ) THEN
            tmpFld = scaleFact*inpArr(i,j)
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
c         IF ( regMask(i,j).NE.0. .AND. arrMask(i,j).NE.0. ) THEN
          IF ( arrMask(i,j).NE.0. ) THEN
            tmpFld = scaleFact*inpArr(i,j)
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
