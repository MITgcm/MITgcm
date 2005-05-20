C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_local.F,v 1.1 2005/05/20 07:28:52 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_LOCAL
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_LOCAL(
     U                  statFld,
     I                  inpFld,
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
#include "SURFACE.h"
#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#include "gridalt_mapping.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     statFld     :: cumulative statistics array (updated)
C     inpFld      :: input field array to process (compute stats & add to statFld)
C     sizI1,sizI2 :: size of inpFld array: 1rst index range (min,max)
C     sizJ1,sizJ2 :: size of inpFld array: 2nd  index range (min,max)
C     sizK        :: size of inpFld array: 3rd  dimension
C     sizTx,sizTy :: size of inpFld array: tile dimensions
C     iRun,jRun   :: range of 1rst & 2nd index
C     kIn         :: level index of inFld array to porcess
C     biIn,bjIn   :: tile indices of inFld array to process
C     k,bi,bj     :: level and tile indices used for weighting (mask,area ...)
C     region2fill :: indicates whether to compute statistics over this region
C     ndId        :: Diagnostics Id Number (in available diag. list)
C     parsFld     :: parser field with characteristics of the diagnostics
C     myThid      :: my Thread Id number
      _RL     statFld(0:nStats,0:nRegions)
      INTEGER sizI1,sizI2,sizJ1,sizJ2
      INTEGER sizK,sizTx,sizTy
      _RL     inpFld(sizI1:sizI2,sizJ1:sizJ2,sizK,sizTx,sizTy)
      INTEGER iRun, jRun, kIn, biIn, bjIn
      INTEGER k, bi, bj, ndId
      INTEGER region2fill(0:nRegions)
      CHARACTER*16 parsFld
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j    :: loop indices
      INTEGER i, n, km
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
           CALL  DIAGSTATS_R_CALC( 
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskW(1-Olx,1-Oly,k,bi,bj),
     I            hFacW(1-Olx,1-Oly,k,bi,bj), rAw(1-Olx,1-Oly,bi,bj), 
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
c    I            drLoc, k,bi,bj, parsFld, myThid )
          ELSEIF ( parsFld(2:2).EQ.'V' ) THEN
           CALL  DIAGSTATS_R_CALC( 
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskS(1-Olx,1-Oly,k,bi,bj),
     I            hFacS(1-Olx,1-Oly,k,bi,bj), rAs(1-Olx,1-Oly,bi,bj), 
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSE
           CALL  DIAGSTATS_R_CALC( 
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskC(1-Olx,1-Oly,k,bi,bj),
     I            hFacC(1-Olx,1-Oly,k,bi,bj), rA(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ENDIF

#ifdef ALLOW_FIZHI
c        ELSEIF ( parsFld(10:10) .EQ. 'L' ) THEN
         ELSEIF ( parsFld(10:10) .EQ. 'M' ) THEN
           drLoc = 1. _d 0
           km = 1 + Nrphys - k
           CALL  DIAGSTATS_R_CALC( 
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskH(1-Olx,1-Oly,bi,bj),
     I            dpphys(1-Olx,1-Oly,km,bi,bj), rA(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
#endif
#ifdef ALLOW_LAND
c        ELSEIF ( parsFld(10:10) .EQ. 'G' ) THEN
#endif
c        ELSEIF ( parsFld(10:10) .EQ. 'I' ) THEN
c        ELSEIF ( parsFld(10:10) .EQ. '1' ) THEN
         ELSE

          km = 1
          IF ( usingPCoords ) km = Nr
          drLoc = 1. _d 0
          IF     ( parsFld(2:2).EQ.'U' ) THEN
           CALL  DIAGSTATS_R_CALC( 
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskW(1-Olx,1-Oly,km,bi,bj),
     I            maskW(1-Olx,1-Oly,km,bi,bj),rAw(1-Olx,1-Oly,bi,bj), 
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSEIF ( parsFld(2:2).EQ.'V' ) THEN
           CALL  DIAGSTATS_R_CALC( 
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            maskH(1-Olx,1-Oly,bi,bj), maskS(1-Olx,1-Oly,km,bi,bj),
     I            maskS(1-Olx,1-Oly,km,bi,bj),rAs(1-Olx,1-Oly,bi,bj), 
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSE
           CALL  DIAGSTATS_R_CALC( 
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
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
C     !ROUTINE: DIAGSTATS_CALC
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_R_CALC(
     U                  statArr,
     I                  inpArr,
     I                  sizI1,sizI2,sizJ1,sizJ2, iRun,jRun,
     I                  regMask, arrMask, arrhFac, arrArea,
     I                  arrDr, specialVal, exclSpVal, useWeight,
     I                  myThid )
c    I                  arrDr, k,bi,bj, parsFld, myThid )

C     !DESCRIPTION:
C     Compute statistics for this tile, level, region

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
c #include "PARAMS.h"
c #include "GRID.h"
c #include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     statArr     :: cumulative statistics array (updated)
C     inpArr      :: input field array to process (compute stats & add to statFld)
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
      _RL statArr(0:nStats)
      INTEGER sizI1,sizI2,sizJ1,sizJ2
      INTEGER iRun, jRun
      _RL inpArr (sizI1:sizI2,sizJ1:sizJ2)
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
      INTEGER im, ix, iv
      PARAMETER ( iv = nStats - 2 , im = nStats - 1 , ix = nStats )
      _RL tmpVol

      DO n=0,nStats
        statArr(n) = 0.
      ENDDO

      IF ( exclSpVal ) THEN

       DO j = 1,jRun
        DO i = 1,iRun
          IF (arrMask(i,j).NE.0. .AND. inpArr(i,j).NE.specialVal) THEN
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = inpArr(i,j)
              statArr(ix) = inpArr(i,j)
            ELSE
              statArr(im) = MIN(inpArr(i,j),statArr(im))
              statArr(ix) = MAX(inpArr(i,j),statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*inpArr(i,j)
            statArr(2) = statArr(2) + tmpVol*inpArr(i,j)*inpArr(i,j)
          ENDIF
        ENDDO
       ENDDO

      ELSE

       DO j = 1,jRun
        DO i = 1,iRun
c         IF ( regMask(i,j).NE.0. .AND. arrMask(i,j).NE.0. ) THEN
          IF ( arrMask(i,j).NE.0. ) THEN
            IF ( statArr(0).EQ.0. ) THEN
              statArr(im) = inpArr(i,j)
              statArr(ix) = inpArr(i,j)
            ELSE
              statArr(im) = MIN(inpArr(i,j),statArr(im))
              statArr(ix) = MAX(inpArr(i,j),statArr(ix))
            ENDIF
            IF ( useWeight ) THEN
              tmpVol = arrDr*arrhFac(i,j)*arrArea(i,j)
            ELSE
              tmpVol = arrDr*arrArea(i,j)
            ENDIF
            statArr(0) = statArr(0) + tmpVol
            statArr(1) = statArr(1) + tmpVol*inpArr(i,j)
            statArr(2) = statArr(2) + tmpVol*inpArr(i,j)*inpArr(i,j)
          ENDIF
        ENDDO
       ENDDO

      ENDIF

      RETURN 
      END
