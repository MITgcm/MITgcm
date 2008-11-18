C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_local.F,v 1.7 2008/11/18 21:41:06 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_LOCAL
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_LOCAL(
     U                  statFld,
     I                  inpFld, frcFld,
     I                  scaleFact, power, useFract, sizF,
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
#include "DIAGSTATS_REGIONS.h"
#include "PARAMS.h"
#include "GRID.h"
c #include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     statFld     :: cumulative statistics array (updated)
C     inpFld      :: input field array to process (compute stats & add to statFld)
C     frcFld      :: fraction used for weighted-average diagnostics
C     scaleFact   :: scaling factor
C     power       :: option to fill-in with the field square (power=2)
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
      INTEGER power
      LOGICAL useFract
      INTEGER iRun, jRun, kIn, biIn, bjIn
      INTEGER k, bi, bj, ndId
      INTEGER region2fill(0:nRegions)
      CHARACTER*16 parsFld
      INTEGER myThid
CEOP

C     !FUNCTIONS:
#ifdef ALLOW_FIZHI
      _RL   getcon
      EXTERNAL getcon
#endif

C     !LOCAL VARIABLES:
C     i,j    :: loop indices
      INTEGER i, n, km, kFr, kRegMsk
      INTEGER im, ix, iv
      PARAMETER ( iv = nStats - 2 , im = nStats - 1 , ix = nStats )
      LOGICAL exclSpVal
      LOGICAL useWeight
      _RL statLoc(0:nStats)
      _RL drLoc
      _RL specialVal

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      useWeight = .FALSE.
      exclSpVal = .FALSE.
      specialVal = 0.
#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) THEN
        exclSpVal = .TRUE.
        specialVal = getcon('UNDEF')
      ENDIF
#endif
      kFr = MIN(kIn,sizF)

      DO n=0,nRegions
       IF (region2fill(n).NE.0) THEN
C---   Compute statistics for this tile, level and region:

         kRegMsk = diagSt_kRegMsk(n)

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
     I            scaleFact, power, useFract, n, diagSt_vRegMsk(n),
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            diagSt_regMask(1-Olx,1-Oly,kRegMsk,bi,bj),
     I            maskW(1-Olx,1-Oly,k,bi,bj),
     I            hFacW(1-Olx,1-Oly,k,bi,bj), rAw(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
c    I            drLoc, k,bi,bj, parsFld, myThid )
          ELSEIF ( parsFld(2:2).EQ.'V' ) THEN
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, power, useFract, n, diagSt_vRegMsk(n),
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            diagSt_regMask(1-Olx,1-Oly,kRegMsk,bi,bj),
     I            maskS(1-Olx,1-Oly,k,bi,bj),
     I            hFacS(1-Olx,1-Oly,k,bi,bj), rAs(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSE
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, power, useFract, n, diagSt_vRegMsk(n),
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            diagSt_regMask(1-Olx,1-Oly,kRegMsk,bi,bj),
     I            maskC(1-Olx,1-Oly,k,bi,bj),
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
     I            scaleFact, power, useFract, n, diagSt_vRegMsk(n),
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            diagSt_regMask(1-Olx,1-Oly,kRegMsk,bi,bj),
     I            maskH(1-Olx,1-Oly,bi,bj), rA(1-Olx,1-Oly,bi,bj),
     I            specialVal, exclSpVal,
     I            k,bi,bj, parsFld, myThid )
         ELSEIF ( useLand .AND.
     &           (parsFld(10:10).EQ.'G' .OR. parsFld(10:10).EQ.'g')
     &          ) THEN
           CALL  DIAGSTATS_G_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, power, useFract, n, diagSt_vRegMsk(n),
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            diagSt_regMask(1-Olx,1-Oly,kRegMsk,bi,bj),
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
     I            scaleFact, power, useFract, n, diagSt_vRegMsk(n),
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            diagSt_regMask(1-Olx,1-Oly,kRegMsk,bi,bj),
     I            maskW(1-Olx,1-Oly,km,bi,bj),
     I            maskW(1-Olx,1-Oly,km,bi,bj),rAw(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSEIF ( parsFld(2:2).EQ.'V' ) THEN
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, power, useFract, n, diagSt_vRegMsk(n),
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            diagSt_regMask(1-Olx,1-Oly,kRegMsk,bi,bj),
     I            maskS(1-Olx,1-Oly,km,bi,bj),
     I            maskS(1-Olx,1-Oly,km,bi,bj),rAs(1-Olx,1-Oly,bi,bj),
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )
          ELSE
           CALL  DIAGSTATS_CALC(
     O            statLoc,
     I            inpFld(sizI1,sizJ1,kIn,biIn,bjIn),
     I            frcFld(sizI1,sizJ1,kFr,biIn,bjIn),
     I            scaleFact, power, useFract, n, diagSt_vRegMsk(n),
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            diagSt_regMask(1-Olx,1-Oly,kRegMsk,bi,bj),
     I            maskH(1-Olx,1-Oly,bi,bj),
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

C---   processing region "n" ends here.
       ENDIF
      ENDDO

      RETURN
      END
