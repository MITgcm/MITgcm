C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_others_calc.F,v 1.1 2006/01/23 22:31:11 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C--  File diagstats_others_calc.F: Routines to calculate regional statistics
C                                  and dealing with special type of fields
C--   o DIAGSTATS_LM_CALC :: for fields on FIZHI-grid (parse(10)='L' or 'M')
C--   o DIAGSTATS_G_CALC  :: for land-type fields     (parse(10)='G')

CBOP
C     !ROUTINE: DIAGSTATS_LM_CALC
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_LM_CALC(
     O                  statArr,
     I                  inpArr, frcArr, scaleFact, power, useFract,
     I                  regId, regMskVal,
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
C     scaleFact   :: scaling factor
C     power       :: option to fill-in with the field square (power=2)
C     useFract    :: if True, use fraction-weight
C     regId       :: region number Id
C     regMskVal   :: region-mask identificator value
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
      INTEGER power
      LOGICAL useFract
      INTEGER regId
      _RS regMskVal
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
     I            inpArr, frcArr, scaleFact, power, useFract,
     I            regId, regMskVal,
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
     I                  inpArr, frcArr, scaleFact, power, useFract,
     I                  regId, regMskVal,
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
C     scaleFact   :: scaling factor
C     power       :: option to fill-in with the field square (power=2)
C     useFract    :: if True, use fraction-weight
C     regId       :: region number Id
C     regMskVal   :: region-mask identificator value
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
      INTEGER power
      LOGICAL useFract
      INTEGER regId
      _RS regMskVal
      _RS regMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
     I            inpArr, frcArr, scaleFact, power, useFract,
     I            regId, regMskVal,
     I            nStats,sizI1,sizI2,sizJ1,sizJ2,iRun,jRun,
     I            regMask, aim_landFr(1-Olx,1-Oly,bi,bj),
     I            aim_landFr(1-Olx,1-Oly,bi,bj), arrArea,
     I            drLoc, specialVal, exclSpVal, useWeight, myThid )

c     ENDIF
#endif /* ALLOW_LAND */

      RETURN
      END
