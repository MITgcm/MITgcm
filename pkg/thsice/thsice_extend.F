#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_EXTEND
C     !INTERFACE:
      SUBROUTINE THSICE_EXTEND(
     I                  bi, bj,
     I                  iMin,iMax, jMin,jMax, dBugFlag,
     I                  fzMlOc, tFrz, tOce,
     U                  icFrac, hIce, hSnow,
     U                  tSrf, tIc1, tIc2, qIc1, qIc2,
     O                  flx2oc, frw2oc, fsalt,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_EXTEND
C     | o Extend sea-ice area incresing ice fraction
C     *==========================================================*
C     | o incorporate surplus of energy to
C     |   make new ice or make ice grow laterally
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     dBugFlag    :: allow to print debugging stuff (e.g. on 1 grid point).
C---  Input:
C         iceMask :: sea-ice fractional mask [0-1]
C  fzMlOc (esurp) :: ocean mixed-layer freezing/melting potential [W/m2]
C  tFrz    (Tf)   :: sea-water freezing temperature [oC] (function of S)
C  tOce           :: surface level oceanic temperature [oC]
C---  Modified (input&output):
C  icFrac(iceFrac):: fraction of grid area covered in ice
C  hIce (iceThick):: ice height [m]
C  hSnow          :: snow height [m]
C  tSrf           :: surface (ice or snow) temperature [oC]
C  tIc1           :: temperature of ice layer 1 [oC]
C  tIc2           :: temperature of ice layer 2 [oC]
C  qIc1   (qicen) :: ice enthalpy (J/kg), 1rst level
C  qIc2   (qicen) :: ice enthalpy (J/kg), 2nd level
C---  Output
C  flx2oc   (=)   :: (additional) heat flux to ocean    [W/m2]        (+=dwn)
C  frw2oc   (=)   :: (additional) fresh water flux to ocean [kg/m2/s] (+=dwn)
C  fsalt    (=)   :: (additional) salt flux to ocean        [g/m2/s]  (+=dwn)
C---  Input:
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      LOGICAL dBugFlag
c     _RL iceMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fzMlOc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFrz   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tOce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL icFrac (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hSnow  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tSrf   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tIc1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tIc2   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qIc1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qIc2   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flx2oc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL frw2oc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fsalt  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C---  local copy of input/output argument list variables (see description above)
      _RL esurp
      _RL Tf
      _RL iceFrac
      _RL iceThick
      _RL qicen(nlyr)

C     == Local variables ==
C     iceVol    :: previous ice volume
C     newIce    :: new ice volume to produce
C     hNewIce   :: thickness of new ice to form
C     iceFormed :: ice-volume formed (new ice volume = iceVol+iceFormed )
C     qicAv     :: mean enthalpy of ice (layer 1 & 2) [J/m^3]
      _RL deltaTice ! time-step for ice model
      _RL iceVol
      _RL newIce
      _RL hNewIce
      _RL iceFormed
      _RL qicAv
      INTEGER  i,j     ! loop indices

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey
#endif
C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

#ifdef ALLOW_DBUG_THSICE
 1020 FORMAT(A,1P4E11.3)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      deltaTice = thSIce_deltaT

#ifdef ALLOW_AUTODIFF
      DO j = 1-OLy, sNy+OLy
       DO i = 1-OLx, sNx+OLx
         flx2oc(i,j) = 0. _d 0
         frw2oc(i,j) = 0. _d 0
         fsalt (i,j) = 0. _d 0
       ENDDO
      ENDDO
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ticekey = (act1 + 1) + act2*max1
     &                     + act3*max1*max2
     &                     + act4*max1*max2*max3
CADJ STORE hIce   = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hSnow  = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE icFrac = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE qIc1   = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE qIc2   = comlev1_bibj, key = ticekey, kind = isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax

        IF (fzMlOc(i,j).GT.0. _d 0) THEN
         esurp     = fzMlOc(i,j)
         Tf        = tFrz(i,j)
         iceFrac   = icFrac(i,j)
         iceThick  = hIce(i,j)
         qicen(1)  = qIc1(i,j)
         qicen(2)  = qIc2(i,j)
C---
C--   start ice
         iceFormed = 0. _d 0
         iceVol    = iceFrac*iceThick

C-    enthalpy of new ice to form :
         IF ( iceFrac.LE.0. _d 0 ) THEN
          qicen(1) = -cpWater*Tmlt1
     &              + cpIce *(Tmlt1-Tf) + Lfresh*(1. _d 0-Tmlt1/Tf)
          qicen(2) = -cpIce *Tf + Lfresh
         ENDIF
         qicAv     = rhoi*(qicen(1)+qicen(2))*0.5 _d 0
         newIce    = esurp*deltaTice/qicAv

         IF ( icFrac(i,j).EQ.0. _d 0 ) THEN
C-  to keep identical results (as it used to be):
c-old_v: IF ( newIce.GE.hThinIce*iceMaskMin ) THEN
C-  here we allow to form ice earlier (as soon as min-ice-vol is reached)
c-new_v:
          IF ( newIce.GT.hIceMin*iceMaskMin ) THEN
C-    if there is no ice in grid-cell and enough ice to form:
C-    make ice over iceMaskMin fraction, up to hThinIce,
C     and if more ice to form, then increase fraction
           iceThick  = MIN(hThinIce,newIce/iceMaskMin)
           iceThick  = MAX(iceThick,newIce/iceMaskMax)
           iceFrac   = newIce/iceThick
           iceFormed = newIce
          ENDIF
         ELSEIF ( iceVol.LT.hiMax*iceMaskMax ) THEN
C-    if there is already some ice
C     create ice with same thickness or hNewIceMax (the smallest of the 2)
           hNewIce = MIN(hIce(i,j),hNewIceMax)
           iceFrac = MIN(icFrac(i,j)+newIce/hNewIce,iceMaskMax)
C-    update thickness: area weighted average
c-new_v:
           iceThick = MIN(hiMax,(iceVol+newIce)/iceFrac)
C-  to keep identical results: comment the line above and uncomment line below:
c-old_v     iceFrac = MIN(icFrac(i,j)+newIce/iceThick,iceMaskMax)
           iceFormed = iceThick*iceFrac - iceVol
C-    spread snow out over ice
           hSnow(i,j) = hSnow(i,j)*icFrac(i,j)/iceFrac
         ENDIF
C-    oceanic fluxes:
         flx2oc(i,j) = qicAv*iceFormed/deltaTice
         frw2oc(i,j) = -rhoi*iceFormed/deltaTice
         fsalt (i,j) = -(rhoi*saltIce)*iceFormed/deltaTice

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_DBUG_THSICE
         IF ( dBug(i,j,bi,bj) ) THEN
          WRITE(6,1020) 'ThSI_EXT: iceH, newIce, newIceFrac=',
     &                       iceThick, newIce, iceFrac-icFrac(i,j)
          WRITE(6,1020) 'ThSI_EXT: iceFrac,flx2oc,fsalt,frw2oc=',
     &                  iceFrac,flx2oc(i,j),fsalt(i,j),frw2oc(i,j)
         ENDIF
#endif
#ifdef CHECK_ENERGY_CONSERV
         CALL THSICE_CHECK_CONSERV( dBugFlag, i, j, bi, bj, 1,
     I            icFrac(i,j), iceFrac, iceThick, hSnow(i,j), qicen,
     I            flx2oc(i,j), frw2oc(i,j), fsalt(i,j),
     I            myTime, myIter, myThid )
#endif /* CHECK_ENERGY_CONSERV */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update Sea-Ice state output:
         IF ( iceFrac.GT.0. _d 0 .AND. icFrac(i,j).EQ.0. _d 0) THEN
c         hSnow(i,j) = 0. _d 0
          tSrf(i,j)  = tFrz(i,j)
          tIc1(i,j)  = tFrz(i,j)
          tIc2(i,j)  = tFrz(i,j)
          qIc1(i,j)  = qicen(1)
          qIc2(i,j)  = qicen(2)
         ENDIF
         icFrac(i,j) = iceFrac
         hIce  (i,j) = iceThick
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_THSICE */

      RETURN
      END
