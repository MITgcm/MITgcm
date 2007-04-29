C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_extend.F,v 1.6 2007/04/29 23:48:44 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_EXTEND
C     !INTERFACE:
      SUBROUTINE THSICE_EXTEND(
     I                  bi, bj, siLo, siHi, sjLo, sjHi,
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
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "SIZE.h"
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     siLo,siHi   :: size of input/output array: 1rst dim. lower,higher bounds
C     sjLo,sjHi   :: size of input/output array: 2nd  dim. lower,higher bounds
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     dBugFlag    :: allow to print debugging stuff (e.g. on 1 grid point).
C---  Input:
C         iceMask :: sea-ice fractional mask [0-1]
C  fzMlOc (esurp) :: ocean mixed-layer freezing/melting potential [W/m2]
C  tFrz    (Tf)   :: sea-water freezing temperature [oC] (function of S)
C  tOce    (sst)  :: surface level oceanic temperature [oC]
C---  Modified (input&output):
C  icFrac(iceFrac):: fraction of grid area covered in ice
C  hIce (iceThick):: ice height [m]
C hSnow(snowThick):: snow height [m]
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
      INTEGER siLo, siHi, sjLo, sjHi
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      LOGICAL dBugFlag
c     _RL iceMask(siLo:siHi,sjLo:sjHi)
      _RL fzMlOc (siLo:siHi,sjLo:sjHi)
      _RL tFrz   (siLo:siHi,sjLo:sjHi)
      _RL tOce   (siLo:siHi,sjLo:sjHi)
      _RL icFrac (siLo:siHi,sjLo:sjHi)
      _RL hIce   (siLo:siHi,sjLo:sjHi)
      _RL hSnow  (siLo:siHi,sjLo:sjHi)
      _RL tSrf   (siLo:siHi,sjLo:sjHi)
      _RL tIc1   (siLo:siHi,sjLo:sjHi)
      _RL tIc2   (siLo:siHi,sjLo:sjHi)
      _RL qIc1   (siLo:siHi,sjLo:sjHi)
      _RL qIc2   (siLo:siHi,sjLo:sjHi)
      _RL flx2oc (siLo:siHi,sjLo:sjHi)
      _RL frw2oc (siLo:siHi,sjLo:sjHi)
      _RL fsalt  (siLo:siHi,sjLo:sjHi)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C---  local copy of input/output argument list variables (see description above)
      _RL esurp
      _RL Tf
      _RL sst
      _RL iceFrac
      _RL iceThick
      _RL snowThick
      _RL qicen(nlyr)

C     == Local variables ==
C     qicAv    :: mean enthalpy of ice (layer 1 & 2) [J/m^3]
      _RL deltaTice ! time-step for ice model
      _RL newIce
      _RL newIceFrac
      _RL qicAv
      INTEGER  i,j     ! loop indices

C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1010 FORMAT(A,I3,3F8.3)
 1020 FORMAT(A,1P4E11.3)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
#endif /* ALLOW_AUTODIFF_TAMC */

      deltaTice = thSIce_deltaT

      DO j = jMin, jMax
       DO i = iMin, iMax
#ifdef ALLOW_AUTODIFF_TAMC
          ikey_1 = i
     &         + sNx*(j-1)
     &         + sNx*sNy*act1
     &         + sNx*sNy*max1*act2
     &         + sNx*sNy*max1*max2*act3
     &         + sNx*sNy*max1*max2*max3*act4
#endif /* ALLOW_AUTODIFF_TAMC */
C--
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hice(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE hsnow(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE icfrac(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE qic1(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE qic2(i,j) = comlev1_thsice_1, key=ikey_1
#endif

        IF (fzMlOc(i,j).GT.0. _d 0) THEN
          esurp   = fzMlOc(i,j)
          Tf      = tFrz(i,j)
          sst     = tOce(i,j)
          iceFrac = icFrac(i,j)
          iceThick= hIce(i,j)
          snowThick=hSnow(i,j)
          qicen(1)= qIc1(i,j)
          qicen(2)= qIc2(i,j)
C---
C--   start ice
        newIceFrac = 0. _d 0

C-    enthalpy of new ice to form :
        IF ( iceFrac.LE.0. _d 0 ) THEN
          qicen(1)= -cpWater*Tmlt1
     &             + cpIce *(Tmlt1-Tf) + Lfresh*(1. _d 0-Tmlt1/Tf)
          qicen(2)= -cpIce *Tf + Lfresh
        ENDIF
        qicAv = rhoi*(qicen(1)+qicen(2))*0.5 _d 0
        newIce = esurp*deltaTice/qicAv

        IF (icFrac(i,j).EQ.0. _d 0) THEN
c         IF (newIce.GE.hIceMin*iceMaskMax) THEN
C- jmc: above is the original version, but below seems more logical:
          IF (newIce.GE.hThinIce*iceMaskMin) THEN
C-    if there is no ice in grid and enough ice to form:
            iceThick   = MAX(hThinIce,newIce/iceMaskMax)
            newIceFrac = MIN(newIce/hThinIce,iceMaskMax)
            iceFrac = newIceFrac
            sst=Tf
          ENDIF
        ELSE
C-    if there is already some ice
            newIceFrac=MIN(newIce/iceThick,iceMaskMax-icFrac(i,j))
            iceFrac = icFrac(i,j) + newIceFrac
C-    spread snow out over ice
            snowThick = snowThick*icFrac(i,j)/iceFrac
            sst=(1. _d 0-newIceFrac)*sst+newIceFrac*Tf
        ENDIF
C-    oceanic fluxes:
        flx2oc(i,j)= iceThick*newIceFrac*qicAv/deltaTice
        frw2oc(i,j)=-(rhoi*iceThick)*newIceFrac/deltaTice
        fsalt(i,j)= -(rhoi*iceThick*saltIce)*newIceFrac/deltaTice

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_DBUG_THSICE
        IF ( dBug(i,j,bi,bj) ) THEN
          WRITE(6,1020) 'ThSI_EXT: iceH, newIce, newIceFrac=',
     &                             iceThick, newIce, newIceFrac
          WRITE(6,1020) 'ThSI_EXT: iceFrac,flx2oc,fsalt,frw2oc=',
     &                  iceFrac,flx2oc(i,j),fsalt(i,j),frw2oc(i,j)
        ENDIF
#endif
#ifdef CHECK_ENERGY_CONSERV
          CALL THSICE_CHECK_CONSERV( dBugFlag, i, j, bi, bj, 1,
     I            icFrac(i,j), iceFrac, iceThick, snowThick, qicen,
     I            flx2oc(i,j), frw2oc(i,j), fsalt(i,j),
     I            myTime, myIter, myThid )
#endif /* CHECK_ENERGY_CONSERV */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update Sea-Ice state output:
          IF ( iceFrac.GT.0. _d 0 .AND. icFrac(i,j).EQ.0. _d 0) THEN
            tSrf(i,j)   = tFrz(i,j)
            tIc1(i,j)   = tFrz(i,j)
            tIc2(i,j)   = tFrz(i,j)
            qIc1(i,j)   = qicen(1)
            qIc2(i,j)   = qicen(2)
          ENDIF
          icFrac(i,j) = iceFrac
          hIce(i,j)   = iceThick
          hSnow(i,j ) = snowThick
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_THSICE */

      RETURN
      END
