#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
# ifdef ALLOW_EXF
#  include "EXF_OPTIONS.h"
# endif
#endif

CBOP
C     !ROUTINE: THSICE_MAIN
C     !INTERFACE:
      SUBROUTINE THSICE_MAIN(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_MAIN
C     | o Therm_SeaIce main routine.
C     |   step forward Thermodynamic_SeaIce variables and modify
C     |    ocean surface forcing accordingly.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_VARS.h"
#ifdef ALLOW_AUTODIFF
# include "THSICE_COST.h"
# include "DYNVARS.h"
# ifdef ALLOW_EXF
#  include "EXF_PARAM.h"
#  include "EXF_CONSTANTS.h"
#  include "EXF_FIELDS.h"
# endif /* ALLOW_EXF */
#endif /* ALLOW_AUTODIFF */
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     prcAtm    :: total precip from the atmosphere [kg/m2/s]
C     snowPr    :: snow precipitation               [kg/m2/s]
C     qPrcRn    :: Energy content of Precip+RunOff (+=down) [W/m2]
      INTEGER i,j
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL snowPr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qPrcRn(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL evpAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL flxAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL flxSW (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tauFac
#ifdef ALLOW_EXF
      INTEGER grpDiag
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_EXF
      IF ( useEXF .AND. useDiagnostics ) THEN
C-    Fill-in EXF surface flux diags, weighted by open-ocean fraction
        grpDiag = 2
        IF ( thSIce_skipThermo ) grpDiag = -2
        CALL EXF_WEIGHT_SFX_DIAGS(
     I                  iceMask, grpDiag, myTime, myIter, myThid )
        IF ( .NOT.useSEAICE ) CALL EXF_WEIGHT_SFX_DIAGS(
     I                       iceMask, -1, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_EXF */

C-    Only compute/update seaice fields over the interior
C     (excluding overlap) and apply exchanges when needed
      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        max3 = nTx*nTy
        act4 = ikey_dynamics - 1
        ticekey = (act1 + 1) + act2*max1
     &                       + act3*max1*max2
     &                       + act4*max1*max2*max3
CADJ STORE uvel  (:,:,1,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE vvel  (:,:,1,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE qsw     (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE Qice1   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE Qice2   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE sHeating(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE Tsrf    (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
#endif

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          prcAtm  (i,j,bi,bj) = 0. _d 0
          snowPr  (i,j) = 0. _d 0
          qPrcRn  (i,j) = 0. _d 0
         ENDDO
        ENDDO

#ifndef ALLOW_AUTODIFF
        IF ( .NOT.useCheapAML ) THEN
#endif
         CALL THSICE_GET_OCEAN(
     I                          bi, bj, myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceMask   (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE iceHeight (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE snowHeight(:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE snowAge   (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE hOceMxL   (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE tOceMxL   (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE sOceMxL   (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE v2ocMxL   (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifndef ALLOW_AUTODIFF
C-   end if not useCheapAML
        ENDIF
#endif

#ifndef ALLOW_AUTODIFF
        IF ( useBulkforce .OR. useCheapAML ) THEN
         CALL THSICE_GET_PRECIP(
     I                  iceMask, tOceMxL,
     O                  prcAtm(1-OLx,1-OLy,bi,bj),
     O                  snowPr, qPrcRn,
     O                  icFlxSW(1-OLx,1-OLy,bi,bj),
     I                  iMin,iMax,jMin,jMax, bi,bj, myThid )
        ENDIF
#endif
        IF ( useEXF ) THEN
         CALL THSICE_MAP_EXF(
     I                  iceMask, tOceMxL,
     O                  prcAtm(1-OLx,1-OLy,bi,bj),
     O                  snowPr, qPrcRn,
     O                  icFlxSW(1-OLx,1-OLy,bi,bj),
     I                  iMin,iMax,jMin,jMax, bi,bj, myThid )
        ENDIF

#ifndef ALLOW_AUTODIFF
        IF ( .NOT.( useCheapAML .OR. thSIce_skipThermo ) ) THEN
#endif
         CALL THSICE_STEP_TEMP(
     I                     bi, bj, iMin, iMax, jMin, jMax,
     I                     myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
C     this store directives avoid recomputation of this routine in ad-code
CADJ STORE flxCndBt(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE EmPmR   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE Qnet    (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE snowAge (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE sHeating(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE snowPr              = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE Qice1   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE Qice2   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE icFrwAtm(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
C     This part of the state is generally not necessary, but you never
C     know so I leave it commented out.
cCADJ STORE Qsw     (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE icFlxSW (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE Tice1   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE Tice2   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE sHeating(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE Tsrf    (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE icFlxAtm(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
#endif

#ifndef ALLOW_AUTODIFF
C-   end if not skipThermo / useCheapAML
        ENDIF
        IF ( .NOT.thSIce_skipThermo ) THEN
#endif
         CALL THSICE_STEP_FWD(
     I                     bi, bj, iMin, iMax, jMin, jMax,
     I                     prcAtm(1-OLx,1-OLy,bi,bj),
     I                     snowPr, qPrcRn,
     I                     myTime, myIter, myThid )
#ifndef ALLOW_AUTODIFF
        ELSE
C-    Compute sIceLoad (if not previously done)
          DO j=1,sNy
           DO i=1,sNx
             sIceLoad(i,j,bi,bj) = ( snowHeight(i,j,bi,bj)*rhos
     &                             + iceHeight(i,j,bi,bj)*rhoi
     &                             )*iceMask(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
#endif

C--  end bi,bj loop
       ENDDO
      ENDDO

#ifdef ALLOW_BALANCE_FLUXES
C--   Balance net Fresh-Water flux from Atm+Land
      IF ( thSIceBalanceAtmFW.NE.0 ) THEN
        CALL THSICE_BALANCE_FRW(
     I                      iMin, iMax, jMin, jMax,
     I                      prcAtm, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_BALANCE_FLUXES */

C     add a small piece of code to check AddFluid implementation:
c#include "thsice_test_addfluid.h"

C--   Exchange fields that are advected by seaice dynamics
      IF ( useSEAICE .OR. thSIceAdvScheme.GT.0
     &               .OR. stressReduction.GT.zeroRL ) THEN
        CALL THSICE_DO_EXCH( myThid )
      ENDIF
      IF ( thSIceAdvScheme.GT.0 .AND. .NOT.useSEAICE ) THEN
C-    when useSEAICE=.true., this S/R is called from SEAICE_MODEL;
C     otherwise, call it from here, after thsice-thermodynamics is done
         CALL THSICE_DO_ADVECT(
     I                          0, 0, myTime, myIter, myThid )
      ELSEIF ( thSIceAdvScheme.LE.0 .AND. useRealFreshWaterFlux ) THEN
        _EXCH_XY_RS( sIceLoad, myThid )
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        max3 = nTx*nTy
        act4 = ikey_dynamics - 1
        ticekey = (act1 + 1) + act2*max1
     &                       + act3*max1*max2
     &                       + act4*max1*max2*max3
CADJ STORE iceMask(:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE fu     (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE fv     (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Cumulate time-averaged fields and also fill-up flux diagnostics
C     (if not done in THSICE_DO_ADVECT call)
        IF ( thSIceAdvScheme.LE.0 ) THEN
         CALL THSICE_AVE(
     I                     bi,bj, myTime, myIter, myThid )
        ENDIF
C--   note: If useSEAICE=.true., the stress is computed in seaice_model,
C--   and stressReduction is always set to zero
        IF ( stressReduction.GT. 0. _d 0 ) THEN
          DO j = 1-OLy,sNy+OLy-1
           DO i = 2-OLx,sNx+OLx-1
            tauFac = stressReduction
     &             *(iceMask(i-1,j,bi,bj)+iceMask(i,j,bi,bj))*0.5 _d 0
            fu(i,j,bi,bj) = (1. _d 0 - tauFac)*fu(i,j,bi,bj)
           ENDDO
          ENDDO
          DO j = 2-OLy,sNy+OLy-1
           DO i = 1-OLx,sNx+OLx-1
            tauFac = stressReduction
     &             *(iceMask(i,j-1,bi,bj)+iceMask(i,j,bi,bj))*0.5 _d 0
            fv(i,j,bi,bj) = (1. _d 0 - tauFac)*fv(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF

C--  end bi,bj loop
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_THSICE */

      RETURN
      END
