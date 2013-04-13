C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_bulkf.F,v 1.8 2013/04/13 20:51:32 heimbach Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_BULK_FORCE
#include "BULK_FORCE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_GET_BULKF
C     !INTERFACE:
      SUBROUTINE THSICE_GET_BULKF(
     I                  bi, bj,
     I                  iMin,iMax, jMin,jMax,
     I                  icFlag, hSnow, Tsf,
     O                  flxExcSw, dFlxdT, evap, dEvdT,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_GET_BULKF
C     *==========================================================*
C     | Interface S/R : get Surface Fluxes from pkg BULK_FORCE
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#ifdef ALLOW_BULK_FORCE
#include "EEPARAMS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     icFlag     :: sea-ice fractional mask [0-1]
C     icFlag     :: True= get fluxes at this location ; False= do nothing
C     hSnow       :: snow height [m]
C     Tsf         :: surface (ice or snow) temperature (oC)
C     flxExcSw    :: net (downward) surface heat flux, except short-wave [W/m2]
C     dFlxdT      :: deriv of flx with respect to Tsf    [W/m/K]
C     evap        :: surface evaporation (>0 if evaporate) [kg/m2/s]
C     dEvdT       :: deriv of evap. with respect to Tsf  [kg/m2/s/K]
C     myThid      :: Thread no. that called this routine.
      INTEGER bi, bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL     icFlag  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     hSnow   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     Tsf     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     flxExcSw(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dFlxdT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     evap    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dEvdT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
#ifdef ALLOW_BULK_FORCE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===
C     iceornot    :: 0=open water, 1=ice cover, 2=ice+snow
      INTEGER iceornot
      INTEGER i, j
      _RL  flwup         ! upward LW at surface (W m-2)
      _RL  flwNet_dwn    ! net (downward) LW at surface (W m-2)
      _RL  fsh           ! surface downward sensible heat (W m-2)
      _RL  flh           ! surface downward latent heat (W m-2)
      _RL  ust, vst, ssq
#ifdef ALLOW_FORMULA_AIM
      _RL     Tsurf(1), SHF(1), EVPloc(1), SLRU(1)
      _RL     dEvp(1), sFlx(0:2)
#endif

      DO j=jMin,jMax
       DO i=iMin,iMax
        IF ( icFlag(i,j).GT.0. _d 0 ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

         IF ( hSnow(i,j).GT.3. _d -1 ) THEN
          iceornot=2
         ELSE
          iceornot=1
         ENDIF

#ifdef ALLOW_FORMULA_AIM
         IF ( useFluxFormula_AIM ) THEN

          Tsurf(1) = Tsf(i,j)
          CALL BULKF_FORMULA_AIM(
     I               Tsurf, flwdwn(i,j,bi,bj),
     I               ThAir(i,j,bi,bj), Tair(i,j,bi,bj),
     I               Qair(i,j,bi,bj), wspeed(i,j,bi,bj),
     O               SHF, EVPloc, SLRU,
     O               dEvp, sFlx,
     I               iceornot, myThid )

          flxExcSw(i,j) = sFlx(1)
          dFlxdT(i,j)   = sFlx(2)
C-      convert from [g/m2/s] to [kg/m2/s]
          evap(i,j)  = EVPloc(1) * 1. _d -3
          dEvdT(i,j) = dEvp(1)   * 1. _d -3

         ELSE
#else  /* ALLOW_FORMULA_AIM */
         IF ( .TRUE. ) THEN
#endif /* ALLOW_FORMULA_AIM */

          ust = 0.
          vst = 0.
          ssq = 0.

          IF ( blk_nIter.EQ.0 ) THEN
           CALL BULKF_FORMULA_LANL(
     I          uwind(i,j,bi,bj), vwind(i,j,bi,bj), wspeed(i,j,bi,bj),
     I          Tair(i,j,bi,bj), Qair(i,j,bi,bj),
     I          cloud(i,j,bi,bj), Tsf(i,j),
     O          flwup, flh, fsh, dFlxdT(i,j), ust, vst,
     O          evap(i,j), ssq, dEvdT(i,j),
     I          iceornot, myThid )
          ELSE
           CALL BULKF_FORMULA_LAY(
     I          uwind(i,j,bi,bj), vwind(i,j,bi,bj), wspeed(i,j,bi,bj),
     I          Tair(i,j,bi,bj), Qair(i,j,bi,bj), Tsf(i,j),
     O          flwup, flh, fsh, dFlxdT(i,j), ust, vst,
     O          evap(i,j), ssq, dEvdT(i,j),
     I          iceornot, i,j,bi,bj,myThid )
          ENDIF

          flwNet_dwn = flwdwn(i,j,bi,bj) - flwup
          flxExcSw(i,j) = flwNet_dwn + fsh + flh

         ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_BULK_FORCE */
#endif /* ALLOW_THSICE */

      RETURN
      END
