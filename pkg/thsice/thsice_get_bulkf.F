C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_bulkf.F,v 1.4 2006/05/25 17:36:44 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_BULK_FORCE
#include "BULK_FORCE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_GET_BULKF
C     !INTERFACE:
      SUBROUTINE THSICE_GET_BULKF(
     I                         iceornot, Tsf,
     O                         flxExceptSw, df0dT, evap, dEvdT,
     I                         i,j,bi,bj,myThid )
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
#ifdef ALLOW_BULK_FORCE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     iceornot    :: 0=open water, 1=ice cover
C     Tsf         :: surface (ice or snow) temperature (oC)
C     flxExceptSw :: net (downward) surface heat flux, except short-wave [W/m2]
C     df0dT       :: deriv of flx with respect to Tsf    [W/m/K]
C     evap        :: surface evaporation (>0 if evaporate) [kg/m2/s]
C     dEvdT       :: deriv of evap. with respect to Tsf  [kg/m2/s/K]
C     i,j, bi,bj  :: current grid point indices
C     myThid      :: Thread no. that called this routine.
      INTEGER i,j, bi,bj
      INTEGER myThid
      INTEGER iceornot
      _RL  Tsf
      _RL  flxExceptSw
      _RL  df0dT
      _RL  evap
      _RL  dEvdT
CEOP

#ifdef ALLOW_THSICE
#ifdef ALLOW_BULK_FORCE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===

      _RL  flwup         ! upward LW at surface (W m-2)
      _RL  flwNet_dwn    ! net (downward) LW at surface (W m-2)
      _RL  fsh           ! surface downward sensible heat (W m-2)
      _RL  flh           ! surface downward latent heat (W m-2)
      _RL  ust, vst, ssq
#ifdef ALLOW_FORMULA_AIM
      _RL     Tsurf(1), SHF(1), EVPloc(1), SLRU(1)
      _RL     dEvp(1), sFlx(0:2)
#endif

#ifdef ALLOW_FORMULA_AIM
      IF ( useFluxFormula_AIM ) THEN

        Tsurf(1) = Tsf
        CALL BULKF_FORMULA_AIM(
     I             Tsurf, flwdwn(i,j,bi,bj),
     I             ThAir(i,j,bi,bj), Tair(i,j,bi,bj),
     I             Qair(i,j,bi,bj), wspeed(i,j,bi,bj),
     O             SHF, EVPloc, SLRU,
     O             dEvp, sFlx,
     I             iceornot, myThid )

        flxExceptSw = sFlx(1)
        df0dT = sFlx(2)
C-      convert from [g/m2/s] to [kg/m2/s]
        evap  = EVPloc(1) * 1. _d -3
        dEvdT = dEvp(1)   * 1. _d -3

      ELSE
#else  /* ALLOW_FORMULA_AIM */
      IF ( .TRUE. ) THEN
#endif /* ALLOW_FORMULA_AIM */

        ust = 0.
        vst = 0.
        ssq = 0.

        IF ( blk_nIter.EQ.0 ) THEN
         CALL BULKF_FORMULA_LANL(
     I        uwind(i,j,bi,bj), vwind(i,j,bi,bj), wspeed(i,j,bi,bj),
     I        Tair(i,j,bi,bj), Qair(i,j,bi,bj), cloud(i,j,bi,bj), Tsf,
     O        flwup, flh, fsh, df0dT, ust, vst, evap, ssq, dEvdT,
     I        iceornot, myThid )
        ELSE
         CALL BULKF_FORMULA_LAY(
     I        uwind(i,j,bi,bj), vwind(i,j,bi,bj), wspeed(i,j,bi,bj),
     I        Tair(i,j,bi,bj), Qair(i,j,bi,bj), Tsf,
     O        flwup, flh, fsh, df0dT, ust, vst, evap, ssq, dEvdT,
     I        iceornot, i,j,bi,bj,myThid )
        ENDIF

        flwNet_dwn = flwdwn(i,j,bi,bj) - flwup
        flxExceptSw = flwNet_dwn + fsh + flh

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_BULK_FORCE */
#endif /* ALLOW_THSICE */

      RETURN
      END
