C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_bulkf.F,v 1.1 2003/11/23 01:20:13 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      SUBROUTINE THSICE_GET_BULKF(
     I                         iceornot, Tsf, 
     O                         flxExceptSw, df0dT, evap,
     I                         i,j,bi,bj,myThid )
C     *==========================================================*
C     | S/R  THSICE_GET_BULKF                                       
C     *==========================================================*
C     | Interface S/R : get Surface Fluxes from pkg BULK_FORCE
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#ifdef ALLOW_BULK_FORCE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "BULKF.h"
#endif

C     === Routine arguments ===
C     iceornot    :: 0=open water, 1=ice cover
C     Tsf         :: surface (ice or snow) temperature
C     flxExceptSw :: net (downward) surface heat flux, except short-wave (W/m2)
C     df0dT       :: deriv of flx wrt Tsf (W m-2 deg-1)
C     evap        :: -surface evaporation (<0 if evaporate)
C     i,j, bi,bj  :: current grid point indices
C     myThid      :: Thread no. that called this routine.
      INTEGER i,j, bi,bj
      INTEGER myThid
      INTEGER iceornot
      _RL  Tsf
      _RL  flxExceptSw
      _RL  df0dT
      _RL  evap

#ifdef ALLOW_THSICE
#ifdef ALLOW_BULK_FORCE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===

      _RL  flwup         ! upward LW at surface (W m-2)
      _RL  flwNet_dwn    ! net (downward) LW at surface (W m-2)
      _RL  fsh           ! surface downward sensible heat (W m-2)
      _RL  flh           ! surface downward latent heat (W m-2)
      _RL  ust, vst, ssq

      ust = 0.
      vst = 0. 
      ssq = 0. 

      CALL BULKF_FORMULA_LANL(uwind(i,j,bi,bj), vwind(i,j,bi,bj),
     &        wspeed(i,j,bi,bj),
     &        Tair(i,j,bi,bj), Qair(i,j,bi,bj), cloud(i,j,bi,bj), Tsf,
     &        flwup, flh, fsh, df0dT, ust, vst, evap, ssq, iceornot,
     &        readwindstress)

      flwNet_dwn = flw(i,j,bi,bj) - flwup
      flxExceptSw = flwNet_dwn + fsh + flh

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_BULK_FORCE */
#endif /* ALLOW_THSICE */

      RETURN
      END
