C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_suflux_ocean.F,v 1.1 2004/03/11 14:33:19 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: SUFLUX_OCEAN
C     !INTERFACE:
      SUBROUTINE SUFLUX_OCEAN(
     I                   PSA, FMASK,
     I                   Tsurf, SSR, SLRD,
     I                   T0, Q0, CDENVV,
     O                   SHF, EVAP, SLRU,
     I                   bi,bj,myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SUFLUX_OCEAN
C     | o compute surface flux over ocean
C     *==========================================================*
C     | o contains part of original S/R SUFLUX (Speedy code)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"
#include "EEPARAMS.h"

C     Physical constants + functions of sigma and latitude
#include "com_physcon.h"

C     Surface flux constants
#include "com_sflcon.h"

C     == Routine Arguments ==
C--   Input:
C    FMASK  :: fractional land-sea mask        (2-dim)
C    Tsurf  :: surface temperature        (2-dim)
C    SSR    :: sfc sw radiation (net flux)     (2-dim)
C    SLRD   :: sfc lw radiation (downward flux)(2-dim)
C    T0     :: near-surface air temperature    (2-dim)
C    Q0     :: near-surface sp. humidity [g/kg](2-dim)
C    CDENVV :: sensible heat flux coefficient  (2-dim)
C--   Output:
C    SHF    :: sensible heat flux              (2-dim)
C    EVAP   :: evaporation [g/(m^2 s)]         (2-dim)
C    SLRU   :: sfc lw radiation (upward flux)  (2-dim)
C--   Input:
C    bi,bj  :: tile index
C    myThid :: Thread number for this instance of the routine
C--
      _RL  PSA(NGP)
      _RL  FMASK(NGP), Tsurf(NGP)
      _RL  SSR(NGP), SLRD(NGP), T0(NGP), Q0(NGP), CDENVV(NGP)

      _RL  SHF(NGP), EVAP(NGP), SLRU(NGP)

      INTEGER bi,bj,myThid
CEOP

#ifdef ALLOW_AIM

C-- Local variables:
      _RL QSAT0(NGP)
      _RL QDUMMY(1), RDUMMY(1), TS4
      INTEGER J

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   2. Computation of fluxes over land and sea

C     2.1 Wind stress

C     2.2 Sensible heat flux (from clim. TS over land)

      DO J=1,NGP
        SHF(J) = CDENVV(J)*CP*(Tsurf(J)-T0(J))
      ENDDO

C     2.3 Evaporation

      CALL SHTORH (0, NGP, Tsurf, PSA, 1. _d 0, QDUMMY, RDUMMY,
     &                QSAT0, myThid)

      DO J=1,NGP
        EVAP(J) = CDENVV(J)*(QSAT0(J)-Q0(J))
      ENDDO

C     2.4 Emission of lw radiation from the surface

      DO J=1,NGP
        TS4     = Tsurf(J)**4
        SLRU(J) = SBC*TS4
      ENDDO

C--   3. Adjustment of skin temperature and fluxes over land
C--      based on energy balance (to be implemented)
C        <= done separately for each surface type (land,sea,se-ice)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */

      RETURN
      END
