C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_suflux_land.F,v 1.4 2004/07/22 23:01:05 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: SUFLUX_LAND
C     !INTERFACE:
      SUBROUTINE SUFLUX_LAND(
     I                   PSA, FMASK, EMISloc,
     I                   Tsurf, dTskin, SWAV, SSR, SLRD,
     I                   T1, T0, Q0, DENVV,
     O                   SHF, EVAP, SLRU,
     O                   Shf0, dShf, Evp0, dEvp, Slr0, dSlr, sFlx,
     O                   TSFC, TSKIN,
     I                   bi,bj,myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SUFLUX_LAND
C     | o compute surface flux over land
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

C-- Physics package
#include "AIM_PARAMS.h"

C     Physical constants + functions of sigma and latitude
#include "com_physcon.h"

C     Surface flux constants
#include "com_sflcon.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C--   Input:
C    PSA    :: norm. surface pressure [p/p0]   (2-dim)
C    FMASK  :: fractional land-sea mask        (2-dim)
C    EMISloc:: longwave surface emissivity
C    Tsurf  :: surface temperature        (2-dim)
C    dTskin :: temp. correction for daily-cycle heating [K]
C    SWAV   :: soil wetness availability [0-1] (2-dim)
C    SSR    :: sfc sw radiation (net flux)     (2-dim)
C    SLRD   :: sfc lw radiation (downward flux)(2-dim)
C    T1     :: near-surface air temperature (from Pot.temp)
C    T0     :: near-surface air temperature    (2-dim)
C    Q0     :: near-surface sp. humidity [g/kg](2-dim)
C    DENVV  :: surface flux (sens,lat.) coeff. (=Rho*|V|) [kg/m2/s]
C--   Output:
C    SHF    :: sensible heat flux              (2-dim)
C    EVAP   :: evaporation [g/(m^2 s)]         (2-dim)
C    SLRU   :: sfc lw radiation (upward flux)  (2-dim)
C    Shf0   :: sensible heat flux over freezing surf.
C    dShf   :: sensible heat flux derivative relative to surf. temp
C    Evp0   :: evaporation computed over freezing surface (Ts=0.oC)
C    dEvp   :: evaporation derivative relative to surf. temp
C    Slr0   :: upward long wave radiation over freezing surf.
C    dSlr   :: upward long wave rad. derivative relative to surf. temp
C    sFlx   :: net surface flux (+=down) function of surf. temp Ts:
C              0: Flux(Ts=0.oC) ; 1: Flux(Ts^n) ; 2: d.Flux/d.Ts(Ts^n)
C    TSFC   :: surface temperature (clim.)     (2-dim)
C    TSKIN  :: skin surface temperature        (2-dim)
C--   Input:
C    bi,bj  :: tile index
C    myThid :: Thread number for this instance of the routine
C--
      _RL  PSA(NGP), FMASK(NGP), EMISloc
      _RL  Tsurf(NGP), dTskin(NGP), SWAV(NGP)
      _RL  SSR(NGP), SLRD(NGP) 
      _RL  T1(NGP), T0(NGP), Q0(NGP), DENVV(NGP)

      _RL  SHF(NGP), EVAP(NGP), SLRU(NGP)
      _RL  Shf0(NGP), dShf(NGP), Evp0(NGP), dEvp(NGP)
      _RL  Slr0(NGP), dSlr(NGP), sFlx(NGP,0:2)
      _RL  TSFC(NGP), TSKIN(NGP)

      INTEGER bi,bj,myThid
CEOP

#ifdef ALLOW_AIM

C-- Local variables:
C    CDENVV :: surf. heat flux (sens.,lat.) coeff including stability effect
      _RL CDENVV(NGP), RDTH, FSLAND
      _RL Fstb0, dTstb, dFstb
      _RL QSAT0(NGP,2)
      _RL QDUMMY(1), RDUMMY(1), TS2
      INTEGER J

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     1.5 Define effective skin temperature to compensate for
C         non-linearity of heat/moisture fluxes during the daily cycle

      DO J=1,NGP
        TSKIN(J) = Tsurf(J) + dTskin(J)
        TSFC(J)=273.16 _d 0 + dTskin(J)
      ENDDO


C--   2. Computation of fluxes over land and sea

C     2.1 Stability correction

      RDTH = FSTAB/DTHETA

      DO J=1,NGP
        FSLAND=1.+MIN(DTHETA,MAX(-DTHETA,TSKIN(J)-T1(J)))*RDTH
        CDENVV(J)=CHL*DENVV(J)*FSLAND
      ENDDO

      IF ( dTstab.GT.0. _d 0 ) THEN
C-    account for stability function derivative relative to Tsurf:
C note: to avoid discontinuity in the derivative (because of min,max), compute
C   the derivative using the discrete form: F(Ts+dTstab)-F(Ts-dTstab)/2.dTstab
       DO J=1,NGP
        Fstb0 = 1.+MIN(DTHETA,MAX(-DTHETA,TSFC(J) -T1(J)))*RDTH
        Shf0(J) = CHL*DENVV(J)*Fstb0
        dTstb = ( DTHETA+dTstab-ABS(TSKIN(J)-T1(J)) )/dTstab
        dFstb = RDTH*MIN(1. _d 0, MAX(0. _d 0, dTstb*0.5 _d 0))
        dShf(J) = CHL*DENVV(J)*dFstb
       ENDDO
      ENDIF

C     2.2 Evaporation

      CALL SHTORH (2, NGP, TSKIN, PSA, 1. _d 0, QDUMMY, dEvp,
     &                QSAT0(1,1), myThid)
      CALL SHTORH (0, NGP, TSFC, PSA, 1. _d 0, QDUMMY, RDUMMY,
     &                QSAT0(1,2), myThid)

#ifdef ALLOW_DEW_ON_LAND
C--   allow negative evaporation (dew):
      IF ( dTstab.GT.0. _d 0 ) THEN
C-    account for stability function derivative relative to Tsurf:
       DO J=1,NGP
        EVAP(J) = CDENVV(J)*SWAV(J)*(QSAT0(J,1)-Q0(J))
        Evp0(J) =   Shf0(J)*SWAV(J)*(QSAT0(J,2)-Q0(J))
        dEvp(J) = CDENVV(J)*SWAV(J)*dEvp(J)
     &            + dShf(J)*SWAV(J)*(QSAT0(J,1)-Q0(J))
       ENDDO
      ELSE
       DO J=1,NGP
        EVAP(J) = CDENVV(J)*SWAV(J)*(QSAT0(J,1)-Q0(J))
        Evp0(J) = CDENVV(J)*SWAV(J)*(QSAT0(J,2)-Q0(J))
        dEvp(J) = CDENVV(J)*SWAV(J)*dEvp(J)
       ENDDO
      ENDIF
#else /* ALLOW_DEW_ON_LAND */
C--   allow only positive evaporation (no dew):
      IF ( dTstab.GT.0. _d 0 ) THEN
C-    account for stability function derivative relative to Tsurf:
       DO J=1,NGP
        EVAP(J) = CDENVV(J)*SWAV(J)*MAX(0. _d 0,QSAT0(J,1)-Q0(J))
        Evp0(J) =   Shf0(J)*SWAV(J)*MAX(0. _d 0,QSAT0(J,2)-Q0(J))
        dEvp(J) = CDENVV(J)*SWAV(J)*dEvp(J)
     &            + dShf(J)*SWAV(J)*MAX(0. _d 0,QSAT0(J,1)-Q0(J))
       ENDDO
      ELSE
       DO J=1,NGP
C       EVAP(J,1) = CDENVV(J,1)*SWAV(J)*MAX(0. _d 0,QSAT0(J,1)-Q0(J))
c       EVAP(J,1) = CDENVV(J,1)*MAX(0. _d 0,SWAV(J)*QSAT0(J,1)-Q0(J))
Cjmc: try the other formulation (= described in F.M paper):
        EVAP(J) = CDENVV(J)*SWAV(J)*MAX(0. _d 0,QSAT0(J,1)-Q0(J))
        Evp0(J) = CDENVV(J)*SWAV(J)*MAX(0. _d 0,QSAT0(J,2)-Q0(J))
        dEvp(J) = CDENVV(J)*SWAV(J)*dEvp(J)
       ENDDO
      ENDIF
#endif /* ALLOW_DEW_ON_LAND */

C     2.3 Sensible heat flux

      IF ( dTstab.GT.0. _d 0 ) THEN
C-    account for stability function derivative relative to Tsurf:
       DO J=1,NGP
        SHF(J)  = CDENVV(J)*CP*(TSKIN(J)-T0(J))
        Shf0(J) =   Shf0(J)*CP*(TSFC(J) -T0(J))
        dShf(J) = CDENVV(J)*CP
     &            + dShf(J)*CP*(TSKIN(J)-T0(J))
        dShf(J) = MAX( dShf(J), 0. _d 0 )
C--   do not allow negative derivative vs Ts of Sensible+Latent H.flux:
C     a) quiet unrealistic ;
C     b) garantee positive deriv. of total H.flux (needed for implicit solver)
        dEvp(J) = MAX( dEvp(J), -dShf(J)/ALHC )
       ENDDO
      ELSE
       DO J=1,NGP
        SHF(J)  = CDENVV(J)*CP*(TSKIN(J)-T0(J))
        Shf0(J) = CDENVV(J)*CP*(TSFC(J) -T0(J))
        dShf(J) = CDENVV(J)*CP
       ENDDO
      ENDIF

C     2.4 Emission of lw radiation from the surface

      DO J=1,NGP
        TS2     = TSFC(J)*TSFC(J)
        Slr0(J) = SBC*TS2*TS2
        TS2     = TSKIN(J)*TSKIN(J)
        SLRU(J) = SBC*TS2*TS2
        dSlr(J)  = 4. _d 0 *SBC*TS2*TSKIN(J)
      ENDDO

C--   Compute net surface heat flux (+=down) and its derivative ./. surf. temp.
      DO J=1,NGP
        sFlx(J,0)= ( SSR(J) + SLRD(J) - EMISloc*Slr0(J) )
     &           - ( Shf0(J) + ALHC*Evp0(J) )
        sFlx(J,1)= ( SSR(J) + SLRD(J) - EMISloc*SLRU(J) )
     &           - ( SHF(J)+ ALHC*EVAP(J) )
        sFlx(J,2)=                    - EMISloc*dSlr(J)
     &           - ( dShf(J) + ALHC*dEvp(J) )
      ENDDO

C--   3. Adjustment of skin temperature and fluxes over land
C--      based on energy balance (to be implemented)
C        <= done separately for each surface type (land,sea,sea-ice)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */

      RETURN
      END
