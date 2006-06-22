C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_formula_aim.F,v 1.2 2006/06/22 14:10:29 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_FORMULA_AIM
C     !INTERFACE:
      SUBROUTINE BULKF_FORMULA_AIM(
     I                   Tsurf, SLRD,
     I                   T1, T0, Q0, Vsurf,
     O                   SHF, EVAP, SLRU,
     O                   dEvp, sFlx,
     I                   iceornot, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R BULKF_FORMULA_AIM
C     | o compute surface flux over ocean and sea-ice,
C     |   using AIM surface flux formulation
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     Resolution parameters

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "BULKF_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C--   Input:
C    FMASK  :: fractional land-sea mask        (2-dim)
C    Tsurf  :: surface temperature        (2-dim)
C    SSR    :: sfc sw radiation (net flux)     (2-dim)
C    SLRD   :: sfc lw radiation (downward flux)(2-dim)
C    T1     :: near-surface air temperature (from Pot.temp)
C    T0     :: near-surface air temperature    (2-dim)
C    Q0     :: near-surface sp. humidity [g/kg](2-dim)
C    Vsurf  :: surface wind speed        [m/s] (2-dim,input)
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
C    sFlx   :: net heat flux (+=down) except SW, function of surf. temp Ts:
C              0: Flux(Ts=0.oC) ; 1: Flux(Ts^n) ; 2: d.Flux/d.Ts(Ts^n)
C    TSFC   :: surface temperature (clim.)     (2-dim)
C    TSKIN  :: skin surface temperature        (2-dim)
C--   Input:
C    iceornot :: 0=open water, 1=ice cover
C    myThid :: Thread number for this instance of the routine
C--
      INTEGER NGP
      PARAMETER ( NGP = 1 )
c     _RL  PSA(NGP), FMASK(NGP), EMISloc
      _RL  Tsurf(NGP)
c     _RL  SSR(NGP)
      _RL  SLRD(NGP)
      _RL  T1(NGP), T0(NGP), Q0(NGP), Vsurf(NGP)

      _RL  SHF(NGP), EVAP(NGP), SLRU(NGP)
      _RL  dEvp(NGP), sFlx(NGP,0:2)
c     _RL  Shf0(NGP), dShf(NGP), Evp0(NGP), dEvp(NGP)
c     _RL  Slr0(NGP), dSlr(NGP), sFlx(NGP,0:2)
c     _RL  TSFC(NGP), TSKIN(NGP)

      INTEGER iceornot
      INTEGER myThid
CEOP

#ifdef ALLOW_FORMULA_AIM

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C      FWIND0 = ratio of near-sfc wind to lowest-level wind
C      CHS    = heat exchange coefficient over sea
C      VGUST  = wind speed for sub-grid-scale gusts
C      DTHETA = Potential temp. gradient for stability correction
C      dTstab = potential temp. increment for stability function derivative
C      FSTAB  = Amplitude of stability correction (fraction)
C       P0    = reference pressure                 [Pa=N/m2]
C       GG    = gravity accel.                     [m/s2]
C       RD    = gas constant for dry air           [J/kg/K]
C       CP    = specific heat at constant pressure [J/kg/K]
C       ALHC  = latent heat of condensation        [J/g]
C       ALHF  = latent heat of freezing            [J/g]
C       SBC   = Stefan-Boltzmann constant
C      EMISloc :: longwave surface emissivity
c     _RL FWIND0, CHS, VGUST, DTHETA, dTstab, FSTAB
      _RL P0, ALHC, ALHF, RD, CP, SBC, EMISloc
      EQUIVALENCE ( ocean_emissivity , EMISloc )
      EQUIVALENCE ( Lvap   , ALHC )
      EQUIVALENCE ( Lfresh , ALHF )
      EQUIVALENCE ( Rgas   , RD )
      EQUIVALENCE ( cpair  , CP )
      EQUIVALENCE ( stefan , SBC )

C-- Local variables:
C    PSA     :: norm. surface pressure [p/p0]   (2-dim)
C    DENVV   :: surface flux (sens,lat.) coeff. (=Rho*|V|) [kg/m2/s]
C    CDENVV  :: surf. heat flux (sens.,lat.) coeff including stability effect
C    ALHevp  :: Latent Heat of evaporation
      _RL PSA(NGP)
      _RL DENVV(NGP), PRD
      _RL  Shf0(NGP), dShf(NGP), Evp0(NGP)
      _RL  Slr0(NGP), dSlr(NGP)
      _RL  TSFC(NGP), TSKIN(NGP)
      _RL CDENVV(NGP), RDTH, FSSICE
      _RL ALHevp, Fstb0, dTstb, dFstb
      _RL QSAT0(NGP,2)
      _RL QDUMMY(1), RDUMMY(1), TS2
      INTEGER J
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      PSA(1) = 1. _d 0
      P0 = 1. _d +5
C---

      ALHevp = ALHC
C     Evap of snow/ice: account for Latent Heat of freezing :
c     IF ( aim_energPrecip .OR. useThSIce ) ALHevp = ALHC + ALHF
      IF ( iceornot.GE.1 ) ALHevp = ALHC + ALHF

C     1.4 Density * wind speed (including gustiness factor)

      PRD = P0/RD
c     VG2 = VGUST*VGUST
c     factWind2 = FWIND0*FWIND0

      DO J=1,NGP
c       SPEED0(J)=SQRT(factWind2*Vsurf2(J)+VG2)
c       DENVV(J)=(PRD*PSA(J)/T0(J))*SPEED0(J)
C-- assuming input file "WspeedFile" contains the time-average "SPEED0"
C     from AIM output (aimPhytave: fields # 15 ; aimDiag: WINDS ) :
        DENVV(J)=(PRD*PSA(J)/T0(J))*Vsurf(J)
      ENDDO

C     1.5 Define effective skin temperature to compensate for
C         non-linearity of heat/moisture fluxes during the daily cycle

      DO J=1,NGP
        TSKIN(J) = Tsurf(J) + celsius2K
        TSFC(J)=273.16 _d 0
      ENDDO

C--   2. Computation of fluxes over land and sea

C     2.1 Stability correction

      RDTH = FSTAB/DTHETA

      DO J=1,NGP
        FSSICE=1.+MIN(DTHETA,MAX(-DTHETA,TSKIN(J)-T1(J)))*RDTH
        CDENVV(J)=CHS*DENVV(J)*FSSICE
      ENDDO

      IF ( dTstab.GT.0. _d 0 ) THEN
C-    account for stability function derivative relative to Tsurf:
C note: to avoid discontinuity in the derivative (because of min,max), compute
C   the derivative using the discrete form: F(Ts+dTstab)-F(Ts-dTstab)/2.dTstab
       DO J=1,NGP
        Fstb0 = 1.+MIN(DTHETA,MAX(-DTHETA,TSFC(J) -T1(J)))*RDTH
        Shf0(J) = CHS*DENVV(J)*Fstb0
        dTstb = ( DTHETA+dTstab-ABS(TSKIN(J)-T1(J)) )/dTstab
        dFstb = RDTH*MIN(1. _d 0, MAX(0. _d 0, dTstb*0.5 _d 0))
        dShf(J) = CHS*DENVV(J)*dFstb
       ENDDO
      ENDIF

C     2.2 Evaporation

      CALL BULKF_SH2RH_AIM( 2, NGP, TSKIN, PSA, 1. _d 0,
     &                      QDUMMY, dEvp,  QSAT0(1,1), myThid )
      CALL BULKF_SH2RH_AIM( 0, NGP, TSFC,  PSA, 1. _d 0,
     &                      QDUMMY, RDUMMY,QSAT0(1,2), myThid )

      IF ( dTstab.GT.0. _d 0 ) THEN
C-    account for stability function derivative relative to Tsurf:
       DO J=1,NGP
        EVAP(J) = CDENVV(J)*(QSAT0(J,1)-Q0(J))
        Evp0(J) =   Shf0(J)*(QSAT0(J,2)-Q0(J))
        dEvp(J) = CDENVV(J)*dEvp(J)
     &            + dShf(J)*(QSAT0(J,1)-Q0(J))
       ENDDO
      ELSE
       DO J=1,NGP
        EVAP(J) = CDENVV(J)*(QSAT0(J,1)-Q0(J))
        Evp0(J) = CDENVV(J)*(QSAT0(J,2)-Q0(J))
        dEvp(J) = CDENVV(J)*dEvp(J)
       ENDDO
      ENDIF

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
        dEvp(J) = MAX( dEvp(J), -dShf(J)/ALHevp )
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

C--   Compute net surface heat flux and its derivative ./. surf. temp.
      DO J=1,NGP
        sFlx(J,0)= ( SLRD(J) - EMISloc*Slr0(J) )
     &           - ( Shf0(J) + ALHevp*Evp0(J) )
        sFlx(J,1)= ( SLRD(J) - EMISloc*SLRU(J) )
     &           - ( SHF(J)  + ALHevp*EVAP(J) )
        sFlx(J,2)=            -EMISloc*dSlr(J)
     &           - ( dShf(J) + ALHevp*dEvp(J) )
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_FORMULA_AIM */

      RETURN
      END
