C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_formula_lay.F,v 1.2 2006/05/30 22:44:54 mlosch Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_FORMULA_LAY
C     !INTERFACE:
      SUBROUTINE BULKF_FORMULA_LAY(
     I                           uw, vw, ws, Ta, Qa, tsfCel,
     O                           flwupa, flha, fsha, df0dT,
     O                           ust, vst, evp, ssq, dEvdT,
     I                           iceornot, i,j,bi,bj,myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE BULKF_FORMULA_LAY
C     | o Calculate bulk formula fluxes over open ocean or seaice
C     |   Large and Yeager, 2004, NCAR/TN-460+STR.
C     *==========================================================*
C     \ev
C
C === Turbulent Fluxes :
C  * use the approach "B": shift coeff to height & stability of the
C    atmosphere state (instead of "C": shift temp & humid to the height
C    of wind, then shift the coeff to this height & stability of the atmos).
C  * similar to EXF (except over sea-ice) ; default parameter values
C    taken from Large & Yeager.
C  * assume that Qair & Tair inputs are from the same height (zq=zt)
C  * formulae in short:
C     wind stress = (ust,vst) = rhoA * Cd * Ws * (del.u,del.v)
C     Sensib Heat flux = fsha = rhoA * Ch * Ws * del.T * CpAir
C     Latent Heat flux = flha = rhoA * Ce * Ws * del.Q * Lvap
C                      = -Evap * Lvap
C   with Ws = wind speed = sqrt(del.u^2 +del.v^2) ;
C        del.T = Tair - Tsurf ; del.Q = Qair - Qsurf ;
C        Cd,Ch,Ce = drag coefficient, Stanton number and Dalton number
C              respectively [no-units], function of height & stability

C     !USES:
       IMPLICIT NONE
C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "BULKF_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     input:
      _RL uw                 ! zonal wind speed (at grid center) [m/s]
      _RL vw                 ! meridional wind speed (at grid center) [m/s]
      _RL ws                 ! wind speed        [m/s]   at height zwd
      _RL Ta                 ! air temperature   [K]     at height zth
      _RL Qa                 ! specific humidity [kg/kg] at heigth zth
      _RL tsfCel             ! sea-ice or sea surface temperature [oC]
      INTEGER iceornot       ! 0=open water, 1=sea-ice, 2=sea-ice with snow
      INTEGER i,j, bi,bj     !current grid point indices
      INTEGER myThid         ! my Thread Id number
C     output:
      _RL flwupa             ! upward long wave radiation (>0 upward) [W/m2]
      _RL flha               ! latent heat flux         (>0 downward) [W/m2]
      _RL fsha               ! sensible heat flux       (>0 downward) [W/m2]
      _RL df0dT              ! derivative of heat flux with respect to Tsf [W/m2/K]
      _RL ust                ! zonal wind stress (at grid center)     [N/m2]
      _RL vst                ! meridional wind stress (at grid center)[N/m2]
      _RL evp                ! evaporation rate (over open water) [kg/m2/s]
      _RL ssq                ! surface specific humidity          [kg/kg]
      _RL dEvdT              ! derivative of evap. with respect to Tsf [kg/m2/s/K]
CEOP

#ifdef ALLOW_BULK_FORCE

C     == Local variables ==
      _RL dflhdT             ! derivative of latent heat with respect to T
      _RL dfshdT             ! derivative of sensible heat with respect to T
      _RL dflwupdT           ! derivative of long wave with respect to T

      _RL Tsf                ! surface temperature [K]
      _RL Ts2                ! surface temperature square [K^2]
c     _RL ht                 ! height for air temperature [m]
c     _RL hq                 ! height for humidity [m]
c     _RL hu                 ! height for wind speed [m]
c     _RL zref               ! reference height [m]
      _RL wsm                ! limited wind speed [m/s] (> umin)
      _RL usn                ! neutral, zref (=10m) wind speed [m/s]
      _RL usm                ! usn but limited [m/s] (> umin)
c     _RL umin               ! minimum wind speed used for drag-coeff [m/s]
      _RL lath               ! latent heat of vaporization or sublimation [J/kg]
      _RL t0                 ! virtual temperature [K]
      _RL delth              ! potential temperature diff [K]
      _RL delq               ! specific humidity difference [kg/kg]
      _RL ustar              ! friction velocity [m/s]
      _RL tstar              ! temperature scale [K]
      _RL qstar              ! humidity scale  [kg/kg]
      _RL rd                 ! = sqrt(Cd)          [-]
      _RL re                 ! = Ce / sqrt(Cd)     [-]
      _RL rh                 ! = Ch / sqrt(Cd)     [-]
      _RL rdn, ren, rhn      ! neutral, zref (=10m) values of rd, re, rh
      _RL stable             ! = 1 if stable ; = 0 if unstable
      _RL huol               ! stability parameter at zwd [-] (=z/Monin-Obuklov length)
      _RL htol               ! stability parameter at zth [-]
      _RL x                  ! stability function  [-]
      _RL xsq                ! = x^2               [-]
      _RL psimh              ! momentum stability function
      _RL psixh              ! latent & sensib. stability function
      _RL czol               ! = zref*Karman_cst*gravity
      _RL zwln               ! = log(zwd/zref)
      _RL ztln               ! = log(zth/zref)
c     _RL cdalton            ! coeff to evaluate Dalton Number
c     _RL mixratio
c     _RL ea
c     _RL psim_fac
      _RL tau                ! surface stress  coef = rhoA * Ws * Cd
      _RL csha               ! sensib.heat flx coef = rhoA * Ws * Ch * CpAir
      _RL clha               ! latent heat flx coef = rhoA * Ws * Ce * Lvap
c     _RL zice
c     _RL ssq0, ssq1, ssq2   ! constant used in saturated specific humidity
c     _RL p0                 ! reference sea-level atmospheric pressure [mb]
      _RL qs1w, qs2w         !   above freezing saturated specific humidity
      _RL qs1i, qs2i         !   below freezing saturated specific humidity
      _RL tmpBlk
      _RL half, one, two
      INTEGER iter

C     == external Functions

C--   Constant
      DATA   half,      one,      two
     &     / 0.5 _d 0 , 1. _d 0 , 2. _d 0 /
c     DATA   ssq0,           ssq1,           ssq2
c    &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /
c     DATA   p0 / 1013. _d 0 /
      DATA   qs1w,           qs2w
     &     /   640.38 _d 3 , 5107.0 _d -0 /
      DATA   qs1i,           qs2i
     &     / 11637.80 _d 3 , 5897.8 _d -0 /

C-- Set surface parameters :
c             zice = 0.0005 _d 0
              zwln = LOG(zwd/zref)
              ztln = LOG(zth/zref)
              czol = zref*xkar*gravity

C-   Surface Temp.
              Tsf = tsfCel+Tf0kel
              Ts2 = Tsf*Tsf
C-   Wind speed
              IF (ws.EQ.0. _d 0) THEN
                ws = SQRT(uw*uw + vw*vw)
              ENDIF
              wsm = MAX(ws,umin)

C--- Compute turbulent surface fluxes
C-   Pot. Temp and saturated specific humidity
              t0     = Ta*(one + humid_fac*Qa)
              IF ( iceornot.EQ.0 ) THEN
                lath=Lvap
                ssq = saltQsFac*qs1w*EXP( -qs2w/Tsf ) / rhoA
                dEvdT = qs2w
              ELSE
                lath = Lvap+Lfresh
                ssq =           qs1i*EXP( -qs2i/Tsf ) / rhoA
                dEvdT = qs2i
              ENDIF
c             ssq = ssq0*EXP( lath*(ssq1-ssq2/Tsf) ) / p0
c             dEvdT = lath*ssq2

              delth  = Ta + gamma_blk*zth - Tsf
              delq   = Qa - ssq

C--  initial guess for exchange coefficients:
C    take U_N = del.U ; stability from del.Theta ;
              stable = half + SIGN(half, delth)
              tmpBlk = cdrag_1/wsm + cdrag_2 + cdrag_3*wsm
              rdn = SQRT(tmpBlk)
              rhn = stable*cStantonS + (one-stable)*cStantonU
              ren = cDalton
c             rdn=xkar/(LOG(zref/zice))
c             rhn=rdn
c             ren=rdn
C--  calculate turbulent scales
              ustar=rdn*wsm
              tstar=rhn*delth
              qstar=ren*delq

C--- iterate with psi-functions to find transfer coefficients
              DO iter=1,blk_nIter

                 huol   = ( tstar/t0
     &                     +qstar/(Qa + one/humid_fac)
     &                    )*czol/(ustar*ustar)
                 huol   = SIGN( MIN(abs(huol),10. _d 0), huol)
                 stable = half + SIGN(half, huol)
                 xsq    = SQRT( ABS(one - huol*16. _d 0) )
                 x      = SQRT(xsq)
                 psimh = -5. _d 0*huol*stable
     &             + (one-stable)*
     &                    ( LOG( (one + two*x + xsq)*(one+xsq)*.125 )
     &                     -two*ATAN(x) + half*pi )
                 htol   = huol*zth/zwd
                 xsq    = SQRT( ABS(one - htol*16. _d 0) )
                 psixh  = -5. _d 0*htol*stable
     &             + (one-stable)*( two*LOG(half*(one+xsq)) )

C-   Shift wind speed using old coefficient
                 usn = ws/(one + rdn/xkar*(zwln-psimh) )
                 usm = MAX(usn, umin)

C-   Update the 10m, neutral stability transfer coefficients
                 tmpBlk = cdrag_1/usm + cdrag_2 + cdrag_3*usm
                 rdn = SQRT(tmpBlk)
                 rhn = stable*cStantonS + (one-stable)*cStantonU
                 ren = cDalton

C-   Shift all coefficients to the measurement height and stability.
                 rd = rdn/(1. _d 0 + rdn*(zwln-psimh)/xkar)
                 rh = rhn/(1. _d 0 + rhn*(ztln-psixh)/xkar)
                 re = ren/(1. _d 0 + ren*(ztln-psixh)/xkar)

C--  Update ustar, tstar, qstar using updated, shifted coefficients.
                 ustar = rd*wsm
                 qstar = re*delq
                 tstar = rh*delth

              ENDDO

C-   Coeff:
              tau   = rhoA*rd*ws
              csha  = cpAir*tau*rh
              clha  =  lath*tau*re

C-   Turbulent Fluxes
              fsha  = csha*delth
              flha  = clha*delq
              evp   = -flha/lath
              ust   = tau*rd*uw
              vst   = tau*rd*vw

C-   surf.Temp derivative of turbulent Fluxes
              dEvdT  =  tau*re*ssq*dEvdT/Ts2
              dflhdT = -lath*dEvdT
              dfshdT = -csha

C--- Upward long wave radiation
              IF ( iceornot.EQ.0 ) THEN
                flwupa  = ocean_emissivity*stefan*Ts2*Ts2
                dflwupdT= ocean_emissivity*stefan*Ts2*Tsf*4. _d 0
              ELSEIF (iceornot.EQ.2) THEN
                flwupa   = snow_emissivity*stefan*Ts2*Ts2
                dflwupdT = snow_emissivity*stefan*Ts2*Tsf*4. _d 0
              ELSE
                flwupa   =  ice_emissivity*stefan*Ts2*Ts2
                dflwupdT =  ice_emissivity*stefan*Ts2*Tsf*4. _d 0
              ENDIF

C-   Total derivative with respect to surface temperature
              df0dT = -dflwupdT+dfshdT+dflhdT

#endif /*ALLOW_BULK_FORCE*/

      RETURN
      END
