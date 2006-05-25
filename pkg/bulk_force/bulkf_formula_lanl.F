C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_formula_lanl.F,v 1.8 2006/05/25 17:30:54 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_FORMULA_LANL
C     !INTERFACE:
      SUBROUTINE BULKF_FORMULA_LANL(
     I                           uw, vw, us, Ta, Qa, nc, tsf_in,
     O                           flwupa, flha, fsha, df0dT,
     O                           ust, vst, evp, ssq, dEvdT,
     I                           iceornot, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE BULKF_FORMULA_LANL
c     | o Calculate bulk formula fluxes over open ocean or seaice
C     *==========================================================*
C     \ev
C swd -- bulkf formula used in bulkf and ice pkgs
C        taken from exf package
C
C     wind stress = (ust,vst) = rhoA * Cd * Ws * (del.u,del.v)
C     Sensib Heat flux = fsha = rhoA * Ch * Ws * del.T * CpAir
C     Latent Heat flux = flha = rhoA * Ce * Ws * del.Q * Lvap
C                      = -Evap * Lvap
C   with Ws = wind speed = sqrt(del.u^2 +del.v^2) ;
C        del.T = Tair - Tsurf ; del.Q = Qair - Qsurf
C        Cd,Ch,Ce = transfer coefficient for momentum, sensible
C                    & latent heat flux [no units]
C     *==========================================================*

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
      _RL us                 ! wind speed        [m/s]   at height hu
      _RL Ta                 ! air temperature   [K]     at height ht
      _RL Qa                 ! specific humidity [kg/kg] at heigth ht
      _RL nc                 ! fraction cloud cover
      _RL tsf_in             ! sea-ice or sea surface temperature [oC]
      INTEGER iceornot       ! 0=open water, 1=sea-ice, 2=sea-ice with snow
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

      _RL tsf                ! surface temperature [K]
      _RL ht                 ! height for air temperature [m]
c     _RL hq                 ! height for humidity [m]
      _RL hu                 ! height for wind speed [m]
c     _RL zref               ! reference height [m]
      _RL usm                ! wind speed limited [m/s]
c     _RL umin               ! minimum wind speed used for drag-coeff [m/s]
      _RL lath               ! latent heat of vaporization or sublimation
      _RL t0                 ! virtual temperature [K]
      _RL deltap             ! potential temperature diff [K]
      _RL delq               ! specific humidity difference [kg/kg]
      _RL ustar              ! friction velocity [m/s]
      _RL tstar              ! temperature scale [K]
      _RL qstar              ! humidity scale  [kg/kg]
      _RL rd                 ! = sqrt(Cd)          [-]
      _RL re                 ! = Ce / sqrt(Cd)     [-]
      _RL rh                 ! = Ch / sqrt(Cd)     [-]
      _RL rdn, ren, rhn      ! initial (neutral) values of rd, re, rh
      _RL stable             ! = 1 if stable ; = 0 if unstable
      _RL huol               ! stability parameter [-]
      _RL x                  ! stability function  [-]
      _RL xsq                ! = x^2               [-]
      _RL psimh              ! momentum stability function
      _RL psixh              ! latent & sensib. stability function
      _RL czol               ! = zref*Karman_cst*gravity
      _RL aln                ! = log(ht/zref)
c     _RL cdalton            ! coeff to evaluate Dalton Number
c     _RL mixratio
c     _RL ea
c     _RL psim_fac
      _RL tau                ! surface stress  coef = ?
      _RL csha               ! sensib.heat flx coef = rhoA * Ws * Ch * CpAir
      _RL clha               ! latent heat flx coef = rhoA * Ws * Ce * Lvap
      _RL zice
      _RL ssq0, ssq1, ssq2   ! constant used in surface specific humidity
      _RL p0                 ! reference sea-level atmospheric pressure [mb]
      _RL bulkf_Cdn          ! drag coefficient
      INTEGER niter_bulk, iter

C     == external Functions
c     _RL       exf_BulkCdn
c     external  exf_BulkCdn
c     _RL       exf_BulkqSat
c     external  exf_BulkqSat
c     _RL       exf_BulkRhn
c     external  exf_BulkRhn

      DATA   ssq0,           ssq1,           ssq2
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /
      DATA   p0 / 1013. _d 0 /

C--- Compute turbulent surface fluxes
              ht =  2. _d 0
c             hq =  2. _d 0
              hu = 10. _d 0
c             zref = 10. _d 0
              zice = 0.0005 _d 0
              aln = log(ht/zref)
              niter_bulk = 5
c             cdalton = 0.0346000 _d 0
              czol = zref*xkar*gravity
c             psim_fac=5. _d 0
c             umin=1. _d 0

              lath=Lvap
              if (iceornot.gt.0) lath=Lvap+Lfresh
              Tsf=Tsf_in+Tf0kel
C-   Wind speed
              if (us.eq.0. _d 0) then
                us = sqrt(uw*uw + vw*vw)
              endif
              usm = max(us,umin)
c
              t0     = Ta*(1. _d 0 + humid_fac*Qa)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
cQQ           ssq    = 0.622*6.11*exp(22.47*(1.d0-Tf0kel/tsf))/p0
c             ssq = 3.797915 _d 0*exp(
c    &                lath*(7.93252 _d -6 - 2.166847 _d -3/Tsf)
c    &                               )/p0
              ssq = ssq0*exp( lath*(ssq1-ssq2/Tsf) ) / p0

              deltap = ta  - tsf + gamma_blk*ht
              delq   = Qa - ssq

C--  initialize estimate exchange coefficients
              rdn=xkar/(log(zref/zice))
              rhn=rdn
              ren=rdn
C--  calculate turbulent scales
              ustar=rdn*usm
              tstar=rhn*deltap
              qstar=ren*delq

C--  iteration with psi-functions to find transfer coefficients
              do iter=1,niter_bulk
                 huol   = czol/ustar**2 *(tstar/t0 +
     &                    qstar/(1. _d 0/humid_fac+Qa))
                 huol   = sign( min(abs(huol),10. _d 0), huol)
                 stable = 5. _d -1 + sign(5. _d -1 , huol)
                 xsq = max(sqrt(abs(1. _d 0 - 16. _d 0*huol)),1. _d 0)
                 x      = sqrt(xsq)
                 psimh = -5. _d 0*huol*stable + (1. _d 0-stable)*
     &                    (2. _d 0*log(5. _d -1*(1. _d 0+x)) +
     &                     2. _d 0*log(5. _d -1*(1. _d 0+xsq)) -
     &                     2. _d 0*atan(x) + pi*.5 _d 0)
                 psixh  = -5. _d 0*huol*stable + (1. _d 0-stable)*
     &                     (2. _d 0*log(5. _d -1*(1. _d 0+xsq)))

C--  Update the transfer coefficients
                 rd = rdn/(1. _d 0 + rdn*(aln-psimh)/xkar)
                 rh = rhn/(1. _d 0 + rhn*(aln-psixh)/xkar)
                 re = rh
C--  Update ustar, tstar, qstar using updated, shifted coefficients.
                 ustar = rd*usm
                 qstar = re*delq
                 tstar = rh*deltap
              enddo

              tau   = rhoa*ustar**2
              tau   = tau*us/usm
              csha  = rhoa*cpair*us*rh*rd
              clha  = rhoa*lath*us*re*rd

              fsha  = csha*deltap
              flha  = clha*delq
              evp   = -flha/lath

C--  Upward long wave radiation
cQQ           mixratio=Qa/(1-Qa)
cQQ           ea=p0*mixratio/(0.62197+mixratio)
cQQ           flwupa=-0.985*stefan*tsf**4
cQQ  &                  *(0.39-0.05*sqrt(ea))
cQQ  &                  *(1-0.6*nc**2)
              if (iceornot.eq.0) then
               flwupa=ocean_emissivity*stefan*tsf**4
               dflwupdT=4. _d 0*ocean_emissivity*stefan*tsf**3
              elseif (iceornot.eq.2) then
                flwupa=snow_emissivity*stefan*tsf**4
                dflwupdT=4. _d 0*snow_emissivity*stefan*tsf**3
              else
                flwupa=ice_emissivity*stefan*tsf**4
                dflwupdT=4. _d 0*ice_emissivity*stefan*tsf**3
              endif
cQQ           dflhdT = -clha*Tf0kel*ssq*22.47/(tsf**2)
c             dflhdT = -clha*Lath*ssq/(Rvap*tsf**2)
c             dflhdT = -clha*ssq*Lath*2.166847 _d -3/(Tsf**2)
              dEvdT  =  clha*ssq*ssq2/(Tsf*Tsf)
              dflhdT = -lath*dEvdT
              dfshdT = -csha
cQQ           dflwupdT= 4.*0.985*stefan*tsf**3
cQQ  &                  *(0.39-0.05*sqrt(ea))
cQQ  &                  *(1-0.6*nc**2)
c total derivative with respect to surface temperature
              df0dT=-dflwupdT+dfshdT+dflhdT

C--  Wind stress at center points
C-   in-lining of function: exf_BulkCdn(umps) = cdrag_1/umps + cdrag_2 + cdrag_3*umps
              bulkf_Cdn = cdrag_1/usm + cdrag_2 + cdrag_3*usm
              ust = rhoa*bulkf_Cdn*us*uw
              vst = rhoa*bulkf_Cdn*us*vw
#endif /*ALLOW_BULK_FORCE*/

      RETURN
      END
