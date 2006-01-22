C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_formula_lanl.F,v 1.7 2006/01/22 15:51:35 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

      subroutine bulkf_formula_lanl(
     I                           uw, vw, us, Ta, Qa, nc, tsf_in,
     O                           flwupa, flha, fsha, df0dT,
     O                           ust, vst, evp, ssq, dEvdT, 
     I                           iceornot, myThid )

c swd -- bulkf formula used in bulkf and ice pkgs
c taken from exf package 

       IMPLICIT NONE
c
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "BULKF_PARAMS.h"

c
c calculate bulk formula fluxes over open ocean or seaice
c
c input
      _RL us                 ! wind speed
      _RL uw                 ! zonal wind speed (at grid center)
      _RL vw                 ! meridional wind speed (at grid center)
      _RL Ta                 ! air temperature at ht
      _RL Qa                 ! specific humidity at ht
      _RL tsf_in             ! surface temperature (either ice or open water)
      _RL nc                 ! fraction cloud cover
      integer iceornot       ! 0=open water, 1=ice cover
      integer myThid         ! my Thread Id number
c output
      _RL flwupa             ! upward long wave radiation (>0 upward)
      _RL flha               ! latent heat flux         (>0 downward)
      _RL fsha               ! sensible heat flux       (>0 downward)
      _RL df0dT              ! derivative of heat flux with respect to Tsf
      _RL ust                ! zonal wind stress (at grid center)
      _RL vst                ! meridional wind stress (at grid center)
      _RL evp                ! evaporation rate (over open water) [kg/m2/s]
      _RL ssq                ! surface specific humidity (kg/kg)
      _RL dEvdT              ! derivative of evap. with respect to Tsf [kg/m2/s/K]

#ifdef ALLOW_BULK_FORCE

c local variables  
      _RL tsf                ! surface temperature in K
      _RL ht                 ! reference height (m)
      _RL hq                 ! reference height for humidity (m)
      _RL hu                 ! reference height for wind speed (m)
      _RL zref               ! reference height
      _RL czol
      _RL usm                ! wind speed limited
      _RL t0                 ! virtual temperature (K)
      _RL deltap             ! potential temperature diff (K)
      _RL delq               ! specific humidity difference
      _RL stable
      _RL rdn   ,ren, rhn 
      _RL ustar
      _RL tstar
      _RL qstar
      _RL huol
      _RL xsq
      _RL x
      _RL re
      _RL rh
      _RL tau
      _RL psimh
      _RL psixh
      _RL rd
      _RL aln
      _RL cdalton
      _RL dflhdT             ! derivative of latent heat with respect to T
      _RL dfshdT             ! derivative of sensible heat with respect to T
      _RL dflwupdT           ! derivative of long wave with respect to T
c     _RL mixratio
c     _RL ea
      _RL psim_fac
      _RL umin
      _RL lath
      _RL csha
      _RL clha
      _RL zice
      _RL ssq0, ssq1, ssq2   ! constant used in surface specific humidity
      _RL bulkf_Cdn          ! drag coefficient
      integer niter_bulk, iter

c --- external functions ---
c     _RL       exf_BulkCdn
c     external  exf_BulkCdn
c     _RL       exf_BulkqSat
c     external  exf_BulkqSat
c     _RL       exf_BulkRhn
c     external  exf_BulkRhn

      DATA   ssq0,           ssq1,           ssq2 
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /

cQQQQQQQQQQQQQQQQQQQQq
c -- compute turbulent surface fluxes
              ht =  2. _d 0
              hq =  2. _d 0
              hu = 10. _d 0
              zref = 10. _d 0
              zice = 0.0005 _d 0
              aln = log(ht/zref)
              niter_bulk = 5
              cdalton = 0.0346000 _d 0
              czol = zref*xkar*gravity
              psim_fac=5. _d 0
              umin=1. _d 0
c 
              lath=Lvap
              if (iceornot.gt.0) lath=Lvap+Lfresh
              Tsf=Tsf_in+Tf0kel
c     Wind speed 
              if (us.eq.0. _d 0) then
                us = sqrt(uw*uw + vw*vw)
              endif
              usm = max(us,umin)
cQQQ try to limit drag
cQQ           usm = min(usm,5. _d 0)
c
              t0     = Ta*(1. _d 0 + humid_fac*Qa)
cQQ           ssq    = 0.622*6.11*exp(22.47*(1.d0-Tf0kel/tsf))/p0
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c             ssq = 3.797915 _d 0*exp(
c    &                lath*(7.93252 _d -6 - 2.166847 _d -3/Tsf)
c    &                               )/p0
              ssq = ssq0*exp( lath*(ssq1-ssq2/Tsf) ) / p0
cBB debugging
cBB             print*,'ice, ssq',  ssq
c
              deltap = ta  - tsf + gamma_blk*ht
              delq   = Qa - ssq
c
c initialize estimate exchange coefficients
              rdn=xkar/(log(zref/zice))
              rhn=rdn
              ren=rdn
c calculate turbulent scales
              ustar=rdn*usm
              tstar=rhn*deltap
              qstar=ren*delq
c
c interation with psi-functions to find transfer coefficients
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
   
c Update the transfer coefficients 
   
                 rd = rdn/(1. _d 0 + rdn*(aln-psimh)/xkar)
                 rh = rhn/(1. _d 0 + rhn*(aln-psixh)/xkar)
                 re = rh
c  Update ustar, tstar, qstar using updated, shifted coefficients.
                 ustar = rd*usm
                 qstar = re*delq
                 tstar = rh*deltap
              enddo
c
                tau   = rhoa*ustar**2
                tau   = tau*us/usm
                csha   = rhoa*cpair*us*rh*rd   
                clha   = rhoa*lath*us*re*rd   
c
                fsha  = csha*deltap
                flha  = clha*delq
                evp   = -flha/lath
c
c up long wave radiation
cQQ           mixratio=Qa/(1-Qa)
cQQ           ea=p0*mixratio/(0.62197+mixratio)
cQQ           flwupa=-0.985*stefan*tsf**4
cQQ  &                  *(0.39-0.05*sqrt(ea))
cQQ  &                  *(1-0.6*nc**2)
              if (iceornot.eq.0) then
               flwupa=ocean_emissivity*stefan*tsf**4
               dflwupdT=4. _d 0*ocean_emissivity*stefan*tsf**3
              else
               if (iceornot.eq.2) then
                flwupa=snow_emissivity*stefan*tsf**4
                dflwupdT=4. _d 0*snow_emissivity*stefan*tsf**3
               else 
                flwupa=ice_emissivity*stefan*tsf**4
                dflwupdT=4. _d 0*ice_emissivity*stefan*tsf**3
               endif
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
c
c wind stress at center points
c             if (.NOT.windread) then
c                ust = rhoa*exf_BulkCdn(usm)*us*uw
c                vst = rhoa*exf_BulkCdn(usm)*us*vw
c             endif
C-  In-lining of function: exf_BulkCdn(umps) = cdrag_1/umps + cdrag_2 + cdrag_3*umps
              bulkf_Cdn = cdrag_1/usm + cdrag_2 + cdrag_3*usm
              ust = rhoa*bulkf_Cdn*us*uw
              vst = rhoa*bulkf_Cdn*us*vw
#endif /*ALLOW_BULK_FORCE*/

      RETURN
      END
