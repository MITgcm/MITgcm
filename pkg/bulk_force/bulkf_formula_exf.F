c swd -- bulkf formula used in bulkf and ice pkgs

c taken from exf package 
#include "CPP_OPTIONS.h"
      subroutine bulkf_formula_exf(
     I                           uw, vw, us, Ta, Qa, nc, tsf_in,
     I                           flwup, flha, fsha, df0dT,
     I                           ust, vst, evp, ssq, iceornot, windread
     &                           )

       IMPLICIT NONE
c
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_BULKFORMULA
#include "BULKF_ICE_CONSTANTS.h"
#include "BULKF.h"
#endif

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
      logical windread       !
c output
      _RL flwup              ! upward long wave radiation
      _RL flha                ! latent heat flux
      _RL fsha                ! sensible heat flux
      _RL df0dT              ! derivative of heat flux with respect to Tsf
      _RL ust                ! zonal wind stress (at grid center)
      _RL vst                ! meridional wind stress (at grid center)
      _RL evp               ! evaporation rate (over open water)
      _RL ssq                ! surface specific humidity (kg/kg)
c
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
      _RL rdn    
      _RL ustar
      _RL tstar
      _RL qstar
      _RL huol
      _RL htol
      _RL hqol
      _RL xsq
      _RL x
      _RL re
      _RL rh
      _RL tau
      _RL psimh
      _RL psixh
      _RL rd
      _RL uzn
      _RL shn
      _RL aln
      _RL cdalton
      _RL dflhdT             ! derivative of latent heat with respect to T
      _RL dfshdT             ! derivative of sensible heat with respect to T
      _RL dflwupdT           ! derivative of long wave with respect to T
      _RL mixratio
      _RL ea
      _RL psim_fac
      _RL umin
      integer niter_bulk, iter

#ifdef ALLOW_BULKFORMULA

c --- external functions ---
      _RL       exf_BulkCdn
      external  exf_BulkCdn
      _RL       exf_BulkqSat
      external  exf_BulkqSat
      _RL       exf_BulkRhn
      external  exf_BulkRhn

c -- compute turbulent surface fluxes
              ht =  2.d0
              hq =  2.d0
              hu = 10.d0
              zref = 10.d0
              aln = log(ht/zref)
              niter_bulk = 2
              cdalton = 0.0346000
              czol = hu*xkar*gravity
              psim_fac=5.d0
              umin=5.d-1
c 
              Tsf=Tsf_in+Tf0kel
c     Wind speed 
              if (us.eq.0) then
                us = sqrt(uw*uw + vw*vw)
              endif
              usm = max(us,umin)
cQQQ try to limit drag
cQQ           usm = min(usm,5.d0)
c
              t0     = Ta*(1.d0 + humid_fac*Qa)
              ssq    = saltsat*exf_BulkqSat(Tsf)/rhoa
cBB debugging
cBB             print*,'ice, ssq', qcoef,  ssq
c
              deltap = ta  - tsf + gamma_blk*ht
              delq   = Qa - ssq
              stable = 5.d-1 + sign(5.d-1, deltap)
              rdn=sqrt(exf_BulkCdn(usm))
              ustar=rdn*usm
              tstar=exf_BulkRhn(stable)*deltap
              qstar=cdalton*delq
c
c interation with psi-functions to find transfer coefficients
              do iter=1,niter_bulk
                 huol   = czol*(tstar/t0 +
     &                     qstar/(1.d0/humid_fac+Qa))/
     &                           ustar**2
                 huol   = max(huol,-100.d0)
                 huol   = min(huol, 100.d0)
                 stable = .5d0+ sign(.5d0, huol)
                 htol   = huol*ht/hu
                 hqol   = huol*hq/hu
   
c Evaluate all stability functions assuming hq = ht.
                 xsq    = max(sqrt(abs(1.d0 - 16.*huol)),1.d0)
                 x     = sqrt(xsq)
                 psimh  = -psim_fac*huol*stable +
     &                     (1.d0 - stable)*
     &                     log((1.d0 + x*(2.d0 + x))*
     &                     (1.d0 + xsq)/8.d0) - 2.d0*atan(x) +
     &                     pi*.5d0
                 xsq    = max(sqrt(abs(1.d0 - 16.*htol)),1.d0)
                 psixh  = -psim_fac*htol*stable + (1.d0 - stable)*
     &                     2.d0*log((1.d0 + xsq)/2.d0)
   
c Shift wind speed using old coefficient
ccc              rd   = rdn/(1.d0    + rdn/xkar*
ccc     &                 (log(hu/zref) - psimh) )
                 rd   = rdn/(1.d0    - rdn/xkar*psimh )
                 shn  = us*rd/rdn
                 uzn  = max(shn, umin)
   
c Update the transfer coefficients at 10 meters and neutral stability.
   
                 rdn = sqrt(exf_BulkCdn(uzn))
   
c  Shift all coefficients to the measurement height and stability.
c                rd = rdn/(1.d0    + rdn/xkar*(log(hu/zref) - psimh))
                 rd = rdn/(1.d0    - rdn/xkar*psimh)
                 rh = exf_BulkRhn(stable)/(1.d0    +
     &                                      exf_BulkRhn(stable)/
     &                                     xkar*(aln - psixh))
                 re = cdalton/(1.d0    + cdalton/xkar*(aln - psixh))

c  Update ustar, tstar, qstar using updated, shifted coefficients.
                 ustar = rd*usm
                 qstar = re*delq
                 tstar = rh*deltap
                enddo
c
                tau   = rhoa*ustar**2
                tau   = tau*us/usm
                fsha  = cpair*tau*tstar/ustar
                flha  = Lvap *tau*qstar/ustar
                evp   = tau*qstar/ustar
c

c up long wave radiation
              mixratio=Qa/(1-Qa)
              ea=p0*mixratio/(0.62197+mixratio)
              flwup=-0.985*stefan*tsf**4
     &                  *(0.39-0.05*sqrt(ea))
     &                  *(1-0.6*nc**2)
#ifdef ALLOW_TSEAICE
       if (iceornot.eq.1) then
c derivatives with respect to Tsf
              dflhdT = -(Lvap*tau*re*ssq/ustar)/(tsf**2)
              dfshdT = -cpair*tau*rh/ustar
              dflwupdT=-4.*0.985*stefan*tsf**3
     &                  *(0.39-0.05*sqrt(ea))
c total derivative with respect to surface temperature
              df0dT=dflwupdT+dfshdT+dflhdT
cBB
cBB     print*,'derivatives:',df0dT,dflwupdT,dfshdT,dflhdT
        endif
#endif /*ALLOW_TSEAICE*/
c
c wind stress at center points
              if (.NOT.windread) then
                 ust = rhoa*exf_BulkCdn(usm)*us*uw
                 vst = rhoa*exf_BulkCdn(usm)*us*vw
               endif
#endif /*ALLOW_BULKFORMULA*/
       ssq=deltap

      return
      end
