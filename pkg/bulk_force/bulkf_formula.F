c swd -- bulkf formula used in bulkf and ice pkgs

#include "CPP_OPTIONS.h"
      subroutine bulkf_formula(
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
      _RL usm                ! wind speed limited
      _RL t0                 ! virtual temperature (K)
      _RL deltap             ! potential temperature diff (K)
      _RL delq               ! specific humidity difference
      _RL stable
      _RL lhcoef
      _RL shcoef
      _RL dflhdT             ! derivative of latent heat with respect to T
      _RL dfshdT             ! derivative of sensible heat with respect to T
      _RL dflwupdT           ! derivative of long wave with respect to T
      _RL mixratio
      _RL ea

#ifdef ALLOW_BULKFORMULA

c --- external functions ---
      _RL       exf_BulkCdn
      external  exf_BulkCdn

              Qa=max(Qa,1e-7)
              Tsf=Tsf_in+Tf0kel
              ht=10
c     Wind speed 
              if (us.eq.0) then
                us = sqrt(uw*uw + vw*vw)
              endif
              usm = max(us,1.d-5)
cQQQ try to limit drag
              usm = min(usm,5.d0)
c
                t0     = Ta*(1.d0 + humid_fac*Qa)
                ssq    = qcoef*exp(22.47*(1.d0-Tf0kel/tsf))
cBB debugging
cBB             print*,'ice, ssq', qcoef,  ssq
c
              deltap = ta  - tsf + gamma_blk*ht
              delq   = Qa - ssq
              stable = 5.d-1 + sign(5.d-1, deltap)
cQQ -- check all of this
c latent heat coeff. following Gill 2.4.6 and 2.6.2 formula
              lhcoef =Lvap*rhoa*usm*1.5e-3  !QQ name coeff.
              flha = lhcoef*delq
              evp= lhcoef*delq/Lvap/rhosw
c sensible heat coeff. following Gill 2.4.5 formula
              if (stable.gt.0) then
                 shcoef = 0.83e-3*rhoa*usm*cpair
              else
                 shcoef = 1.10e-3*rhoa*usm*cpair
              endif 
              fsha = shcoef*deltap
c up long wave radiation
              mixratio=Qa/(1-Qa)
              ea=p0*mixratio/(0.62197+mixratio)
              flwup=-0.985*stefan*tsf**4
     &                  *(0.39-0.05*sqrt(ea))
     &                  *(1-0.6*nc**2)
#ifdef ALLOW_TSEAICE
       if (iceornot.eq.1) then
c derivatives with respect to Tsf
              dflhdT = -lhcoef*Tf0kel*ssq*22.47/(tsf**2)
              dfshdT = -shcoef
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

      return
      end
