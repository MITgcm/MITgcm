c swd -- bulkf formula used in bulkf and ice pkgs
c cswd -- adapted from vince/andrei surf_flux.f

#include "CPP_OPTIONS.h"
      subroutine bulkf_formula_vince(
     I                           uw, vw, us, Ta, Qa, nc, tsf_in,
     I                           UFocean, flwup, flha, fsha, df0dT,
     I                           ust, vst, evp, ssq, 
     I                           iceornot, readstress
     &                         )

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
#endif
#ifdef ALLOW_TSEAICE
#include "ICE.h"
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
      _RL nc                 ! cloud cover
      _RL tsf_in             ! surface temperature (either ice or open water)
      integer iceornot       ! 0=open water, 1=ice cover
      logical readstress
c output
      _RL flwup              ! upward long wave radiation
      _RL flha               ! latent heat flux
      _RL fsha               ! sensible heat flux
      _RL df0dT              ! derivative of heat flux with respect to Tsf
      _RL ust                ! zonal wind stress (at grid center)
      _RL vst                ! meridional wind stress (at grid center)
      _RL evp                ! evaporation rate (over open water)
      _RL ssq                ! surface specific humidity (kg/kg)
c
c input and output
      _RL UFocean
c
c local variables  
      _RL ea
      _RL mixratio
      _RL stable
      _RL tsf                ! surface temperature in K
      _RL ht                 ! reference height (m)
      _RL usm                ! wind speed limited
      _RL deltap             ! potential temperature diff (K)
      _RL delq               ! specific humidity difference
      _RL lhcoef
      _RL shcoef
      _RL dflhdT             ! derivative of latent heat with respect to T
      _RL dfshdT             ! derivative of sensible heat with respect to T
      _RL dflwupdT           ! derivative of long wave with respect to T
c from vinces
      _RL elhx
      _RL rough
      _RL cdn
      _RL surf_emissivity    ! ocean or ice emissivity
      _RL qg
      _RL rigs
      _RL dm
      _RL dh
      _RL cdm
      _RL cdh
      _RL rcdhws
      _RL Tsv
      _RL dqssdt
      _RL x
      integer lr

      _RL flwdown           ! part of output eventually QQ
#ifdef ALLOW_BULKFORMULA

      _RL AROUGH(20),BROUGH(20),CROUGH(20),DROUGH(20),EROUGH(20)
      DATA  AROUGH/16.59,13.99,10.4,7.35,5.241,3.926,3.126,2.632,2.319,
     *2.116,1.982,1.893,1.832,1.788,1.757,1.733,1.714,1.699,1.687,1.677/
      DATA BROUGH/3.245,1.733,0.8481,0.3899,0.1832,0.9026E-1,0.4622E-1,
     * .241E-1,.1254E-1,.6414E-2,.3199E-2,.1549E-2,.7275E-3,.3319E-3,
     * .1474E-3,.6392E-4,.2713E-4,.1130E-4,.4630E-5,.1868E-5/
      DATA  CROUGH/5.111,3.088,1.682,.9239,.5626,.3994,.3282,.3017,.299
     *,.3114,.3324,.3587,.3881,.4186,.4492,.4792,.5082,.5361,.5627,
     * .5882/
      DATA DROUGH/1.24,1.02,0.806,0.682,0.661,0.771,0.797,0.895,0.994,
     * 1.09,1.18,1.27,1.35,1.43,1.50,1.58,1.65,1.71,1.78,1.84/
      DATA EROUGH/0.128,0.130,0.141,0.174,0.238,0.330,0.438,0.550,0.660,
     * 0.766,0.866,0.962,1.05,1.14,1.22,1.30,1.37,1.45,1.52,1.58/


c --- external functions ---
      _RL       exf_BulkCdn
      external  exf_BulkCdn

c       UFocean=1e-2          !QQ what is this and what is the number?

c -- set all to zero
       flwup = 0.d0          ! upward long wave radiation
       flha  = 0.d0          ! latent heat flux
       fsha  = 0.d0          ! sensible heat flux
       df0dT = 0.d0          ! derivative of heat flux with respect to Tsf
       ust   = 0.d0          ! zonal wind stress (at grid center)
       vst   = 0.d0          ! meridional wind stress (at grid center)
       evp   = 0.d0          ! evaporation rate (over open water)
       ssq   = 0.d0          ! surface specific humidity (kg/kg)


       Qa=max(Qa,1e-7)
       Tsf=Tsf_in+Tf0kel
       ht=10.e0 
c Wind speed
       if (us.eq.0.d0) then    !in case we have a constant prescribed us
           us = sqrt(uw*uw + vw*vw)
       endif
       usm = max(us,1.d-5)
cQQQ try to limit drag
cQQQ    usm = min(usm,5.d0)

c from vinces surfflux.F
       if (iceornot.eq.0) then
c for open water
          rough=max(0.018*UFocean/gravity,0.5E-5)   !QQcheck
          elhx=Lvap   !QQcheck
       else
c for sea ice cover
          rough=10**(-3.37)   !QQcheck
          elhx = Lvap_ice
       endif
       Qg    =3.797915*exp(elhx*(7.93252e-6-2.166847E-3/Ta))/p0
c
c find stability of atmospheric surface layer stratification
       rough=log10(ht/rough)
       cdn=.0231/(rough*rough)   !QQcheck
       lr=rough*2.d0 - 5.d-1   !QQcheck
       lr=max(1,min(lr,20)) 
       rigs=ht*gravity*(Tsf-Ta)/(Ta*usm*usm)
       if (rigs.le.0.d0) then
c unstable
          dm=sqrt((1.d0-AROUGH(lr)*rigs)*(1.d0-BROUGH(lr)*rigs)/
     &        (1.d0-CROUGH(lr)*rigs))
          dh=1.35*sqrt((1.d0-DROUGH(lr)*rigs)/
     &        (1.d0-EROUGH(lr)*rigs))   !QQcheck
        else
c stable
          dm=1.d0/(1.d0+(11.238+89.9*rigs)*rigs)   !QQcheck
          dh=1.35/(1.d0+1.93*rigs)   !QQcheck
        endif
        cdm = cdn * dm
        cdh = cdn * dm * dh 

        rcdhws = cdh*us*100*p0/(Rgas*Tsf)  !QQ check units (take out 100)
        ssq    =3.797915*exp(elhx*(7.93252e-6-2.166847E-3/Tsf))/p0
cBB debugging
cBB             print*,'ssq', qcoef,  ssq
c

        if (Qa.le.ssq) then
           Tsv=Tsf
           ssq=ssq    !QQQQ should be ssq=qa?
        else
           dqssdt=ssq*elhx/(Rvap*Tsf*Tsf)
           x=(Qa-ssq)/(dqssdt+(sha/elhx))
           Tsv=Tsf+x
           ssq=Qa+x*(sha/elhx)
        endif

        deltap = Ta - Tsv    !tsv-Ta
        delq = Qg - ssq       !Qsv - Qg
cmine   deltap = ta  - tsf + gamma_blk*ht
cmine   delq   = Qa - ssq
c latent heat
c -------------
cvinc    lhcoef = Lvap*rcdhws
cmine latent heat coeff. following Gill 2.4.6 and 2.6.2 formula
        lhcoef =Lvap*rhoa*us*5e-3  !QQ name coeff. !QQwas 1.5e-3
        flha   = lhcoef*delq !QQQ
        evp    = lhcoef*delq/Lvap/rhosw
c sensible heat
c ----------------
cvinc        shcoef=sha*rcdhws
cmine sensible heat coeff. following Gill 2.4.5 formula
        stable = 5.d-1 + sign(5.d-1, deltap)
        if (stable.gt.0) then 
            shcoef = 0.83e-3*rhoa*us*cpair
        else
            shcoef = 1.10e-3*rhoa*us*cpair
        endif 
        fsha = shcoef*deltap

c long wave 
c ------------
cvinc      if (iceornot.eq.1) then
cvinc#ifdef ALLOW_TSEAICE
cvinc        if (snowheight.gt.0.d0) then
cvinc          surf_emissivity=snow_emissivity
cvinc        else
cvinc          surf_emissivity=ice_emissivity
cvinc        endif
cvinc#endif
cvinc      else
cvinc        surf_emissivity=ocean_emissivity
cvinc      endif
cvinc      flwup = -surf_emissivity*stefan*
cvinc     &         Tsf**4.0    !QQcheck
cvinc      flwdown = atm_emissivity*stefan *
cvinc     &          Ta**4.0 * (1.0+0.275*nc) *
cvinc     &          (1.0-0.261*exp(-7.77e-4*(273.15-Ta)**2.0))     !QQcheck
cvinc      flwup=flwup+flwdown                                      !QQQQQ??
cmine up (net) long wave radiation
              mixratio=Qa/(1-Qa)
              ea=p0*mixratio/(0.62197+mixratio)
              flwup=-0.985*stefan*tsf**4
     &                  *(0.39-0.05*sqrt(ea))
     &                  *(1-0.6*nc**2)
c -------------------------
           UFocean=cdm*us*us

#ifdef ALLOW_TSEAICE
       if (iceornot.eq.1) then
c derivatives of heat flux
c -------------------------
c          dflwupdT=-4.0*surf_emissivity*stefan *
c    &               Tsf**3.0     !QQcheck
cQQ corrected for Vince formula
           dflhdT = -lhcoef*ssq*Lvap*2.166847E-3/(Tsf**2)
           dfshdT = -shcoef   !QQQQQQ not quite right
cmine derivatives with respect to Tsf
cmine         dflhdT = -lhcoef*Tf0kel*ssq*22.47/(tsf**2)
cmine         dfshdT = -shcoef
              dflwupdT=-4.*0.985*stefan*tsf**3
     &                  *(0.39-0.05*sqrt(ea))
c
c total derivative with respect to surface temperature
              df0dT=dflwupdT+dfshdT+dflhdT
cBB
cBB     print*,'derivatives:',df0dT,dflwupdT,dfshdT,dflhdT
        endif
#endif /*ALLOW_TSEAICE*/
c
c wind stress at center points
c -----------------------------
        if (.NOT.readstress) then
                 ust = rhoa*exf_BulkCdn(usm)*us*uw
                 vst = rhoa*exf_BulkCdn(usm)*us*vw
        endif
#endif /*ALLOW_BULKFORMULA*/

      return
      end
