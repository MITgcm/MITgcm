C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_lanl_flux.F,v 1.3 2011/02/24 20:23:42 wienders Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"
#undef ALLOW_THSICE

      subroutine cheapaml_LANL_flux
     I(i,j,bi,bj,fsha,flha,evp,xolw,ssqt,q100)

c     ==================================================================
c     SUBROUTINE cheapaml_LANL_flux
c     ==================================================================
c
c     o compute surface fluxes using LANL algorithms
c       Output:
c       ------
c       ustress, vstress - wind stress
c       fsha - sensible heat flux
c       flha - latent heat flux
c       xolw - oceanic upwelled long wave radiation
c       ssqt - sat. specific humidity at atm layer top
c       ---------
c
c       Input:
c       ------
c       uwind, vwind  - mean wind speed (m/s)
c       Tair  - mean air temperature (K)  at height ht (m)
c       theta(k=1) - sea surface temperature (C)
c       Qair - Specific humidity kg/kg
c       Solar - short wave net solar flux at surface (W/m^2)
c	Tr - relaxation profile for temperature on boundaries (C)
c	qr - relaxation profile for specific humidity (kg/kg)
c       i,j,bi,bj - indices of data
c     ==================================================================
c     SUBROUTINE cheapaml_LANL_flux
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_THSICE
#include "THSICE_VARS.h"
#endif
#include "CHEAPAML.h"

      integer bi,bj
      integer i,j

c variables to include seaice effect
#ifdef ALLOW_THSICE
      _RL     tmp
      _RL     albedo
#endif      
      integer iceornot

C     == Local variables ==
c local variables
c zonal and meridional transports
        _RL deltaTm
        _RL uss,usm,uw,vw
        _RL cheapaml_BulkCdn
        _RL to
        _RL t
        _RL xolw
        _RL t0,QaR
        _RL ssq,ssq0,ssq1,ssq2,q,ssqt
        _RL deltap,delq,pt,q100,psx100,z100ol
        _RL rdn,ren,rhn,zice,zref
        _RL rd,re,rh,tta,ttas,toa,ttt
        _RL ustar,tstar,qstar,ht,hu,hq
        _RL aln,cdalton,czol,psim_fac
        _RL huol,stable,xsq,x,psimh,psixh
        _RL clha,csha,flha,fsha,evp
        integer niter_bulk,iter

c coefficients used to compute saturation specific humidity
      DATA   ssq0,           ssq1,           ssq2
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /

c useful values
c hardwire atmospheric relative humidity at 80%
        QaR=0.8 _d 0
c factor to compute rainfall from specific humidity
c inverse of time step
        deltaTm=1. _d 0/deltaT
c reference values to compute turbulent flux
		ht=zt
		hq=zq
		hu=zu
              zref = zt
     	      zice=.0005 _d 0
              aln = log(ht/zref)
c for iterating on turbulence
              niter_bulk = 5
              cdalton = 0.0346000 _d 0
              czol = zref*xkar*gravity
              psim_fac=5. _d 0

c     determine wind stress
        IF(.NOT.useStressOption)THEN

             if (maskC(i,j,1,bi,bj).ne.0. _d 0) then
#ifdef ALLOW_THSICE
               if (ICEMASK(i,j,bi,bj).gt.0. _d 0) then
                 tmp=Tsrf(i,j,bi,bj)
                if (snowheight(i,j,bi,bj).gt.3. _d -1) then
                   iceornot=2
                 else
                   iceornot=1
                 endif
               else
                 iceornot=0
               endif
#else
               iceornot=0
#endif
                       uw=uwind(i,j,bi,bj)
                       vw=vwind(i,j,bi,bj)
                       uss=sqrt(uw**2+vw**2)
                       usm=max(uss,1. _d 0)
                  cheapaml_BulkCdn = cdrag_1/usm + cdrag_2 + cdrag_3*usm
                       ustress(i,j,bi,bj)= rhoa*cheapaml_BulkCdn*uss*uw
                       vstress(i,j,bi,bj)= rhoa*cheapaml_BulkCdn*uss*vw
             else
               usm=0. _d 0
               ustress(i,j,bi,bj) = 0. _d 0
               vstress(i,j,bi,bj) = 0. _d 0
                endif
c wind stress computed
                ENDIF
c diabatic and freshwater flux forcing
        to=theta(i,j,1,bi,bj)
        t=Tair(i,j,bi,bj)
        toa=to+Celsius2K
        tta=t+Celsius2K
	    ttas=tta+gamma_blk*zref
        ttt=tta-(cheapaml_h - zref)*gamma_blk
	    pt=p0*(1-gamma_blk*cheapaml_h/ttas)**(gravity/gamma_blk/gasR)

c specific humidities
              ssq = ssq0*exp( lath*(ssq1-ssq2/toa) ) / p0
              ssqt = ssq0*exp( lath*(ssq1-ssq2/ttt) ) / pt

            if (useFreshwaterFlux) then
            q=qair(i,j,bi,bj)
            else
            q=QaR * ssq
            endif
c
c adjust temperature from reference height to formula height
            deltap = t  - to + gamma_blk*(zref-ht)
            delq   = q - ssq
            ttas   = tta+gamma_blk*(zref-ht)
            t0     = ttas*(1. _d 0 + humid_fac*q)

c initialize estimate exchange coefficients
              rdn=xkar/(log(zref/zice))
              rhn=rdn
              ren=rdn
c calculate turbulent scales
              ustar=rdn*usm
              tstar=rhn*deltap
              qstar=ren*delq
c
c iteration with psi-functions to find transfer coefficients
              do iter=1,niter_bulk
                 huol   = czol/ustar**2 *(tstar/t0 +
     &                    qstar/(1. _d 0/humid_fac+q))
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
                        usm=max(uss,0.5 _d 0)
                csha   = rhoa*cpair*usm*rh*rd
                clha   = rhoa*lath*usm*re*rd
c
                fsha  = csha*deltap
                flha  = clha*delq
                evp   = -flha/lath

c the sensible and latent heat fluxes, fsha and flha,
c are computed so that positive values are downward.
c the convention for cheapaml is upward fluxes are positive,
c so they must be multiplied by -1
        fsha=-fsha
        flha=-flha

c oceanic upwelled long wave
        xolw=stefan*(toa)**4
c compute specific humidity at 100m
                 huol   = czol/ustar**2 *(tstar/t0 +
     &                    qstar/(1. _d 0/humid_fac+q))
                 huol   = sign( min(abs(huol),10. _d 0), huol)
                 stable = 5. _d -1 + sign(5. _d -1 , huol)
                 z100ol   = 100. _d 0 *xkar*gravity/ustar**2 *(tstar/t0 +
     &                    qstar/(1. _d 0/humid_fac+q))
                 xsq = max(sqrt(abs(1. _d 0 - 16. _d 0*z100ol)),1. _d 0)
                 x      = sqrt(xsq)
                 psx100  = -5. _d 0*z100ol*stable + (1. _d 0-stable)*
     &                     (2. _d 0*log(5. _d -1*(1. _d 0+xsq)))
	             q100=ssq+qstar*(dlog(100. _d 0/zice)-psx100)

      RETURN
      END
