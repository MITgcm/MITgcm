C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_lanl_flux.F,v 1.6 2012/12/28 23:25:50 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"
#undef ALLOW_THSICE

CBOP
C     !ROUTINE: CHEAPAML_LANL_FLUX
C     !INTERFACE:
      SUBROUTINE CHEAPAML_LANL_FLUX(
     I                    i,j,bi,bj,
     O                    fsha, flha, evp, xolw, ssqt, q100 )

C     !DESCRIPTION:
C     ==================================================================
C     SUBROUTINE cheapaml_LANL_flux
C     ==================================================================
C     o compute surface fluxes using LANL algorithms

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
c#include "FFIELDS.h"
c#ifdef ALLOW_THSICE
c#include "THSICE_VARS.h"
c#endif
#include "CHEAPAML.h"

C     !INPUT PARAMETERS:
      INTEGER i,j,bi,bj
C     !OUTPUT PARAMETERS:
      _RL fsha, flha, evp, xolw, ssqt, q100
CEOP

C       Output:
C       ustress, vstress - wind stress
C       fsha - sensible heat flux
C       flha - latent heat flux
C       xolw - oceanic upwelled long wave radiation
C       ssqt - sat. specific humidity at atm layer top

C       Input:
C       uwind, vwind  - mean wind speed (m/s)
C       Tair  - mean air temperature (K)  at height ht (m)
C       theta(k=1) - sea surface temperature (C)
C       Qair - Specific humidity kg/kg
C       Solar - short wave net solar flux at surface (W/m^2)
C       Tr - relaxation profile for temperature on boundaries (C)
C       qr - relaxation profile for specific humidity (kg/kg)
C       i,j,bi,bj - indices of data

C     !LOCAL VARIABLES:
C     iceornot :: variables to include seaice effect
      INTEGER iceornot
      _RL deltaTm
      _RL uss,usm,uw,vw
      _RL cheapaml_BulkCdn
      _RL to
      _RL t
      _RL t0,QaR
      _RL ssq, q
      _RL deltap, delq, pt, psx100, z100ol
      _RL rdn,ren,rhn,zice,zref
      _RL rd,re,rh,tta,ttas,toa,ttt
      _RL ustar,tstar,qstar,ht,hu,hq
      _RL aln,cdalton,czol,psim_fac
      _RL huol,stable,xsq,x,psimh,psixh
      _RL clha, csha
      INTEGER niter_bulk,iter

C useful values
C hardwire atmospheric relative humidity at 80%
        QaR=0.8 _d 0
C factor to compute rainfall from specific humidity
C inverse of time step
        deltaTm=1. _d 0/deltaT
C reference values to compute turbulent flux
              ht=zt
              hq=zq
              hu=zu
              zref = zt
              zice=.0005 _d 0
              aln = log(ht/zref)
C for iterating on turbulence
              niter_bulk = 5
              cdalton = 0.0346000 _d 0
              czol = zref*xkar*gravity
              psim_fac=5. _d 0

C     determine wind stress
        IF(.NOT.useStressOption)THEN

             if (maskC(i,j,1,bi,bj).ne.0. _d 0) then
#ifdef ALLOW_THSICE
               if (ICEMASK(i,j,bi,bj).gt.0. _d 0) then
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
C wind stress computed
                ENDIF
C diabatic and freshwater flux forcing
        to=theta(i,j,1,bi,bj)
        t=Tair(i,j,bi,bj)
        toa=to+Celsius2K
        tta=t+Celsius2K
        ttas=tta+gamma_blk*zref
        ttt=tta-( cheaphgrid(i,j,bi,bj)- zref)*gamma_blk
        pt=p0*(1-gamma_blk*cheaphgrid(i,j,bi,bj)/ttas)
     &     **(gravity/gamma_blk/gasR)

C specific humidities
              ssq= ssq0*exp( lath*(ssq1-ssq2/toa) ) / p0
              ssqt = ssq0*exp( lath*(ssq1-ssq2/ttt) ) / pt
C     saturation no more at the top:
              ssqt = 0.7 _d 0*ssq

            if (useFreshWaterFlux) then
            q=qair(i,j,bi,bj)
            else
            q=QaR * ssq
            endif

C adjust temperature from reference height to formula height
            deltap = t  - to + gamma_blk*(zref-ht)
            delq   = q - ssq
            ttas   = tta+gamma_blk*(zref-ht)
            t0     = ttas*(1. _d 0 + humid_fac*q)

C initialize estimate exchange coefficients
              rdn=xkar/(log(zref/zice))
              rhn=rdn
              ren=rdn
C calculate turbulent scales
              ustar=rdn*usm
              tstar=rhn*deltap
              qstar=ren*delq

C iteration with psi-functions to find transfer coefficients
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

C Update the transfer coefficients

                 rd = rdn/(1. _d 0 + rdn*(aln-psimh)/xkar)
                 rh = rhn/(1. _d 0 + rhn*(aln-psixh)/xkar)
                 re = rh
C  Update ustar, tstar, qstar using updated, shifted coefficients.
                 ustar = rd*usm
                 qstar = re*delq
                 tstar = rh*deltap
              enddo

                        usm=max(uss,0.5 _d 0)
                csha   = rhoa*cpair*usm*rh*rd
                clha   = rhoa*lath*usm*re*rd

                fsha  = csha*deltap
                flha  = clha*delq
                evp   = -flha/lath

C the sensible and latent heat fluxes, fsha and flha,
C are computed so that positive values are downward.
C the convention for cheapaml is upward fluxes are positive,
C so they must be multiplied by -1
        fsha=-fsha
        flha=-flha

C oceanic upwelled long wave
        xolw=stefan*(toa)**4
C compute specific humidity at 100m
                 huol   = czol/ustar**2 *(tstar/t0 +
     &                    qstar/(1. _d 0/humid_fac+q))
                 huol   = sign( min(abs(huol),10. _d 0), huol)
                 stable = 5. _d -1 + sign(5. _d -1 , huol)
                 z100ol   = 100. _d 0 *xkar*gravity/ustar**2 *(tstar/t0
     &                    + qstar/(1. _d 0/humid_fac+q))
                 xsq = max(sqrt(abs(1. _d 0 - 16. _d 0*z100ol)),1. _d 0)
                 x      = sqrt(xsq)
                 psx100  = -5. _d 0*z100ol*stable + (1. _d 0-stable)*
     &                     (2. _d 0*log(5. _d -1*(1. _d 0+xsq)))
                 q100=ssq+qstar*(dlog(100. _d 0/zice)-psx100)

      RETURN
      END
