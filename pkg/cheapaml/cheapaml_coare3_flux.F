C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_coare3_flux.F,v 1.9 2012/10/09 20:15:43 wienders Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

C     !ROUTINE: CHEAPAML_COARE3_flux
C     !INTERFACE:
      subroutine cheapaml_COARE3_flux
     &(i,j,bi,bj,hf,ef,evap,Rnl,ssqt,q100,cdq)
c
      implicit none
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CHEAPAML.h"
#include "DYNVARS.h"

      integer iter,i,j,bi,bj,nits
      _RL hf,ef,evap,tau,L,psu,pst,Bf
      _RL CD,usr,tsr,qsr,q100,ssqt,ttas,essqt
      _RL zo,zot,zoq,RR,zL,pt,ttt,tta,ttt2
      _RL Rnl,es,twopi,cwave,lwave,cdq

c
c various constants
c
      _RL u,ts,q,zi,qs,tsw
      _RL psiu,psit,zot10,Ct10,CC,Ribu
      _RL Du,Wg,Dt,Dq,u10,zo10,Cd10,Ch10
      _RL xBeta,visa,Ribcu,QaR
      _RL Ct,zetu,L10,Tas,ta,charn

      _RL ssq0,           ssq1,           ssq2
      DATA   ssq0,           ssq1,           ssq2
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /

c
c
c Constants and coefficients (Stull 1988 p640).
      xBeta=1.2     !Given as 1.25 in Fairall et al.(1996)
      twopi=2. _d 0*pi
      visa=1.326 _d -5
c default relative humidity
      QaR=0.8 _d 0

c sea surface temperature without skin correction
      ts=theta(i,j,1,bi,bj)
      tsw=ts
      Tas=Tair(i,j,bi,bj)


c net upward long wave
      Rnl= 0.96*(stefan*(tsw+Celsius2K)**4) !Net longwave (up = +).
c
c Teten''s returns air svp es in mb
      es = (1.0007+3.46e-6*p0)*6.1121*dexp(17.502*tsw/(240.97+tsw)) !mb
      es=es*0.98                  !reduced for salinity Kraus 1972 p. 46
      !convert from mb to spec. humidity  kg/kg
      qs=.62197*es/(p0-0.378*es)
      tta=Tas+Celsius2K
      ttas=tta+gamma_blk*zt
      ttt=tta-(cheaphgrid(i,j,bi,bj) - zt)*gamma_blk
      ttt2=tta-(cheaphgrid(i,j,bi,bj) - zt)*gamma_blk-Celsius2K
      pt=p0*(1-gamma_blk*cheaphgrid(i,j,bi,bj)/ttas)
     $   **(gravity/gamma_blk/gasR)
      essqt = (1.0007+3.46e-6*pt)*6.1121*dexp(17.502*ttt2/(240.97+ttt2))
      !convert from mb to spec. humidity  kg/kg
      ssqt = .62197*essqt/(pt-0.378*essqt)
C      LANL formulation
C     saturation no more at the top:
      ssqt=ssq0*exp( lath*(ssq1-ssq2/tta) ) / p0


      if (useFreshWaterFlux)then
      q=qair(i,j,bi,bj)
      else
      q=QaR*ssqt
      endif


c Wave parameters
      cwave=gravity*wavesp(i,j,bi,bj)/twopi
      lwave=cwave*wavesp(i,j,bi,bj)
c
c Initial guesses
      zo=0.0001
      Wg=0.5                      !Gustiness factor initial guess
c
c Air-sea differences - includes warm layer in Dt and Dq
        u=(uwind(i,j,bi,bj)-uVel(i,j,1,bi,bj))**2+
     &(vwind(i,j,bi,bj)-vVel(i,j,1,bi,bj))**2
        u=dsqrt(u)
      Du=(u**2.+Wg**2.)**.5    !include gustiness in wind spd. difference
      Dt=tsw-Tas-gamma_blk*zt  !potential temperature difference.
      Dq=qs-q
c
c **************** neutral coefficients ******************
c
      u10=Du*dlog(10. _d 0/zo)/dlog(zu/zo)
      usr=0.035*u10
      zo10=0.011*usr*usr/gravity+0.11*visa/usr
      Cd10=(xkar/dlog(10. _d 0/zo10))**2
      Ch10=0.00115 _d 0
      Ct10=Ch10/sqrt(Cd10)
      zot10=10. _d 0/dexp(xkar/Ct10)
      Cd=(xkar/dlog(zu/zo10))**2

c standard coare3 boundary layer height
      zi=600. _d 0

c
c ************* Grachev and Fairall (JAM, 1997) **********
c
      ta=Tas+Celsius2K
      Ct=xkar/dlog(zt/zot10)   ! Temperature transfer coefficient
      CC=xkar*Ct/Cd            ! z/L vs Rib linear coefficient
      Ribcu=-zu/(zi*0.004 _d 0*xBeta**3)  ! Saturation or plateau Rib
      Ribu=-gravity*zu*(Dt+0.61 _d 0*ta*Dq)/(ta*Du**2)
      if (Ribu.lt.0. _d 0) then
          zetu=CC*Ribu/(1. _d 0+Ribu/Ribcu)   ! Unstable G and F
      else
          zetu=CC*Ribu*(1. _d 0 +27. _d 0/9. _d 0*Ribu/CC) ! Stable
      endif
      L10=zu/zetu                       ! MO length
      if (zetu.gt.50. _d 0) then
        nits=1
      else
        nits=3   ! number of iterations
      endif
c
c First guess M-O stability dependent
c scaling params.(u*,t*,q*) to estimate zo and z/L
c
      usr= Du*xkar/(dlog(zu/zo10)-psiu(zu/L10))
      tsr=-(Dt)*xkar/(dlog(zt/zot10)-psit(zt/L10))
      qsr=-(Dq)*xkar/(dlog(zq/zot10)-psit(zq/L10))
c
      charn=0.011 _d 0     !then modify Charnock for high wind speeds Chris data
      if(Du.gt.10. _d 0) charn=0.011 _d 0+(0.018-0.011)*(Du-10.)/(18.0-10.0)
      if(Du.gt.18. _d 0) charn=0.018 _d 0
c
c **** Iterate across u*(t*,q*),zo(zot,zoq) and z/L including cool skin ****
c
      do iter=1,nits
       if(WAVEMODEL.eq.'Smith') then
        zo=charn*usr*usr/gravity + 0.11 _d 0*visa/usr    !after Smith 1988
       else if(WAVEMODEL.eq.'Oost') then
        zo=(50./twopi)*lwave*(usr/cwave)**4.5 _d 0+0.11*visa/usr !Oost et al.
       else if(WAVEMODEL.eq.'TayYel') then
        zo=1200. _d 0*wavesh(i,j,bi,bj)*(wavesh(i,j,bi,bj)/lwave)**4.5
     & +0.11 _d 0*visa/usr !Taylor and Yelland
       endif
      rr=zo*usr/visa
c
c *** zoq and zot fitted to results from several ETL cruises ************
c
      zoq=min(1.15 _d -4,5.5 _d -5/rr**0.6 _d 0)
      zot=zoq
c
      zL=xkar*gravity*zu*(tsr*(1.+0.61*q)+0.61*ta*qsr)
     &   /(ta*usr*usr*(1. _d 0+0.61 _d 0*q))
      L=zu/zL
      psu=psiu(zu/L)
      pst=psit(zt/L)
      usr=Du*xkar/(dlog(zu/zo)-psiu(zu/L))
      tsr=-(Dt)*xkar/(dlog(zt/zot)-psit(zt/L))
      qsr=-(Dq)*xkar/(dlog(zq/zoq)-psit(zq/L))
      Bf=-gravity/ta*usr*(tsr+0.61 _d 0*ta*qsr)
       if (Bf.gt.0. _d 0) then
          Wg=xBeta*(Bf*zi)**.333 _d 0
       else
          Wg=0.2 _d 0
       endif
         Du=sqrt(u**2.+Wg**2.)        !include gustiness in wind spd.
       enddo

c compute surface fluxes and other parameters
       tau=rhoa*usr*usr*u/Du          !stress N/m2
       hf=-cpair*rhoa*usr*tsr           !sensible W/m2
       ef=-lath*rhoa*usr*qsr           !latent W/m2
       evap=-rhoa*usr*qsr
       cdq = evap/Dq
       if(.NOT.useStressOption)THEN
       ustress(i,j,bi,bj)=tau*(uwind(i,j,bi,bj)-uVel(i,j,1,bi,bj))/u
       vstress(i,j,bi,bj)=tau*(vwind(i,j,bi,bj)-vVel(i,j,1,bi,bj))/u
       endif
       q100=qs+qsr*(dlog(100. _d 0/zoq)-psit(100. _d 0/L))
c
      return
      end
c
c------------------------------------------------------------------
      function psiu(zL)
c
c psiu and psit evaluate stability function for wind speed and scalars
c matching Kansas and free convection forms with weighting f
c convective form follows Fairall et al (1996) with profile constants
c from Grachev et al (2000) BLM
c stable form from Beljaars and Holtslag (1991)
c
      implicit none
      _RL zL,x,y,psik,psic,f,psiu,c
      if(zL.lt.0.0) then
       x=(1.-15.*zL)**.25                        !Kansas unstable
       psik=2.*dlog((1.+x)/2.)+dlog((1.+x*x)/2.)-2.*atan(x)+2.*atan(1.)
       y=(1.-10.15*zL)**.3333                   !Convective
       psic=1.5*dlog((1.+y+y*y)/3.)-sqrt(3.)*atan((1.+2.*y)/sqrt(3.))
     &      +4.*atan(1.)/sqrt(3.)
       f=zL*zL/(1.+zL*zL)
       psiu=(1.-f)*psik+f*psic
      else
       c=min(50.,0.35*zL)                       !Stable
       psiu=-((1.+1.*zL)**1.+.6667*(zL-14.28)/dexp(c)+8.525)
      endif
      return
      end
c--------------------------------------------------------------
      function psit(zL)

      implicit none
      _RL zL,x,y,psik,psic,f,psit,c
      if(zL.lt.0.0) then
       x=(1.-15.*zL)**.5                          !Kansas unstable
       psik=2.*dlog((1.+x)/2.)
       y=(1.-34.15*zL)**.3333                    !Convective
       psic=1.5*dlog((1.+y+y*y)/3.)-sqrt(3.)*atan((1.+2.*y)/sqrt(3.))
     &      +4.*atan(1.)/sqrt(3.)
       f=zL*zL/(1.+zL*zL)
       psit=(1.-f)*psik+f*psic
      else
       c=min(50.,0.35*zL)                        !Stable
       psit=-((1.+2.*zL/3.)**1.5+.6667*(zL-14.28)/dexp(c)+8.525)
      endif
      return
      end

c-------------------------------------------------------------
