C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml.F,v 1.1 2008/08/05 21:49:31 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"
#undef ALLOW_THSICE

      subroutine cheapaml(
     I                           myTime, myIter, mythid )

c     ==================================================================
c     SUBROUTINE cheapaml
c     ==================================================================
c
c     o Get the surface fluxes used to force ocean model
c       Output:
c       ------
c       ustress, vstress - wind stress
c       Qnet             - net heat flux
c       ---------
c
c       Input:
c       ------
c       uwind, vwind  - mean wind speed (m/s)
c       Tair  - mean air temperature (K)  at height ht (m)
c       theta(k=1) - sea surface temperature (C)
c
c     ==================================================================
c     SUBROUTINE cheapaml 
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
c#include "BULKF_PARAMS.h"
c#include "BULKF.h"
c#include "BULKF_INT.h"
c#include "BULKF_DIAG.h"
#ifdef ALLOW_THSICE
#include "THSICE_VARS.h"
#endif
#include "CHEAPAML.h"
c     == routine arguments ==

      integer mythid
      integer myIter
      _RL     myTime

C     == Local variables ==
      integer bi,bj
      integer i,j,k,nt

c  integer cheapaml_ntim atmospheric timesteps per ocean time step

c variables to include seaice effect
      _RL     tmp
      _RL     albedo
      integer iceornot

c local variables
c tendencies of atmospheric temperature, current and past
        _RL gTair(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
c       _RL gTairm(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
c zonal and meridional transports
        _RL uTrans(1-olx:snx+olx,1-oly:sny+oly)
        _RL vTrans(1-olx:snx+olx,1-oly:sny+oly)
C       AML timestep
        _RL deltaTTracer
        _RL uss,usm,uw,vw,hm
        _RL cheapaml_BulkCdn
        _RL xrel,to
        _RL xgs,t
        _RL xef
        _RL xefi
        _RL dtemp,xflu,xfld,xrelf
        _RL xgamm,xgam,xalw,xolw
        _RL t0,humid_fac,Qa,gamma_blk
        _RL ssq,ssq0,ssq1,ssq2
        _RL lath,p0,deltap,delq
        _RL rdn,ren,rhn,xkar,zice,zref
        _RL rd,re,rh,tta,toa
        _RL ustar,tstar,qstar,ht,hu,hq
c       _RL gravity,aln,cdalton,czol,psim_fac
        _RL aln,cdalton,czol,psim_fac
        _RL huol,stable,xsq,x,psimh,psixh
        _RL clha,csha,flha,fsha,evp
        integer niter_bulk,iter

c External functions
c       EXTERNAL  
c     $   cheapaml_BulkCdn

c coefficients used to compute saturation specific humidity
      DATA   ssq0,           ssq1,           ssq2 
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /

c useful values
c hardwire atmospheric relative humidity at 80%
        Qa=0.8d0
c atmospheric adiabatic lapse rate
        gamma_blk=0.01d0
c humidity factor for computing virtual potential temperature
        humid_fac=0.606d0
c surface pressure in mb
        p0=1000.d0
c latent heat (J/kg)
        lath=2.5d6
c reference values to compute turbulent flux
              ht =  2. _d 0
              hq =  2. _d 0
              hu = 10. _d 0
              zref = 10. _d 0
        zice=.0005d0
c von Karman constant
        xkar=0.4d0
c set gravity
c       gravity=9.81d0
              aln = log(ht/zref)
c for iterating on turbulence
              niter_bulk = 5
              cdalton = 0.0346000 _d 0
              czol = zref*xkar*gravity
              psim_fac=5. _d 0


c       write(*,*) 'i,j,bi,bj, latent, sensible, long, short, qnet'


c relaxation time scales from input
c cheapaml_taurelax1 holds over the ocean
c cheapaml_taurelax2 holds over the buffer zones
        xrel=1.d0/cheapaml_taurelax1/8.64d4
        xgs=1.d0/cheapaml_taurelax2/8.64d4
        xrelf=1.d0/(1.d0+xgs*deltaT)

c energy flux conversion factors
        xef=1.d0/rhoa/cpair
        xefi=rhoa*cpair

c inverse of constant atmosphere mixed layer
c h from input
        hm=1.d0/cheapaml_h

C       timestep
        deltaTtracer = deltaT/FLOAT(cheapaml_ntim)

c     determine wind stress
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do j = 1-oly,sny+oly
           do i = 1-olx,snx+olx
             if (hFacC(i,j,1,bi,bj).ne.0. _d 0) then
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
c                      ustress(i,j,bi,bj)= rhoa*cheapaml_BulkCdn(usm)*uss*uw
c                      vstress(i,j,bi,bj)= rhoa*cheapaml_BulkCdn(usm)*uss*vw
                       ustress(i,j,bi,bj)= rhoa*cheapaml_BulkCdn*uss*uw
                       vstress(i,j,bi,bj)= rhoa*cheapaml_BulkCdn*uss*vw
             else
               ustress(i,j,bi,bj) = 0. _d 0
               vstress(i,j,bi,bj) = 0. _d 0
                endif
                enddo
                enddo
c wind stress computed
c initialize net heat flux array
          do j = 1-oly,sny+oly
           do i = 1-olx,snx+olx
        Qnet(i,j,bi,bj)=0.d0
        enddo
        enddo

c close bi, bj loops
        enddo
        enddo

c this is a reprogramming to speed up cheapaml
c the short atmospheric time step is applied to
c advection and diffusion only.  diabatic forcing is computed
c once and used for the entire oceanic time step.  

c cycle through atmospheric advective/diffusive 
c surface temperature evolution
        do nt=1,cheapaml_ntim
       DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

c compute advective and diffusive flux divergence

         do j=1-oly,sny+oly
         do i=1-olx,snx+olx
         gTair(i,j,bi,bj)=0.d0
         enddo
         enddo
         call GAD_2d_CALC_RHS( 
     I           bi,bj,1-olx,nsx+olx,1-oly,nsy+oly,
     I           uTrans,vTrans,
     I           uwind, vwind, 
     I           cheapaml_kdiff, Tair,
     U           gTair,
     I           myTime, myIter, myThid )
        call adams2d( 
     I                     bi, bj, 
     U                     gTair, gTairm,
     I                     nt-1, myThid )
c       WRITE(*,*) 'gTair ',nt  
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML Tair',1,myThid)
c       WRITE(*,*) 'Tair ',nt   
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      write(*,*)'Qnet before timestep',nt
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid) 

        call timestep_2d_tracer(
     I                     bi, bj, 1-olx,snx+olx ,1-oly, sny+oly, 
     I                     deltaTtracer,
     I                     Tair, gTair,
     I                     myIter, myThid )
c       WRITE(*,*) 'gTair ',nt  
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML Tair',1,myThid)
c       WRITE(*,*) 'Tair ',nt   
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      write(*,*)'Qnet before cycle',nt
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid) 

        call cycle_2d_tracer(
     I                   bi, bj, 
     U                   Tair, gTair,
     I                   myTime, myIter, myThid )
c       WRITE(*,*) 'gTair ',nt  
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML Tair',1,myThid)
c       WRITE(*,*) 'Tair ',nt   
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      write(*,*)'Qnet after cycle ',nt
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid) 
c       write(*,*)Qnet  
c close bi,bj loops
        enddo
        enddo
c update edges

         _EXCH_XY_R8(Tair,mythid)
         _EXCH_XY_R8(Qnet,mythid)
c       WRITE(*,*) 'gTair ',nt  
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML Tair',1,myThid)
c       WRITE(*,*) 'Tair ',nt   
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      write(*,*)'Qnet before adams',nt
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid) 
c       STOP
        enddo
c cycling on short atmospheric time step is now done

c now continue with diabatic forcing
       DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        do j=1-oly,sny+oly
        do i=1-olx,snx+olx
        to=theta(i,j,1,bi,bj)
        t=Tair(i,j,bi,bj)
        toa=to+273.16d0
        tta=t+273.16d0


              ssq = ssq0*exp( lath*(ssq1-ssq2/toa) ) / p0
              t0     = tta*(1. _d 0 + humid_fac*Qa*ssq)
              deltap = t  - to + gamma_blk*ht
              delq   = (Qa - 1)* ssq
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
c iteration with psi-functions to find transfer coefficients
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
                        uw=uwind(i,j,bi,bj)
                        vw=vwind(i,j,bi,bj)
                        uss=sqrt(uw**2+vw**2)
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

        
c atmospheric upwelled long wave
        xalw=stefan*(t+273.16d0)**4*0.5d0
c oceanic upwelled long wave
        xolw=stefan*(to+273.16d0)**4
c total flux at upper mixed layer interface 
        xflu=(-solar(i,j,bi,bj)+xalw+flha)*xef*hfacC(i,j,1,bi,bj)
c lower flux calculation. will switch to relaxation if over buffer
c       xrelf=xgs*(t-tr(i,j,bi,bj))*(1-hfacC(i,j,1,bi,bj))*cheapaml_h
        xfld=(-solar(i,j,bi,bj)-xalw+xolw+fsha+flha)
     .*xef*hfacC(i,j,1,bi,bj)
c     .-xrelf
c add flux divergences into atmospheric temperature tendency
        gTair(i,j,bi,bj)=(xfld-xflu)*hm
c       Qnet(i,j,bi,bj)=Qnet(i,j,bi,bj)+xfld*xefi/cheapaml_ntim*
c     .hfacC(i,j,1,bi,bj)
c       Qnet(i,j,bi,bj)=Qnet(i,j,bi,bj)+xfld*xefi/cheapaml_ntim
c       Qnet=Qnet+(-solar(i,j,bi,bj)-xalw+xolw+fsha+flha)
c     .*xef*hfacC(i,j,1,bi,bj)
        Qnet(i,j,bi,bj)=(-solar(i,j,bi,bj)-xalw+xolw+fsha+flha)
     .*hfacC(i,j,1,bi,bj)

c      write(*,*) i,j,bi,bj,flha,fsha,-xalw+xolw,-solar(i,j,bi,bj),
c     .Qnet(i,j,bi,bj)
                                
                              
                              
        enddo
        enddo
        call adams2d( 
     I                     bi, bj, 
     U                     gTair, gTairm,
     I                          0,myThid)


c the value 0 is used in the above line, because we
c always assume gTairm is not known.  this is because
c the diabatic forcing is diagnosed from the SST, and 
c done locally.  
c     I                     myIter, myThid )
c       WRITE(*,*) 'gTair ',nt  
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML Tair',1,myThid)
c       WRITE(*,*) 'Tair ',nt   
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      write(*,*)'Qnet before timestep',nt
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid) 

        call timestep_2d_tracer(
     I                     bi, bj, 1-olx,snx+olx ,1-oly, sny+oly, 
     I                     deltaT,
     I                     Tair, gTair,
     I                     myIter, myThid )
c full oceanic time step deltaT is used in the above
c       WRITE(*,*) 'gTair ',nt  
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML Tair',1,myThid)
c       WRITE(*,*) 'Tair ',nt   
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      write(*,*)'Qnet before cycle',nt
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid) 

        call cycle_2d_tracer(
     I                   bi, bj, 
     U                   Tair, gTair,
     I                   myTime, myIter, myThid )
c       WRITE(*,*) 'gTair ',nt  
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML Tair',1,myThid)
c       WRITE(*,*) 'Tair ',nt   
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      write(*,*)'Qnet after cycle ',nt
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid) 
c       write(*,*)Qnet  
c       do implicit time stepping over land
        do j=1-oly,sny+oly
        do i=1-olx,snx+olx
        dtemp=tr(i,j,bi,bj)-Tair(i,j,bi,bj)     
        Tair(i,j,bi,bj)=Tair(i,j,bi,bj)+xgs*deltaT*dtemp
     .*xrelf*(1.d0-hFacC(i,j,1,bi,bj))
        enddo
        enddo
c close bi,bj loops
        enddo
        enddo
c update edges

         _EXCH_XY_R8(Tair,mythid)
         _EXCH_XY_R8(Qnet,mythid)
c       WRITE(*,*) 'gTair ',nt  
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML Tair',1,myThid)
c       WRITE(*,*) 'Tair ',nt   
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      write(*,*)'Qnet before adams',nt
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid) 

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
cswd move wind stresses to u and v points
           DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx+1,sNx+Olx
              fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)
     &          *(ustress(i,j,bi,bj)+ustress(i-1,j,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO
           DO j = 1-Oly+1,sNy+Oly
            DO i = 1-Olx,sNx+Olx
              fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)
     &          *(vstress(i,j,bi,bj)+vstress(i,j-1,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO

c
c        IF ( blk_taveFreq.GT.0. _d 0 )
c     &    CALL BULKF_AVE(bi,bj,mythid)

C--   end bi,bj loops
       ENDDO
      ENDDO


      RETURN
      END
