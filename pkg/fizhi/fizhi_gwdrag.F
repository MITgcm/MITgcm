C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_gwdrag.F,v 1.1 2005/05/20 23:50:52 molod Exp $
C $Name:  $
#include "FIZHI_OPTIONS.h"
      subroutine gwdrag (myid,pz,pl,ple,dpres,pkz,uz,vz,tz,qz,phis_var,
     .         dudt,dvdt,dtdt,im,jm,lm,bi,bj,istrip,npcs,imglobal)
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Driver Routine for Gravity Wave Drag
C
C  INPUT:
C  ======
C  myid  ....... Process ID
C  pz    ....... Surface Pressure [im,jm]
C  pl    ....... 3D pressure field [im,jm,lm]
C  ple   ....... 3d pressure at model level edges [im,jm,lm+1]
C  dpres ....... pressure difference across level [im,jm,lm]
C  pkz   ....... pressure**kappa [im,jm,lm]
C  uz    ....... zonal velocity [im,jm,lm]
C  vz    ....... meridional velocity [im,jm,lm]
C  tz    ....... temperature [im,jm,lm]
C  qz    ....... specific humidity [im,jm,lm]
C  phis_var .... topography variance
C  im    ....... number of grid points in x direction
C  jm    ....... number of grid points in y direction
C  lm    ....... number of grid points in vertical
C  istrip ...... 'strip' length for cache size control
C  npcs  ....... number of strips
C  imglobal .... (avg) number of longitude points around the globe
C
C  INPUT/OUTPUT:
C  ============
C  dudt  ....... Updated U-Wind   Tendency including Gravity Wave Drag
C  dvdt  ....... Updated V-Wind   Tendency including Gravity Wave Drag
C  dtdt  ....... Updated Pi*Theta Tendency including Gravity Wave Drag
C
C***********************************************************************
      implicit none

#ifdef ALLOW_DIAGNOSTICS
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#endif

c Input Variables
c ---------------
      integer myid,im,jm,lm,bi,bj,istrip,npcs,imglobal
      real pz(im,jm)
      real pl(im,jm,lm)
      real ple(im,jm,lm+1)
      real dpres(im,jm,lm)
      real pkz(im,jm,lm)
      real uz(im,jm,lm)
      real vz(im,jm,lm)
      real tz(im,jm,lm)
      real qz(im,jm,lm)
      real phis_var(im,jm)

      real dudt(im,jm,lm)
      real dvdt(im,jm,lm)
      real dtdt(im,jm,lm)

c Local Variables
c ---------------
      real tv(im,jm,lm)
      real dragu(im,jm,lm), dragv(im,jm,lm)
      real dragt(im,jm,lm) 
      real dragx(im,jm), dragy(im,jm)
      real sumu(im,jm)
      integer nthin(im,jm),nbase(im,jm)
      integer nthini, nbasei

      real phis_std(im,jm)

      real std(istrip), ps(istrip)
      real us(istrip,lm), vs(istrip,lm), ts(istrip,lm)
      real dragus(istrip,lm), dragvs(istrip,lm) 
      real dragxs(istrip), dragys(istrip)
      real plstr(istrip,lm),plestr(istrip,lm),dpresstr(istrip,lm)
      integer nthinstr(istrip),nbasestr(istrip)

      integer n,i,j,L
      real    getcon, pi
      real    grav, rgas, cp, cpinv, lstar

c Initialization
c --------------
      pi    = 4.0*atan(1.0)
      grav  = getcon('GRAVITY')
      rgas  = getcon('RGAS')
      cp    = getcon('CP')
      cpinv = 1.0/cp
      lstar = 2*getcon('EARTH RADIUS')*cos(pi/3.0)/imglobal

c Compute NTHIN and NBASE
c -----------------------
      do j=1,jm
      do i=1,im

      do nthini = 1,lm+1
       if( 1000.0-ple(i,j,lm+2-nthini).gt.25. ) then
        nthin(i,j) = nthini
        goto 10
       endif
      enddo
   10 continue
      do nbasei = 1,lm+1
       if( ple(i,j,lm+2-nbasei).lt.666.7 ) then
        nbase(i,j) = nbasei
        goto 20
       endif
      enddo
   20 continue
      if( 666.7-ple(i,j,lm+2-nbase(i,j)) .gt. 
     .           ple(i,j,lm+3-nbase(i,j))-666.7 ) then
      nbase(i,j) = nbase(i,j)-1
      endif

      enddo
      enddo
c Compute Topography Sub-Grid Standard Deviation
c ----------------------------------------------
      do j=1,jm
      do i=1,im
      phis_std(i,j) = min( 400.0, sqrt( max(0.0,phis_var(i,j)) )/grav )
      enddo
      enddo

c Compute Virtual Temperatures
c ----------------------------
      do L = 1,lm
      do j = 1,jm
      do i = 1,im
      tv(i,j,L) = tz(i,j,L)*pkz(i,j,L)*(1.+.609*qz(i,j,L))
      enddo
      enddo
      enddo

c Call Gravity Wave Drag Paramterization on A-Grid
c ------------------------------------------------

      do n=1,npcs

      call strip ( phis_std,std,im*jm,istrip,1,n )

      call strip ( pz,ps,im*jm,istrip,1 ,n )
      call strip ( uz,us,im*jm,istrip,lm,n )
      call strip ( vz,vs,im*jm,istrip,lm,n )
      call strip ( tv,ts,im*jm,istrip,lm,n )
      call strip ( pl,plstr,im*jm,istrip,lm,n )
      call strip ( ple,plestr,im*jm,istrip,lm,n )
      call strip ( dpres,dpresstr,im*jm,istrip,lm,n )
      call stripint ( nthin,nthinstr,im*jm,istrip,lm,n )
      call stripint ( nbase,nbasestr,im*jm,istrip,lm,n )

      call GWDD ( ps,us,vs,ts,
     .            dragus,dragvs,dragxs,dragys,std,
     .            plstr,plestr,dpresstr,grav,rgas,cp,
     .            istrip,lm,nthinstr,nbasestr,lstar )

      call paste ( dragus,dragu,istrip,im*jm,lm,n )
      call paste ( dragvs,dragv,istrip,im*jm,lm,n )
      call paste ( dragxs,dragx,istrip,im*jm,1 ,n )
      call paste ( dragys,dragy,istrip,im*jm,1 ,n )

      enddo

c Add Gravity-Wave Drag to Wind and Theta Tendencies
c -------------------------------------------------- 
      do L = 1,lm
      do j = 1,jm
      do i = 1,im
      dragu(i,j,L) = sign( min(0.006,abs(dragu(i,j,L))),dragu(i,j,L) )
      dragv(i,j,L) = sign( min(0.006,abs(dragv(i,j,L))),dragv(i,j,L) )
      dragt(i,j,L) = -( uz(i,j,L)*dragu(i,j,L)+vz(i,j,L)*dragv(i,j,L) )
     .                                                         *cpinv
       dudt(i,j,L) = dudt(i,j,L) + dragu(i,j,L)
       dvdt(i,j,L) = dvdt(i,j,L) + dragv(i,j,L)
       dtdt(i,j,L) = dtdt(i,j,L) + dragt(i,j,L)*pz(i,j)/pkz(i,j,L)
      enddo
      enddo
      enddo

c Compute Diagnostics
c -------------------
      if( igwdu.ne.0 .or. igwdv.ne.0 .or. igwdt.ne.0 ) then
      do L = 1,lm
       if( igwdu.ne.0 ) then
        do j = 1,jm
        do i = 1,im
        qdiag(i,j,igwdu+L-1,bi,bj) = qdiag(i,j,igwdu+L-1,bi,bj) + 
     .                                         dragu(i,j,L)*86400
        enddo
        enddo
       endif
       if( igwdv.ne.0 ) then
        do j = 1,jm
        do i = 1,im
        qdiag(i,j,igwdv+L-1,bi,bj) = qdiag(i,j,igwdv+L-1,bi,bj) + 
     .                                         dragv(i,j,L)*86400
        enddo
        enddo
       endif
       if( igwdt.ne.0 ) then
        do j = 1,jm
        do i = 1,im
        qdiag(i,j,igwdt+L-1,bi,bj) = qdiag(i,j,igwdt+L-1,bi,bj) + 
     .                                         dragt(i,j,L)*86400
        enddo
        enddo
       endif
      enddo
      endif

c Gravity Wave Drag at Surface (U-Wind)
c -------------------------------------
      if( igwdus.ne.0 ) then
      do j = 1,jm
      do i = 1,im
      qdiag(i,j,igwdus,bi,bj) = qdiag(i,j,igwdus,bi,bj) + dragx(i,j)
      enddo
      enddo
      endif

c Gravity Wave Drag at Surface (V-Wind)
c -------------------------------------
      if( igwdvs.ne.0 ) then
      do j = 1,jm
      do i = 1,im
      qdiag(i,j,igwdvs,bi,bj) = qdiag(i,j,igwdvs,bi,bj) + dragy(i,j)
      enddo
      enddo
      endif

c Gravity Wave Drag at Model Top (U-Wind)
c ---------------------------------------
      if( igwdut.ne.0 ) then
      do j = 1,jm
      do i = 1,im
      sumu(i,j) = 0.0
      enddo
      enddo
      do L = 1,lm
      do j = 1,jm
      do i = 1,im
      sumu(i,j) = sumu(i,j) + dragu(i,j,L)*dpres(i,j,L)/pz(i,j)
      enddo
      enddo
      enddo
      do j = 1,jm
      do i = 1,im
      qdiag(i,j,igwdut,bi,bj) = qdiag(i,j,igwdut,bi,bj) + dragx(i,j)
     .                  + sumu(i,j)*pz(i,j)/grav*100
      enddo
      enddo
      endif

c Gravity Wave Drag at Model Top (V-Wind)
c ---------------------------------------
      if( igwdvt.ne.0 ) then
      do j = 1,jm
      do i = 1,im
      sumu(i,j) = 0.0
      enddo
      enddo
      do L = 1,lm
      do j = 1,jm
      do i = 1,im
      sumu(i,j) = sumu(i,j) + dragv(i,j,L)*dpres(i,j,L)/pz(i,j)
      enddo
      enddo
      enddo
      do j = 1,jm
      do i = 1,im
      qdiag(i,j,igwdvt,bi,bj) = qdiag(i,j,igwdvt,bi,bj) + dragy(i,j)
     .                  + sumu(i,j)*pz(i,j)/grav*100
      enddo
      enddo
      endif

      ngwdu  = ngwdu  + 1
      ngwdv  = ngwdv  + 1
      ngwdt  = ngwdt  + 1
      ngwdus = ngwdus + 1
      ngwdvs = ngwdvs + 1
      ngwdut = ngwdut + 1
      ngwdvt = ngwdvt + 1

      return
      end
      SUBROUTINE GWDD ( ps,u,v,t,dudt,dvdt,xdrag,ydrag,
     .                  std,pl,ple,dpres,
     .                  grav,rgas,cp,irun,lm,nthin,nbase,lstar )
C***********************************************************************
C
C Description:
C  ============
C    Parameterization to introduce a Gravity Wave Drag
C    due to sub-grid scale orographic forcing
C
C Input:
C  ======
C    ps ......... Surface Pressure
C    u .......... Zonal      Wind (m/sec)
C    v .......... Meridional Wind (m/sec)
C    t .......... Virtual Temperature (deg K)
C    std ........ Standard Deviation of sub-grid Orography (m)
C    ple  ....... Model pressure Edge Values
C    pl  ........ Model pressure Values
C    dpres....... Model Delta pressure Values
C    grav ....... Gravitational constant (m/sec**2)
C    rgas ....... Gas constant
C    cp ......... Specific Heat at constant pressure
C    irun ....... Number of grid-points in horizontal dimension
C    lm ......... Number of grid-points in vertical   dimension
C    lstar ...... Monochromatic Wavelength/(2*pi)
C
C Output:
C  =======
C    dudt ....... Zonal Acceleration due to GW Drag (m/sec**2)
C    dvdt ....... Meridional Acceleration due to GW Drag (m/sec**2)
C    xdrag ...... Zonal Surface and Base Layer Stress (Pa)
C    ydrag ...... Meridional Surface and Base Layer Stress (Pa)
C
C***********************************************************************

      implicit none

c Input Variables
c ---------------
      integer irun,lm
      real ps(irun)
      real u(irun,lm), v(irun,lm), t(irun,lm)
      real dudt(irun,lm), dvdt(irun,lm)
      real xdrag(irun), ydrag(irun)
      real std(irun)
      real ple(irun,lm+1), pl(irun,lm), dpres(irun,lm)
      real grav, rgas, cp
      integer nthin(irun),nbase(irun)
      real lstar

c Dynamic Allocation Variables
c ----------------------------
      real ubar(irun), vbar(irun), robar(irun)
      real speed(irun), ang(irun)
      real bv(irun,lm)
      real nbar(irun)

      real tstd(irun)
      real XTENS(irun,lm+1), YTENS(irun,lm+1)
      real TENSIO(irun,lm+1)
      real DRAGSF(irun)
      real RO(irun,lm), DZ(irun,lm)

      integer icrilv(irun)

c Local Variables
c ---------------
      integer  i,l
      real a,g,stdmax,agrav,akwnmb
      real gocp,roave,roiave,frsf,gstar,vai1,vai2
      real vaisd,velco,deluu,delvv,delve2,delz,vsqua
      real richsn,crifro,crif2,fro2,coef

c Initialization
c --------------
      a      = 1.0
      g      = 1.0
      agrav  = 1.0/GRAV
      akwnmb = 1.0/lstar
      gocp   = GRAV/CP

c Constrain the Maximum Value of the Standard Deviation
c -----------------------------------------------------
      stdmax = 400.
      do i = 1,irun
         tstd(i) = std(i)
      if( std(i).gt.stdmax ) tstd(i) = stdmax
      enddo

c Compute Atmospheric Density
c ---------------------------
      do l = 1,lm
      do i = 1,irun
      ro(i,l) = pl(i,l)/(rgas*t(i,lm+1-l))
      enddo
      enddo

c Compute Layer Thicknesses
c -------------------------
      do l = 2,lm
      do i = 1,irun
      roiave  = ( 1./ro(i,l-1) + 1./ro(i,l) )*0.5
      dz(i,l) = agrav*roiave*( pl(i,l-1)-pl(i,l) )
      enddo
      enddo


c******************************************************
c          Surface and Base Layer Stress              *
c******************************************************

c Definition of Surface Wind Vector
c ---------------------------------
      do  i = 1,irun
      robar(i) = 0.0
       ubar(i) = 0.0
       vbar(i) = 0.0
      enddo

      do  i = 1,irun
      do  L = 1,nbase(i)-1
      robar(i) = robar(i) + ro(i,L)     *(ple(i,L)-ple(i,L+1))
       ubar(i) =  ubar(i) +  u(i,lm+1-L)*(ple(i,L)-ple(i,L+1))
       vbar(i) =  vbar(i) +  v(i,lm+1-L)*(ple(i,L)-ple(i,L+1))
      enddo
      enddo

      do  i = 1,irun
      robar(i) = robar(i)/(ple(i,1)-ple(i,nbase(i))) * 100.0
       ubar(i) =  ubar(i)/(ple(i,1)-ple(i,nbase(i)))
       vbar(i) =  vbar(i)/(ple(i,1)-ple(i,nbase(i)))

      speed(i) = SQRT( ubar(i)*ubar(i) + vbar(i)*vbar(i) )
        ang(i) = ATAN2(vbar(i),ubar(i))

      enddo

c Brunt Vaisala Frequency
c -----------------------
      do i = 1,irun
      do l = 2,nbase(i)
          VAI1 = (T(i,lm+1-l)-T(i,lm+2-l))/DZ(i,l)+GOCP
      if( VAI1.LT.0.0 ) then
          VAI1 =  0.0
      endif
      VAI2    = 2.0*GRAV/( T(i,lm+1-l)+T(i,lm+2-l) )
      VSQUA   = VAI1*VAI2
      BV(i,l) = SQRT(VSQUA)
      enddo
      enddo

c Stress at the Surface Level
c ---------------------------
      do i = 1,irun
      nbar(i) = 0.0
      enddo
      do i = 1,irun
      do l = 2,nbase(i)
      NBAR(i) = NBAR(i) + BV(i,l)*(pl(i,l-1)-pl(i,l))
      enddo
      enddo

      do i = 1,irun
      NBAR(i) = NBAR(i)/(pl(i,1)-pl(i,nbase(i)))
      FRSF    = NBAR(i)*tstd(i)/speed(i)

      if( speed(i).eq.0.0 .or. nbar(i).eq.0.0 ) then
      TENSIO(i,1) = 0.0
      else
      GSTAR = G*FRSF*FRSF/(FRSF*FRSF+A*A)
      TENSIO(i,1) = GSTAR*(ROBAR(i)*speed(i)*speed(i)*speed(i))
     .            / (NBAR(i)*LSTAR)
      endif

       XTENS(i,1) = TENSIO(i,1) * cos(ang(i))
       YTENS(i,1) = TENSIO(i,1) * sin(ang(i))
      DRAGSF(i)   = TENSIO(i,1)
       XDRAG(i)   =  XTENS(i,1)
       YDRAG(i)   =  YTENS(i,1)
      enddo

c Check for Very thin lowest layer
c --------------------------------
      do i = 1,irun
      if( NTHIN(i).gt.1 ) then
      do l = 1,nthin(i)
      TENSIO(i,l) = TENSIO(i,1)
       XTENS(i,l) =  XTENS(i,1)
       YTENS(i,l) =  YTENS(i,1)
      enddo
      endif
      enddo

c******************************************************
c  Compute Gravity Wave Stress from NTHIN+1 to NBASE  *
c******************************************************

      do i = 1,irun
      do l = nthin(i)+1,nbase(i)

      velco = 0.5*( (u(i,lm+1-l)*ubar(i) + v(i,lm+1-l)*vbar(i))
     .            + (u(i,lm+2-l)*ubar(i) + v(i,lm+2-l)*vbar(i))  )
     .      /   speed(i)

C Convert to Newton/m**2
      roave = 0.5*(ro(i,l-1)+ro(i,l)) * 100.0     

      if( VELCO.le.0.0 ) then
      TENSIO(i,l) = TENSIO(i,l-1)
      goto 1500
      endif
                    
c Froude number squared
c ---------------------
      FRO2   = bv(i,l)/(AKWNMB*ROAVE*VELCO*VELCO*VELCO)*TENSIO(i,l-1)
      DELUU  = u(i,lm+1-l)-u(i,lm+2-l)
      DELVV  = v(i,lm+1-l)-v(i,lm+2-l)
      DELVE2 = ( DELUU*DELUU + DELVV*DELVV )

c Compute Richarson Number
c ------------------------
      if( DELVE2.ne.0.0 ) then
        DELZ = DZ(i,l)
       VSQUA = BV(i,l)*BV(i,l)
      RICHSN = DELZ*DELZ*VSQUA/DELVE2
      else
      RICHSN = 99999.0
      endif

      if( RICHSN.le.0.25 ) then
      TENSIO(i,l) = TENSIO(i,l-1)
      goto 1500
      endif

c Stress in the Base Layer changes if the local Froude number
c exceeds the Critical Froude number
c ----------------------------------
                  CRIFRO = 1.0 - 0.25/RICHSN
                   CRIF2 = CRIFRO*CRIFRO
      if( l.eq.2 ) CRIF2 = MIN(0.7,CRIF2)

      if( FRO2.gt.CRIF2 ) then
      TENSIO(i,l) = CRIF2/FRO2*TENSIO(i,l-1)
      else
      TENSIO(i,l) = TENSIO(i,l-1)
      endif

1500  CONTINUE
      XTENS(i,l) = TENSIO(i,l)*COS(ang(i))
      YTENS(i,l) = TENSIO(i,l)*SIN(ang(i))

      enddo
      enddo

c******************************************************
c    Compute Gravity Wave Stress from Base+1 to Top   *
c******************************************************

      do i = 1,irun
      icrilv(i) = 0
      enddo

      do i = 1,irun
      do l = nbase(i)+1,lm+1

      TENSIO(i,l) = 0.0

c Check for Critical Level Absorption
c -----------------------------------
      if( icrilv(i).eq.1 ) goto 130

c Let Remaining Stress escape out the top edge of model
c -----------------------------------------------------
      if( l.eq.lm+1 ) then
      TENSIO(i,l) = TENSIO(i,l-1)
      goto 130
      endif

      ROAVE = 0.5*(ro(i,l-1)+ro(i,l)) * 100.0
      VAI1  = (T(i,lm+1-l)-T(i,lm+2-l))/DZ(i,l)+GOCP
 
      if( VAI1.lt.0.0 ) then
      icrilv(i)   = 1
      TENSIO(i,l) = 0.0
      goto 130
      endif

      VAI2  = 2.0*GRAV/(T(i,lm+1-l)+T(i,lm+2-l))
      VSQUA = VAI1*VAI2
      VAISD = SQRT(VSQUA)

      velco = 0.5*( (u(i,lm+1-l)*ubar(i) + v(i,lm+1-l)*vbar(i))
     .            + (u(i,lm+2-l)*ubar(i) + v(i,lm+2-l)*vbar(i))  )
     .      /   speed(i)

      if( velco.lt.0.0 ) then
      icrilv(i)   = 1
      TENSIO(i,l) = 0.0
      goto 130
      endif

c Froude number squared
c ---------------------
      FRO2   = vaisd/(AKWNMB*ROAVE*VELCO*VELCO*VELCO)*TENSIO(i,l-1)
      DELUU  = u(i,lm+1-l)-u(i,lm+2-l)
      DELVV  = v(i,lm+1-l)-v(i,lm+2-l)
      DELVE2 = ( DELUU*DELUU + DELVV*DELVV )

c Compute Richarson Number
c ------------------------
      if( DELVE2.ne.0.0 ) then
      DELZ   = DZ(i,l)
      RICHSN = DELZ*DELZ*VSQUA/DELVE2
      else
      RICHSN = 99999.0
      endif

      if( RICHSN.le.0.25 ) then
      TENSIO(i,l) = 0.0
      icrilv(i)   = 1
      goto 130
      endif

c Stress in Layer changes if the local Froude number
c exceeds the Critical Froude number
c ----------------------------------
      CRIFRO = 1.0 - 0.25/RICHSN
       CRIF2 = CRIFRO*CRIFRO

      if( FRO2.ge.CRIF2 ) then
      TENSIO(i,l) = CRIF2/FRO2*TENSIO(i,l-1)
      else
      TENSIO(i,l) = TENSIO(i,l-1)
      endif

  130 continue
      XTENS(i,l) = TENSIO(i,l)*COS(ang(i))
      YTENS(i,l) = TENSIO(i,l)*SIN(ang(i))
      enddo
      enddo
 
C ******************************************************
C       MOMENTUM CHANGE FOR FREE ATMOSPHERE            *
C ******************************************************
 
      do i = 1,irun
      do l = nthin(i)+1,lm
      coef = -grav*ple(i,lm+1)/dpres(i,lm+1-l)
      dudt(i,lm+1-l) = coef*(XTENS(i,l+1)-XTENS(i,l))
      dvdt(i,lm+1-l) = coef*(YTENS(i,l+1)-YTENS(i,l))
      enddo
      enddo
 
c Momentum change near the surface
c --------------------------------
      do i = 1,irun
      coef = grav*ple(i,lm+1)/(ple(i,lm+1-nthin(i))-ple(i,lm+1))
      dudt(i,lm) = coef*(XTENS(i,nthin(i)+1)-XTENS(i,1))
      dvdt(i,lm) = coef*(YTENS(i,nthin(i)+1)-YTENS(i,1))
      enddo

c If Lowest layer is very thin, it is strapped to next layer
c ----------------------------------------------------------
      do i = 1,irun
      if( nthin(i).gt.1 ) then
      do l = 2,nthin(i)
      dudt(i,lm+1-l) = dudt(i,lm)
      dvdt(i,lm+1-l) = dvdt(i,lm)
      enddo
      endif
      enddo

c Convert Units to (m/sec**2)
c --------------------------- 
      do l = 1,lm
      do i = 1,irun
      dudt(i,l) = - dudt(i,l)/ps(i)*0.01
      dvdt(i,l) = - dvdt(i,l)/ps(i)*0.01
      enddo
      enddo

      return
      end
