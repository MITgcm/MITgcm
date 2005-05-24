C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_gwdrag.F,v 1.3 2005/05/24 00:02:29 molod Exp $
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

c Input Variables
c ---------------
      integer myid,im,jm,lm,bi,bj,istrip,npcs,imglobal
      _RL pz(im,jm)
      _RL pl(im,jm,lm)
      _RL ple(im,jm,lm+1)
      _RL dpres(im,jm,lm)
      _RL pkz(im,jm,lm)
      _RL uz(im,jm,lm)
      _RL vz(im,jm,lm)
      _RL tz(im,jm,lm)
      _RL qz(im,jm,lm)
      _RL phis_var(im,jm)

      _RL dudt(im,jm,lm)
      _RL dvdt(im,jm,lm)
      _RL dtdt(im,jm,lm)

c Local Variables
c ---------------
      _RL tv(im,jm,lm)
      _RL dragu(im,jm,lm), dragv(im,jm,lm)
      _RL dragt(im,jm,lm) 
      _RL dragx(im,jm), dragy(im,jm)
      _RL sumu(im,jm)
      integer nthin(im,jm),nbase(im,jm)
      integer nthini, nbasei

      _RL phis_std(im,jm)

      _RL std(istrip), ps(istrip)
      _RL us(istrip,lm), vs(istrip,lm), ts(istrip,lm)
      _RL dragus(istrip,lm), dragvs(istrip,lm) 
      _RL dragxs(istrip), dragys(istrip)
      _RL plstr(istrip,lm),plestr(istrip,lm),dpresstr(istrip,lm)
      integer nthinstr(istrip),nbasestr(istrip)

      integer n,i,j,L
      _RL getcon, pi
      _RL grav, rgas, cp, cpinv, lstar
#ifdef ALLOW_DIAGNOSTICS
      logical  diagnostics_is_on
      external diagnostics_is_on
      _RL tmpdiag(im,jm)
#endif

      return

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
#ifdef ALLOW_DIAGNOSTICS
      do L = 1,lm

      if(diagnostics_is_on('GWDU    ',myid) ) then
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = dragu(i,j,L)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'GWDU    ',L,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('GWDV    ',myid) ) then
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = dragv(i,j,L)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'GWDV    ',L,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('GWDT    ',myid) ) then
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = dragt(i,j,L)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'GWDT    ',L,1,3,bi,bj,myid)
      endif

      enddo

c Gravity Wave Drag at Surface (U-Wind)
c -------------------------------------
      if(diagnostics_is_on('GWDUS   ',myid) ) then
       call diagnostics_fill(dragx,'GWDUS   ',0,1,3,bi,bj,myid)
      endif

c Gravity Wave Drag at Surface (V-Wind)
c -------------------------------------
      if(diagnostics_is_on('GWDVS   ',myid) ) then
       call diagnostics_fill(dragy,'GWDVS   ',0,1,3,bi,bj,myid)
      endif

c Gravity Wave Drag at Model Top (U-Wind)
c ---------------------------------------
      if(diagnostics_is_on('GWDUT   ',myid) ) then
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
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = dragx(i,j) + sumu(i,j)*pz(i,j)/grav*100
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'GWDUT   ',0,1,3,bi,bj,myid)
      endif

c Gravity Wave Drag at Model Top (V-Wind)
c ---------------------------------------
      if(diagnostics_is_on('GWDVT   ',myid) ) then
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
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = dragy(i,j) + sumu(i,j)*pz(i,j)/grav*100
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'GWDVT   ',0,1,3,bi,bj,myid)
      endif
#endif

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
      _RL ps(irun)
      _RL u(irun,lm), v(irun,lm), t(irun,lm)
      _RL dudt(irun,lm), dvdt(irun,lm)
      _RL xdrag(irun), ydrag(irun)
      _RL std(irun)
      _RL ple(irun,lm+1), pl(irun,lm), dpres(irun,lm)
      _RL grav, rgas, cp
      integer nthin(irun),nbase(irun)
      _RL lstar

c Dynamic Allocation Variables
c ----------------------------
      _RL ubar(irun), vbar(irun), robar(irun)
      _RL speed(irun), ang(irun)
      _RL bv(irun,lm)
      _RL nbar(irun)

      _RL tstd(irun)
      _RL XTENS(irun,lm+1), YTENS(irun,lm+1)
      _RL TENSIO(irun,lm+1)
      _RL DRAGSF(irun)
      _RL RO(irun,lm), DZ(irun,lm)

      integer icrilv(irun)

c Local Variables
c ---------------
      integer  i,l
      _RL a,g,stdmax,agrav,akwnmb
      _RL gocp,roave,roiave,frsf,gstar,vai1,vai2
      _RL vaisd,velco,deluu,delvv,delve2,delz,vsqua
      _RL richsn,crifro,crif2,fro2,coef

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
