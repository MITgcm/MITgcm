C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_gwdrag.F,v 1.11 2005/07/01 01:12:00 jmc Exp $
C $Name:  $
#include "FIZHI_OPTIONS.h"
      subroutine gwdrag (myid,pz,pl,ple,dpres,pkz,uz,vz,tz,qz,phis_var,
     .         dudt,dvdt,dtdt,im,jm,Lm,bi,bj,istrip,npcs,imglobal)
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
C  pl    ....... 3D pressure field [im,jm,Lm]
C  ple   ....... 3d pressure at model level edges [im,jm,Lm+1]
C  dpres ....... pressure difference across level [im,jm,Lm]
C  pkz   ....... pressure**kappa [im,jm,Lm]
C  uz    ....... zonal velocity [im,jm,Lm]
C  vz    ....... meridional velocity [im,jm,Lm]
C  tz    ....... temperature [im,jm,Lm]
C  qz    ....... specific humidity [im,jm,Lm]
C  phis_var .... topography variance
C  im    ....... number of grid points in x direction
C  jm    ....... number of grid points in y direction
C  Lm    ....... number of grid points in vertical
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
      integer myid,im,jm,Lm,bi,bj,istrip,npcs,imglobal
      _RL pz(im,jm)
      _RL pl(im,jm,Lm)
      _RL ple(im,jm,Lm+1)
      _RL dpres(im,jm,Lm)
      _RL pkz(im,jm,Lm)
      _RL uz(im,jm,Lm)
      _RL vz(im,jm,Lm)
      _RL tz(im,jm,Lm)
      _RL qz(im,jm,Lm)
      _RL phis_var(im,jm)

      _RL dudt(im,jm,Lm)
      _RL dvdt(im,jm,Lm)
      _RL dtdt(im,jm,Lm)

c Local Variables
c ---------------
      _RL tv(im,jm,Lm)
      _RL dragu(im,jm,Lm), dragv(im,jm,Lm)
      _RL dragt(im,jm,Lm) 
      _RL dragx(im,jm), dragy(im,jm)
      _RL sumu(im,jm)
      integer nthin(im,jm),nbase(im,jm)
      integer nthini, nbasei

      _RL phis_std(im,jm)

      _RL std(istrip), ps(istrip)
      _RL us(istrip,Lm), vs(istrip,Lm), ts(istrip,Lm)
      _RL dragus(istrip,Lm), dragvs(istrip,Lm) 
      _RL dragxs(istrip), dragys(istrip)
      _RL plstr(istrip,Lm),plestr(istrip,Lm+1),dpresstr(istrip,Lm)
      integer nthinstr(istrip),nbasestr(istrip)

      integer n,i,j,L
      _RL getcon, pi
      _RL grav, rgas, cp, cpinv, lstar
#ifdef ALLOW_DIAGNOSTICS
      logical  diagnostics_is_on
      external diagnostics_is_on
      _RL tmpdiag(im,jm)
#endif

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

      do nthini = 1,Lm+1
       if( pz(i,j)-ple(i,j,Lm+2-nthini).gt.25. ) then
        nthin(i,j) = nthini
        goto 10
       endif
      enddo
   10 continue
      do nbasei = 1,Lm+1
       if( ple(i,j,Lm+2-nbasei).lt.(0.667*pz(i,j)) ) then
        nbase(i,j) = nbasei
        goto 20
       endif
      enddo
   20 continue
      if( (0.667*pz(i,j))-ple(i,j,Lm+2-nbase(i,j)) .gt. 
     .           ple(i,j,Lm+3-nbase(i,j))-(0.667*pz(i,j)) ) then
      nbase(i,j) = nbase(i,j)-1
      endif

      enddo
      enddo

c Compute Topography Sub-Grid Standard Deviation
c        and constrain the Maximum Value
c ----------------------------------------------
      do j=1,jm
      do i=1,im
         phis_std(i,j) = min( 400.0 _d 0, sqrt( max(0.0 _d 0,
     $        phis_var(i,j)) )/grav )
      enddo
      enddo

c Compute Virtual Temperatures
c ----------------------------
      do L = 1,Lm
      do j = 1,jm
      do i = 1,im
      tv(i,j,L) = tz(i,j,L)*pkz(i,j,L)*(1.+.609*qz(i,j,L))
      enddo
      enddo
      enddo

      do L = 1,Lm
      do j = 1,jm
      do i = 1,im
       dragu(i,j,L) = 0.
       dragv(i,j,L) = 0.
       dragt(i,j,L) = 0.
      enddo
      enddo
      enddo

c Call Gravity Wave Drag Paramterization on A-Grid
c ------------------------------------------------

      do n=1,npcs

      call stripit ( phis_std,std,im*jm,im*jm,istrip,1,n )

      call stripit ( pz,ps,im*jm,im*jm,istrip,1 ,n )
      call stripit ( uz,us,im*jm,im*jm,istrip,Lm,n )
      call stripit ( vz,vs,im*jm,im*jm,istrip,Lm,n )
      call stripit ( tv,ts,im*jm,im*jm,istrip,Lm,n )
      call stripit ( pl,plstr,im*jm,im*jm,istrip,Lm,n )
      call stripit ( ple,plestr,im*jm,im*jm,istrip,Lm+1,n )
      call stripit ( dpres,dpresstr,im*jm,im*jm,istrip,Lm,n )
      call stripitint ( nthin,nthinstr,im*jm,im*jm,istrip,1,n )
      call stripitint ( nbase,nbasestr,im*jm,im*jm,istrip,1,n )

      call GWDD ( ps,us,vs,ts,
     .            dragus,dragvs,dragxs,dragys,std,
     .            plstr,plestr,dpresstr,grav,rgas,cp,
     .            istrip,Lm,nthinstr,nbasestr,lstar )

      call pastit( dragus,dragu,istrip,im*jm,im*jm,Lm,n )
      call pastit( dragvs,dragv,istrip,im*jm,im*jm,Lm,n )
      call pastit( dragxs,dragx,istrip,im*jm,im*jm,1 ,n )
      call pastit( dragys,dragy,istrip,im*jm,im*jm,1 ,n )

      enddo

c Add Gravity-Wave Drag to Wind and Theta Tendencies
c -------------------------------------------------- 
      do L = 1,Lm
      do j = 1,jm
      do i = 1,im
         dragu(i,j,L) = sign( min(0.006 _d 0,abs(dragu(i,j,L))), dragu(i
     $        ,j,L) ) 
         dragv(i,j,L) = sign( min(0.006 _d 0,abs(dragv(i,j,L))), dragv(i
     $        ,j,L) ) 
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
      do L = 1,Lm

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
      do L = 1,Lm
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
      do L = 1,Lm
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
     .                  grav,rgas,cp,irun,Lm,nthin,nbase,lstar )
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
C    Lm ......... Number of grid-points in vertical   dimension
C    lstar ...... Monochromatic Wavelength/(2*pi)
C
C Output:
C  =======
C    dudt ....... Zonal Acceleration due to GW Drag (m/sec**2)
C    dvdt ....... Meridional Acceleration due to GW Drag (m/sec**2)
C    xdrag ...... Zonal Surface and Base Layer Stress (Pa)
C    ydrag ...... Meridional Surface and Base Layer Stress (Pa)
C
C NOTE: Quantities computed locally in GWDD use a
C              bottom-up counting of levels
C       The fizhi code uses a top-down so all
C       Quantities that came in through the arg list
C       must use reverse vertical indexing!!!
C***********************************************************************

      implicit none

c Input Variables
c ---------------
      integer irun,Lm
      _RL ps(irun)
      _RL u(irun,Lm), v(irun,Lm), t(irun,Lm)
      _RL dudt(irun,Lm), dvdt(irun,Lm)
      _RL xdrag(irun), ydrag(irun)
      _RL std(irun)
      _RL ple(irun,Lm+1), pl(irun,Lm), dpres(irun,Lm)
      _RL grav, rgas, cp
      integer nthin(irun),nbase(irun)
      _RL lstar

c Dynamic Allocation Variables
c ----------------------------
      _RL ubar(irun), vbar(irun), robar(irun)
      _RL speed(irun), ang(irun)
      _RL bv(irun,Lm)
      _RL nbar(irun)

      _RL XTENS(irun,Lm+1), YTENS(irun,Lm+1)
      _RL TENSIO(irun,Lm+1)
      _RL DRAGSF(irun)
      _RL RO(irun,Lm), DZ(irun,Lm)

      integer icrilv(irun)

c Local Variables
c ---------------
      integer  i,L
      _RL a,g,agrav,akwnmb
      _RL gocp,roave,roiave,frsf,gstar,vai1,vai2
      _RL vaisd,velco,deluu,delvv,delve2,delz,vsqua
      _RL richsn,crifro,crif2,fro2,coef

c Initialization
c --------------
      a      = 1.0
      g      = 1.0
      agrav  = 1.0/grav
      akwnmb = 1.0/lstar
      gocp   = grav/cp

c Compute Atmospheric Density (with virtual temp)
c -----------------------------------------------
      do l = 1,Lm
      do i = 1,irun
       ro(i,L) = pl(i,Lm+1-L)/(rgas*t(i,Lm+1-L))
      enddo
      enddo

c Compute Layer Thicknesses
c -------------------------
      do l = 2,Lm
      do i = 1,irun
       roiave  = ( 1./ro(i,L-1) + 1./ro(i,L) )*0.5
       dz(i,L) = agrav*roiave*( pl(i,Lm+2-L)-pl(i,Lm+1-L) )
      enddo
      enddo


c***********************************************************************
c          Surface and Base Layer Stress                               *
c***********************************************************************

c Definition of Surface Wind Vector
c ---------------------------------
      do  i = 1,irun
       robar(i) = 0.0
       ubar(i) = 0.0
       vbar(i) = 0.0
      enddo

      do  i = 1,irun
      do  L = 1,nbase(i)-1
       robar(i) = robar(i) + ro(i,L) * (ple(i,Lm+2-L)-ple(i,Lm+1-L))
       ubar(i) = ubar(i) + u(i,Lm+1-L) * (ple(i,Lm+2-L)-ple(i,Lm+1-L))
       vbar(i) = vbar(i) + v(i,Lm+1-L) * (ple(i,Lm+2-L)-ple(i,Lm+1-L))
      enddo
      enddo

      do  i = 1,irun
       robar(i) = robar(i)/(ps(i)-ple(i,Lm+1-(nbase(i)-1))) * 100.0
       ubar(i) = ubar(i)/(ps(i)-ple(i,Lm+1-(nbase(i)-1)))
       vbar(i) = vbar(i)/(ps(i)-ple(i,Lm+1-(nbase(i)-1)))

       speed(i) = sqrt( ubar(i)*ubar(i) + vbar(i)*vbar(i) )
       ang(i) = atan2(vbar(i),ubar(i))
      enddo

c Brunt Vaisala Frequency
c -----------------------
      do i = 1,irun
       do l = 2,nbase(i)
        vai1 = (t(i,Lm+1-L)-t(i,Lm+2-L))/dz(i,L)+gocp
        if( vai1.LT.0.0 ) then
         vai1 =  0.0
        endif
        vai2    = 2.0*grav/( t(i,Lm+1-L)+t(i,Lm+2-L) )
        vsqua   = vai1*vai2
        bv(i,L) = sqrt(vsqua)
       enddo
      enddo

c Stress at the Surface Level
c ---------------------------
      do i = 1,irun
       nbar(i) = 0.0
      enddo
      do i = 1,irun
      do l = 2,nbase(i)
       nbar(i) = nbar(i) + bv(i,L)*(pl(i,Lm+2-L)-pl(i,Lm+1-L))
      enddo
      enddo

      do i = 1,irun
       nbar(i) = nbar(i)/(pl(i,Lm)-pl(i,Lm+1-nbase(i)))
       frsf = nbar(i)*std(i)/speed(i)

       if( speed(i).eq.0.0 .or. nbar(i).eq.0.0 ) then
        tensio(i,1) = 0.0
       else
        gstar = g*frsf*frsf/(frsf*frsf+a*a)
        tensio(i,1) = gstar*(robar(i)*speed(i)*speed(i)*speed(i))
     .            / (nbar(i)*lstar)
       endif

       xtens(i,1) = tensio(i,1) * cos(ang(i))
       ytens(i,1) = tensio(i,1) * sin(ang(i))
       dragsf(i) = tensio(i,1)
       xdrag(i) = xtens(i,1)
       ydrag(i) = ytens(i,1)
      enddo

c Check for Very thin lowest layer
c --------------------------------
      do i = 1,irun
       if( nthin(i).gt.1 ) then
        do l = 1,nthin(i)
         tensio(i,L) = tensio(i,1)
         xtens(i,L) = xtens(i,1)
         ytens(i,L) = ytens(i,1)
        enddo
       endif
      enddo

c******************************************************
c  Compute Gravity Wave Stress from NTHIN+1 to NBASE  *
c******************************************************

      do i = 1,irun
       do l = nthin(i)+1,nbase(i)

        velco = 0.5*( (u(i,Lm+1-L)*ubar(i) + v(i,Lm+1-L)*vbar(i))
     .            + (u(i,Lm+2-L)*ubar(i) + v(i,Lm+2-L)*vbar(i))  )
     .      /   speed(i)

C Convert to Newton/m**2
        roave = 0.5*(ro(i,L-1)+ro(i,L)) * 100.0     

        if( velco.le.0.0 ) then
         tensio(i,L) = tensio(i,L-1)
         goto 1500
        endif
                    
c Froude number squared
c ---------------------
        fro2 = bv(i,L)/(akwnmb*roave*velco*velco*velco)*tensio(i,L-1)
        deluu = u(i,Lm+1-L)-u(i,Lm+2-L)
        delvv = v(i,Lm+1-L)-v(i,Lm+2-L)
        delve2 = ( deluu*deluu + delvv*delvv )

c Compute Richarson Number
c ------------------------
        if( delve2.ne.0.0 ) then
         delz = dz(i,L)
         vsqua = bv(i,L)*bv(i,L)
         richsn = delz*delz*vsqua/delve2
        else
         richsn = 99999.0
        endif

        if( richsn.le.0.25 ) then
         tensio(i,L) = tensio(i,L-1)
         goto 1500
        endif

c Stress in the Base Layer changes if the local Froude number
c exceeds the Critical Froude number
c ----------------------------------
        crifro = 1.0 - 0.25/richsn
        crif2 = crifro*crifro
        if( l.eq.2 ) crif2 = min(0.7 _d 0,crif2)

        if( fro2.gt.crif2 ) then
         tensio(i,L) = crif2/fro2*tensio(i,L-1)
        else
         tensio(i,L) = tensio(i,L-1)
        endif

1500    continue
        xtens(i,L) = tensio(i,L)*cos(ang(i))
        ytens(i,L) = tensio(i,L)*sin(ang(i))

       enddo
      enddo

c******************************************************
c    Compute Gravity Wave Stress from Base+1 to Top   *
c******************************************************

      do i = 1,irun
       icrilv(i) = 0
      enddo

      do i = 1,irun
       do l = nbase(i)+1,Lm+1

        tensio(i,L) = 0.0

c Check for Critical Level Absorption
c -----------------------------------
        if( icrilv(i).eq.1 ) goto 130

c Let Remaining Stress escape out the top edge of model
c -----------------------------------------------------
        if( l.eq.Lm+1 ) then
         tensio(i,L) = tensio(i,L-1)
         goto 130
        endif

        roave = 0.5*(ro(i,L-1)+ro(i,L)) * 100.0
        vai1  = (t(i,Lm+1-L)-t(i,Lm+2-L))/dz(i,L)+gocp
 
        if( vai1.lt.0.0 ) then
         icrilv(i)   = 1
         tensio(i,L) = 0.0
         goto 130
        endif

        vai2  = 2.0*grav/(t(i,Lm+1-L)+t(i,Lm+2-L))
        vsqua = vai1*vai2
        vaisd = sqrt(vsqua)

        velco = 0.5*( (u(i,Lm+1-L)*ubar(i) + v(i,Lm+1-L)*vbar(i))
     .            + (u(i,Lm+2-L)*ubar(i) + v(i,Lm+2-L)*vbar(i))  )
     .      /   speed(i)

        if( velco.lt.0.0 ) then
         icrilv(i)   = 1
         tensio(i,L) = 0.0
         goto 130
        endif

c Froude number squared
c ---------------------
        fro2 = vaisd/(akwnmb*roave*velco*velco*velco)*tensio(i,L-1)
        deluu = u(i,Lm+1-L)-u(i,Lm+2-L)
        delvv = v(i,Lm+1-L)-v(i,Lm+2-L)
        delve2 = ( deluu*deluu + delvv*delvv )

c Compute Richarson Number
c ------------------------
        if( delve2.ne.0.0 ) then
         delz   = dz(i,L)
         richsn = delz*delz*vsqua/delve2
        else
         richsn = 99999.0
        endif

        if( richsn.le.0.25 ) then
         tensio(i,L) = 0.0
         icrilv(i)   = 1
         goto 130
        endif

c Stress in Layer changes if the local Froude number
c exceeds the Critical Froude number
c ----------------------------------
        crifro = 1.0 - 0.25/richsn
        crif2 = crifro*crifro

        if( fro2.ge.crif2 ) then
         tensio(i,L) = crif2/fro2*tensio(i,L-1)
        else
         tensio(i,L) = tensio(i,L-1)
        endif

  130   continue
        xtens(i,L) = tensio(i,L)*cos(ang(i))
        ytens(i,L) = tensio(i,L)*sin(ang(i))
       enddo
      enddo
 
C ******************************************************
C       MOMENTUM CHANGE FOR FREE ATMOSPHERE            *
C ******************************************************
 
      do i = 1,irun
      do l = nthin(i)+1,Lm
       coef = -grav*ps(i)/dpres(i,Lm+1-L)
       dudt(i,Lm+1-L) = coef*(xtens(i,L+1)-xtens(i,L))
       dvdt(i,Lm+1-L) = coef*(ytens(i,L+1)-ytens(i,L))
      enddo
      enddo
 
c Momentum change near the surface
c --------------------------------
      do i = 1,irun
       coef = grav*ps(i)/(ple(i,Lm+1-nthin(i))-ple(i,Lm+1))
       dudt(i,Lm) = coef*(xtens(i,nthin(i)+1)-xtens(i,1))
       dvdt(i,Lm) = coef*(ytens(i,nthin(i)+1)-ytens(i,1))
      enddo

c If Lowest layer is very thin, it is strapped to next layer
c ----------------------------------------------------------
      do i = 1,irun
       if( nthin(i).gt.1 ) then
        do l = 2,nthin(i)
         dudt(i,Lm+1-L) = dudt(i,Lm)
         dvdt(i,Lm+1-L) = dvdt(i,Lm)
        enddo
       endif
      enddo

c Convert Units to (m/sec**2)
c --------------------------- 
      do l = 1,Lm
      do i = 1,irun
       dudt(i,L) = - dudt(i,L)/ps(i)*0.01
       dvdt(i,L) = - dvdt(i,L)/ps(i)*0.01
      enddo
      enddo

      return
      end
