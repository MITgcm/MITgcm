C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_step_diag.F,v 1.1 2004/06/24 15:06:51 molod Exp $
C $Name:  $
                                                                                       
#include "CPP_OPTIONS.h"
      subroutine fizhi_step_diag(myThid,p,uphy,vphy,thphy,sphy,qq,
     .  radswt,radswg,swgclr,osr,osrclr,st4,dst4,tgz,tg0,radlwg,lwgclr,
     .  turbu,turbv,turbt,turbq,moistu,moistv,moistt,moistq,
     .  lwdt,swdt,lwdtclr,swdtclr,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,bi,bj)
C***********************************************************************
      implicit none

#include "diagnostics.h"

      integer myThid,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,bi,bj
      real radswt(im2,jm2)

      integer  i,j,L,m
      real getcon
      real cp, pstd, tstd, akap, pkstd, thstd, grav, delp

      cp = getcon('CP')
      pstd = getcon('PSTD')
      tstd = getcon('TSTD')
      akap = getcon('KAPPA')
      pkstd = pstd**akap
      thstd = tstd/pkstd
      grav = getcon('GRAVITY')

C **********************************************************************        
C ****                  Compute 2-D Diagnostics                     ****        
C **********************************************************************        
                                                                                
      do j=jm1,jm2
      do i=im1,im2
      pinv(i,j) = 1.0 / p(i,j)                                    
      enddo                                                             
      enddo                                                             

c Analysis Increment of Surface Pressure (mb/day)
c -----------------------------------------------
      if( ipiau.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ipiau) = qdiag(i,j,ipiau) + tend%iau%dp(i,j)*86400
      enddo
      enddo
      endif
                                                                  
c Incident Solar Radiation (W/m**2)
c ---------------------------------
      if (iradswt.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradswt) = qdiag(i,j,iradswt) + radswt(i,j)
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Net Solar Radiation at the Ground (W/m**2)
c ------------------------------------------
      if (iradswg.ne.0) then                                                    
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradswg) = qdiag(i,j,iradswg) + coup%sw%radswg(i,j)*radswt(i,j)         
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Net Clear Sky Solar Radiation at the Ground (W/m**2)
c ----------------------------------------------------
      if (iswgclr.ne.0) then                                                    
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iswgclr) = qdiag(i,j,iswgclr) + coup%sw%swgclr(i,j)*radswt(i,j)         
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Outgoing Solar Radiation at Ptop (W/m**2)
c -----------------------------------------
      if (iosr.ne.0) then                                                    
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iosr) = qdiag(i,j,iosr) + (1.0-coup%sw%osr(i,j))*radswt(i,j)         
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Outgoing Clear Sky Solar Radiation at Ptop (W/m**2)
c ---------------------------------------------------
      if (iosrclr.ne.0) then                                                    
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iosrclr) = qdiag(i,j,iosrclr) + (1.0-coup%sw%osrclr(i,j))*radswt(i,j)         
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Upward Longwave Flux at the Ground (W/m**2)
c -------------------------------------------
      if (ilwgup.ne.0) then                                                    
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ilwgup) =  qdiag(i,j,ilwgup) + coup%lw%st4(i,j) 
     .                   +   coup%lw%dst4(i,j)*(coup%land%tgz(i,j)-coup%lw%tg0(i,j))
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Net Longwave Flux at the Ground (W/m**2)
c ----------------------------------------
      if (iradlwg.ne.0) then                                                    
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradlwg) =  qdiag(i,j,iradlwg) + coup%lw%radlwg(i,j) 
     .                   +   coup%lw%dst4(i,j)*(coup%land%tgz(i,j)-coup%lw%tg0(i,j))
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Net Longwave Flux at the Ground Clear Sky (W/m**2)
c --------------------------------------------------
      if (ilwgclr.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ilwgclr) =  qdiag(i,j,ilwgclr) + coup%lw%lwgclr(i,j) 
     .                   +   coup%lw%dst4(i,j)*(coup%land%tgz(i,j)-coup%lw%tg0(i,j))
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Total Surface Pressure Tendency (mb/day)
c ----------------------------------------
      if( idpdt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idpdt) = qdiag(i,j,idpdt) + (updt%p(i,j)-curr%p(i,j))*fact
      enddo
      enddo
      endif
                                                                  
c Averaged P-Field (mb)
c ---------------------
      if( ips.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ips) = qdiag(i,j,ips) + curr%p(i,j)+ptop
      enddo
      enddo
      endif

c Averaged SLP-Field (mb)
c ----------------------
      if( islp.ne.0 ) then
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      tmp2(i,j,L) = curr%t(i,j,L) * (1.+0.609*curr%q(i,j,L,1))
      enddo
      enddo
      enddo
      call slprs ( tmp1(1,1,2),curr%p,ptop,coup%earth%phis_cmp,tmp2,dsig,coup%earth%lw_cmp,im,jm,lm )
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,islp) = qdiag(i,j,islp) + tmp1(i,j,2)
      enddo
      enddo
      endif

      npiau   = npiau   + 1
      nradswt = nradswt + 1                                                     
      nradswg = nradswg + 1                                                     
      nswgclr = nswgclr + 1                                                     
      nosr    = nosr    + 1
      nosrclr = nosrclr + 1
      nradlwg = nradlwg + 1
      nlwgclr = nlwgclr + 1
      nlwgup  = nlwgup  + 1
      ndpdt   = ndpdt   + 1                                              
      nps     = nps     + 1
      nslp    = nslp    + 1
                                                                                
C **********************************************************************        
C ****                  Compute 3-D Diagnostics                     ****        
C **********************************************************************        
                                                                                
      do L=1,Nrphys

      if( itiau.ne.0 .or. ivavetiau.ne.0 .or. idiabt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      dtiau(i,j,L) = ( tend%iau%dt(i,j,L) + curr%t(i,j,L)*tend%iau%dp(i,j)
     .               * ( sig(L)*akap*curr%p(i,j)/(sig(L)*curr%p(i,j)+ptop) - 1.0) )
      enddo
      enddo
      endif

      if( iqiau.ne.0 .or. ivaveqiau.ne.0 .or. idiabq.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      dqiau(i,j,L) = tend%iau%dq(i,j,L) - curr%q(i,j,L,1)*tend%iau%dp(i,j)
      enddo
      enddo
      endif

c Total Diabatic U-Tendency (m/sec/day)
c -------------------------------------
      if( idiabu.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idiabu+L-1) =   qdiag  (i,j,idiabu+L-1)
     .                      + ( tend%iau%du (i,j,L)
     .                        + tend%turb%du(i,j,L) )*86400
      enddo
      enddo
      endif
                                                                  
c Total Diabatic V-Tendency (m/sec/day)
c -------------------------------------
      if( idiabv.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idiabv+L-1) =   qdiag  (i,j,idiabv+L-1)
     .                      + ( tend%iau%dv (i,j,L)
     .                        + tend%turb%dv(i,j,L) )*86400
      enddo
      enddo
      endif

c Total Diabatic T-Tendency (deg/day)
c -----------------------------------
      if( idiabt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idiabt+L-1) =   qdiag(i,j,idiabt+L-1) 
     .                      + ( dtiau(i,j,L) + tend%turb%dt(i,j,L) + tend%lw%dt(i,j,L)
     .                        + coup%lw%dlwdtg(i,j,L)*(coup%land%tgc(i,j)-coup%lw%tg0c(i,j))
     .                        + tend%sw%dt(i,j,L)*radswc(i,j) 
     .                        + tend%moist%dt(i,j,L) )
     .                        * pkn(i,j,L)*pinv(i,j)*86400
      enddo
      enddo
      endif
                                                                  
c Total Diabatic Q-Tendency (g/kg/day)
c ------------------------------------
      if( idiabq.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idiabq+L-1) =   qdiag(i,j,idiabq+L-1)
     .                      + ( dqiau(i,j,L) + tend%turb%dq(i,j,L,1) + tend%moist%dq(i,j,L,1) )
     .                      *    pinv(i,j)*86400*1000
      enddo
      enddo
      endif
    
c Analysis U-Wind Increment (m/sec/day)
c -------------------------------------
      if( iuiau.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iuiau+L-1) = qdiag (i,j,iuiau+L-1) + tend%iau%du(i,j,L)*86400
      enddo
      enddo
      endif

c Analysis V-Wind Increment (m/sec/day)
c -------------------------------------
      if( iviau.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iviau+L-1) = qdiag (i,j,iviau+L-1) + tend%iau%dv(i,j,L)*86400
      enddo
      enddo
      endif

c Analysis Temperature Tendency (deg/day)
c ---------------------------------------
      if( itiau.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,itiau+L-1) = qdiag(i,j,itiau+L-1)
     .                     + dtiau(i,j,L) * pkn(i,j,L)*pinv(i,j)*86400
      enddo
      enddo
      endif
                                                                  
c Analysis Moisture Tendency (g/kg/day)
c -------------------------------------
      if( iqiau.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iqiau+L-1) = qdiag(i,j,iqiau+L-1)
     .                     + dqiau(i,j,L) * pinv(i,j)*86400*1000
      enddo
      enddo
      endif
    
c Longwave Heating (deg/day)
c --------------------------
      if (iradlw.ne.0) then                                                     
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradlw+l-1) = qdiag(i,j,iradlw+l-1)
     .                      + ( tend%lw%dt(i,j,l)
     .                      +   coup%lw%dlwdtg (i,j,L)*(coup%land%tgc(i,j)-coup%lw%tg0c(i,j)))
     .                      * pkn(i,j,l)*pinv(i,j)*86400
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Longwave Heating Clear-Sky (deg/day)
c ------------------------------------
      if (ilwclr.ne.0) then                                                     
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ilwclr+l-1) = qdiag(i,j,ilwclr+l-1)
     .                      + ( tend%lw%dtclr(i,j,l)
     .                      +   coup%lw%dlwdtg(i,j,L)*(coup%land%tgc(i,j)-coup%lw%tg0c(i,j)))
     .                      * pkn(i,j,l)*pinv(i,j)*86400
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Solar Radiative Heating (deg/day)
c ---------------------------------
      if (iradsw.ne.0) then                                                     
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradsw+l-1) = qdiag(i,j,iradsw+l-1)
     .                      + tend%sw%dt(i,j,l)*radswc(i,j)                        
     .                      * pkn(i,j,l)*pinv(i,j)*86400
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Clear Sky Solar Radiative Heating (deg/day)
c -------------------------------------------
      if (iswclr.ne.0) then                                                     
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iswclr+l-1) = qdiag(i,j,iswclr+l-1)
     .                      + tend%sw%dtclr(i,j,l)*radswc(i,j)                        
     .                      * pkn(i,j,l)*pinv(i,j)*86400
      enddo                                                                     
      enddo                                                                     
      endif                                                                     
                                                                                
c Total U-Tendency (m/sec/day)
c ----------------------------
      if( idudt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idudt+L-1) =  qdiag(i,j,idudt+L-1) 
     .                     + ( updt%u(i,j,L)-curr%u(i,j,L) )*fact
      enddo
      enddo
      endif
                                                                  
c Total V-Tendency (m/sec/day)
c ----------------------------
      if( idvdt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idvdt+L-1) =  qdiag(i,j,idvdt+L-1) 
     .                     + ( updt%v(i,j,L)-curr%v(i,j,L) )*fact
      enddo
      enddo
      endif

c Total T-Tendency (deg/day)
c --------------------------
      if( idtdt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idtdt+L-1) =  qdiag(i,j,idtdt+L-1)
     .                     + ( updt%t(i,j,L)*pknp1(i,j,L) - curr%t(i,j,L)*pkn(i,j,L) )*fact
      enddo
      enddo
      endif

c Total Q-Tendency (g/kg/day)
c ---------------------------
      if( idqdt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idqdt+L-1) =  qdiag(i,j,idqdt+L-1)
     .                     + ( updt%q(i,j,L,1)-curr%q(i,j,L,1) )*fact*1000
      enddo
      enddo
      endif

c Averaged U-Field (m/sec)
c ------------------------
      if( iuwnd.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iuwnd+L-1) = qdiag(i,j,iuwnd+L-1) + curr%u(i,j,L)
      enddo
      enddo
      endif

c Averaged V-Field (m/sec)
c ------------------------
      if( ivwnd.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivwnd+L-1) = qdiag(i,j,ivwnd+L-1) + curr%v(i,j,L)
      enddo
      enddo
      endif

c Averaged T-Field (deg)
c ----------------------
      if( itmpu.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,itmpu+L-1) = qdiag(i,j,itmpu+L-1) + curr%t(i,j,L)*pkn(i,j,L)
      enddo
      enddo
      endif
                                                                                
c Averaged QQ-Field (m/sec)**2
c ----------------------------
      if( itke.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,itke+L-1) = qdiag(i,j,itke+L-1) + coup%turb%qq(i,j,L)
      enddo
      enddo
      endif
                                                                                
c Averaged Q-Field (g/kg)
c -----------------------
      if( isphu.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,isphu+L-1) = qdiag(i,j,isphu+L-1) + curr%q(i,j,L,1)*1000
      enddo
      enddo
      endif
                                                                                
      enddo   ! End Level Loop

      ndiabu = ndiabu + 1
      ndiabv = ndiabv + 1
      ndiabt = ndiabt + 1
      nuiau  = nuiau  + 1
      nviau  = nviau  + 1
      ntiau  = ntiau  + 1
      nradlw = nradlw + 1                                                     
      nlwclr = nlwclr + 1                                                     
      nradsw = nradsw + 1                                                     
      nswclr = nswclr + 1                                                     
      ndudt  = ndudt  + 1                                              
      ndvdt  = ndvdt  + 1                                              
      ndtdt  = ndtdt  + 1                                              
      ndiabq = ndiabq + 1
      nqiau  = nqiau  + 1
      ndqdt  = ndqdt  + 1                                              
      nuwnd  = nuwnd  + 1
      nvwnd  = nvwnd  + 1                           
      ntmpu  = ntmpu  + 1                           
      ntke   = ntke   + 1
      nsphu  = nsphu  + 1

C **********************************************************************        
C ****         Compute Vertically Integrated Diagnostics            ****        
C **********************************************************************        

c Compute Moisture and Temperature Convergence Diagnostic
c -------------------------------------------------------
      if( ivaveut.ne.0 .or. ivavevt.ne.0  .or.
     .    ivaveuq.ne.0 .or. ivavevq.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      vintut(i,j) = 0.0
      vintvt(i,j) = 0.0
      vintuq(i,j) = 0.0
      vintvq(i,j) = 0.0
      enddo
      enddo
 
      call ctoa   ( curr%t,curr%t,dlam,dphi,im,jm,lm,0,grid%lattice )
      call ctoa   ( curr%q,curr%q,dlam,dphi,im,jm,lm,0,grid%lattice )
      call ctoa_winds ( curr%u,curr%v,tmp1,tmp2,dlam,dphi,im,jm,lm,grid%lattice )
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      vintut(i,j) = vintut(i,j) + tmp1(i,j,L)*curr%t(i,j,L)  *dsig(L)* pkn(i,j,L)
      vintvt(i,j) = vintvt(i,j) + tmp2(i,j,L)*curr%t(i,j,L)  *dsig(L)* pkn(i,j,L)
      vintuq(i,j) = vintuq(i,j) + tmp1(i,j,L)*curr%q(i,j,L,1)*dsig(L)
      vintvq(i,j) = vintvq(i,j) + tmp2(i,j,L)*curr%q(i,j,L,1)*dsig(L)
      enddo
      enddo
      enddo

      if( ivaveut.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivaveut) = qdiag(i,j,ivaveut) + vintut(i,j)
      enddo
      enddo
      endif
      if( ivavevt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivavevt) = qdiag(i,j,ivavevt) + vintvt(i,j)
      enddo
      enddo
      endif
      if( ivaveuq.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivaveuq) = qdiag(i,j,ivaveuq) + vintuq(i,j)*1000
      enddo
      enddo
      endif
      if( ivavevq.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivavevq) = qdiag(i,j,ivavevq) + vintvq(i,j)*1000
      enddo
      enddo
      endif

      endif    ! End Convergence Diagnostic

c Total Precipitable Water (gm/cm**2)
c -----------------------------------
      if( itpw.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = 0.0
      enddo
      enddo
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = qbar(i,j) + curr%q(i,j,L,1)*dsig(L)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,itpw) = qdiag(i,j,itpw) + qbar(i,j)*10*curr%p(i,j)/grav
      enddo
      enddo
      endif
                                                                                
c Total Precipitable Analysis Increment (mm/day)
c ----------------------------------------------
      if( ivaveqiau.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = 0.0
      enddo
      enddo
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = qbar(i,j) + dqiau(i,j,L)*dsig(L)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivaveqiau) = qdiag(i,j,ivaveqiau) + qbar(i,j)*(100*86400/grav)
      enddo
      enddo
      endif

c Vertically Averaged Analysis Temperature Increment (K/day)
c ----------------------------------------------------------
      if( ivavetiau.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = 0.0
      enddo
      enddo
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = qbar(i,j) + dtiau(i,j,L)*pkn(i,j,l)*dsig(L)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivavetiau) = qdiag(i,j,ivavetiau) + qbar(i,j)*pinv(i,j)*86400
      enddo
      enddo
      endif

c Vertically Averaged Moist-T Increment (K/day)
c ---------------------------------------------
      if( ivdtmoist.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = 0.0
      enddo
      enddo
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = qbar(i,j) + tend%moist%dt(i,j,L)*pkn(i,j,l)*dsig(L)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivdtmoist) = qdiag(i,j,ivdtmoist) + qbar(i,j)*pinv(i,j)*86400
      enddo
      enddo
      endif

c Vertically Averaged Turb-T Increment (K/day)
c --------------------------------------------
      if( ivdtturb.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = 0.0
      enddo
      enddo
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = qbar(i,j) + tend%turb%dt(i,j,L)*pkn(i,j,l)*dsig(L)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivdtturb) = qdiag(i,j,ivdtturb) + qbar(i,j)*pinv(i,j)*86400
      enddo
      enddo
      endif

c Vertically Averaged RADLW Temperature Increment (K/day)
c -------------------------------------------------------
      if( ivdtradlw.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = 0.0
      enddo
      enddo
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = qbar(i,j) + ( tend%lw%dt(i,j,L)
     .          + coup%lw%dlwdtg(i,j,L)*(coup%land%tgc(i,j)-coup%lw%tg0c(i,j)) )*pkn(i,j,l)*dsig(L)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivdtradlw) = qdiag(i,j,ivdtradlw) + qbar(i,j)*pinv(i,j)*86400
      enddo
      enddo
      endif

c Vertically Averaged RADSW Temperature Increment (K/day)
c -------------------------------------------------------
      if( ivdtradsw.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = 0.0
      enddo
      enddo
      do L=1,Nrphys
      do j=jm1,jm2
      do i=im1,im2
      qbar(i,j) = qbar(i,j) + tend%sw%dt(i,j,L)*pkn(i,j,l)*dsig(L)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivdtradsw) = qdiag(i,j,ivdtradsw) + qbar(i,j)*radswc(i,j)*pinv(i,j)*86400
      enddo
      enddo
      endif

      nvaveut   = nvaveut   + 1
      nvavevt   = nvavevt   + 1
      nvaveuq   = nvaveuq   + 1
      nvavevq   = nvavevq   + 1
      ntpw      = ntpw      + 1
      nvaveqiau = nvaveqiau + 1
      nvavetiau = nvavetiau + 1
      nvdtmoist = nvdtmoist + 1
      nvdtturb  = nvdtturb  + 1
      nvdtradlw = nvdtradlw + 1
      nvdtradsw = nvdtradsw + 1
 
C *****************************************************************     
C ****                  Release Workspace                      ****     
C *****************************************************************     

      deallocate (  dlam )
      deallocate (  dphi )
      deallocate (  dsig )
      deallocate (   sig )
      deallocate (  sige )

      deallocate (  pinv  )
      deallocate (  tmp1  )
      deallocate (  tmp2  )
      deallocate (   qbar )
      deallocate ( vintuq )
      deallocate ( vintvq )
      deallocate ( vintut )
      deallocate ( vintvt )
      deallocate (    pkn )
      deallocate (  pknp1 )
      deallocate (  dtiau )
      deallocate (  dqiau )

      call timeend (' step_diag')
      return
      end
