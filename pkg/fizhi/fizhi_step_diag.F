C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_step_diag.F,v 1.7 2004/07/26 19:51:08 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine fizhi_step_diag(myThid,p,uphy,vphy,thphy,sphy,qq,pk,dp,
     .  radswt,radswg,swgclr,osr,osrclr,st4,dst4,tgz,tg0,radlwg,lwgclr,
     .  turbu,turbv,turbt,turbq,moistu,moistv,moistt,moistq,
     .  lwdt,swdt,lwdtclr,swdtclr,dlwdtg,
     .  im1,im2,jm1,jm2,Nrphys,Nbi,Nbj,bi,bj,ntracer)
C***********************************************************************
      implicit none

#ifdef ALLOW_DIAGNOSTICS
#include "SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"
#endif

      integer myThid,im1,im2,jm1,jm2,Nrphys,Nbi,Nbj,bi,bj,ntracer
      _RL p(im2,jm2,Nbi,Nbj)
      _RL uphy(im2,jm2,Nrphys,Nbi,Nbj)
      _RL vphy(im2,jm2,Nrphys,Nbi,Nbj)
      _RL thphy(im2,jm2,Nrphys,Nbi,Nbj)
      _RL sphy(im2,jm2,Nrphys,Nbi,Nbj)
      _RL qq(im2,jm2,Nrphys),pk(im2,jm2,Nrphys,Nbi,Nbj)
      _RL dp(im2,jm2,Nrphys,Nbi,Nbj)
      _RL radswt(im2,jm2,Nbi,Nbj),radswg(im2,jm2,Nbi,Nbj)
      _RL swgclr(im2,jm2,Nbi,Nbj),osr(im2,jm2,Nbi,Nbj)
      _RL osrclr(im2,jm2,Nbi,Nbj),st4(im2,jm2,Nbi,Nbj)
      _RL dst4(im2,jm2,Nbi,Nbj),tgz(im2,jm2,Nbi,Nbj)
      _RL tg0(im2,jm2,Nbi,Nbj),radlwg(im2,jm2,Nbi,Nbj)
      _RL lwgclr(im2,jm2,Nbi,Nbj)
      _RL turbu(im2,jm2,Nrphys,Nbi,Nbj)
      _RL turbv(im2,jm2,Nrphys,Nbi,Nbj)
      _RL turbt(im2,jm2,Nrphys,Nbi,Nbj)
      _RL turbq(im2,jm2,Nrphys,ntracer,Nbi,Nbj)
      _RL moistu(im2,jm2,Nrphys,Nbi,Nbj)
      _RL moistv(im2,jm2,Nrphys,Nbi,Nbj)
      _RL moistt(im2,jm2,Nrphys,Nbi,Nbj)
      _RL moistq(im2,jm2,Nrphys,ntracer,Nbi,Nbj)
      _RL lwdt(im2,jm2,Nrphys,Nbi,Nbj)
      _RL swdt(im2,jm2,Nrphys,Nbi,Nbj)
      _RL lwdtclr(im2,jm2,Nrphys,Nbi,Nbj)
      _RL swdtclr(im2,jm2,Nrphys,Nbi,Nbj)
      _RL dlwdtg(im2,jm2,Nrphys,Nbi,Nbj)

      integer  i,j,L
      _RL pinv(im2,jm2), qbar(im2,jm2)

C **********************************************************************        

      do j=jm1,jm2
      do i=im1,im2
      pinv(i,j) = 1.0 / p(i,j,bi,bj)
      enddo
      enddo

c Incident Solar Radiation (W/m**2)
c ---------------------------------
      if (iradswt.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradswt,bi,bj)= qdiag(i,j,iradswt,bi,bj) +
     .                                                 radswt(i,j,bi,bj)
      enddo
      enddo
      endif
                                                                                
c Net Solar Radiation at the Ground (W/m**2)
c ------------------------------------------
      if (iradswg.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradswg,bi,bj) = qdiag(i,j,iradswg,bi,bj) + 
     .                               radswg(i,j,bi,bj)*radswt(i,j,bi,bj)
      enddo
      enddo
      endif
                                                                                
c Net Clear Sky Solar Radiation at the Ground (W/m**2)
c ----------------------------------------------------
      if (iswgclr.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iswgclr,bi,bj) = qdiag(i,j,iswgclr,bi,bj) + 
     .                               swgclr(i,j,bi,bj)*radswt(i,j,bi,bj)
      enddo
      enddo
      endif
                                                                                
c Outgoing Solar Radiation at top (W/m**2)
c -----------------------------------------
      if (iosr.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iosr,bi,bj) = qdiag(i,j,iosr,bi,bj) + 
     .                            (1.0-osr(i,j,bi,bj))*radswt(i,j,bi,bj)
      enddo
      enddo
      endif
                                                                                
c Outgoing Clear Sky Solar Radiation at top (W/m**2)
c ---------------------------------------------------
      if (iosrclr.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iosrclr,bi,bj) = qdiag(i,j,iosrclr,bi,bj) + 
     .                         (1.0-osrclr(i,j,bi,bj))*radswt(i,j,bi,bj)         
      enddo
      enddo
      endif
                                                                                
c Upward Longwave Flux at the Ground (W/m**2)
c -------------------------------------------
      if (ilwgup.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ilwgup,bi,bj) = qdiag(i,j,ilwgup,bi,bj) + st4(i,j,bi,bj) 
     .                 + dst4(i,j,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj))
      enddo
      enddo
      endif
                                                                                
c Net Longwave Flux at the Ground (W/m**2)
c ----------------------------------------
      if (iradlwg.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradlwg,bi,bj) =  qdiag(i,j,iradlwg,bi,bj) + 
     .                                              radlwg(i,j,bi,bj) + 
     .                  dst4(i,j,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj))
      enddo
      enddo
      endif
                                                                                
c Net Longwave Flux at the Ground Clear Sky (W/m**2)
c --------------------------------------------------
      if (ilwgclr.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ilwgclr,bi,bj) = qdiag(i,j,ilwgclr,bi,bj) + 
     .                                               lwgclr(i,j,bi,bj) + 
     .                   dst4(i,j,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj))
      enddo
      enddo
      endif
                                                                                
      nradswt = nradswt + 1
      nradswg = nradswg + 1
      nswgclr = nswgclr + 1
      nosr    = nosr    + 1
      nosrclr = nosrclr + 1
      nradlwg = nradlwg + 1
      nlwgclr = nlwgclr + 1
      nlwgup  = nlwgup  + 1
                                                                                
C **********************************************************************        
      do L=1,Nrphys

c Total Diabatic U-Tendency (m/sec/day)
c -------------------------------------
      if( idiabu.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idiabu+L-1,bi,bj) = qdiag(i,j,idiabu+L-1,bi,bj)
     .            + ( moistu (i,j,L,bi,bj) + turbu(i,j,L,bi,bj) )*86400
      enddo
      enddo
      endif
                                                                  
c Total Diabatic V-Tendency (m/sec/day)
c -------------------------------------
      if( idiabv.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idiabv+L-1,bi,bj) = qdiag(i,j,idiabv+L-1,bi,bj)
     .            + ( moistv (i,j,L,bi,bj) + turbv(i,j,L,bi,bj) )*86400
      enddo
      enddo
      endif

c Total Diabatic T-Tendency (deg/day)
c -----------------------------------
      if( idiabt.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idiabt+L-1,bi,bj) = qdiag(i,j,idiabt+L-1,bi,bj) +
     .   ( turbt(i,j,L,bi,bj) + moistt(i,j,L,bi,bj) +
     .      lwdt(i,j,L,bi,bj) +
     .      dlwdtg(i,j,L,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) +
     .      swdt(i,j,L,bi,bj)*radswt(i,j,bi,bj) )
     .      * pk(i,j,L,bi,bj)*pinv(i,j)*86400
      enddo
      enddo
      endif
                                                                  
c Total Diabatic Q-Tendency (g/kg/day)
c ------------------------------------
      if( idiabq.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,idiabq+L-1,bi,bj) =   qdiag(i,j,idiabq+L-1,bi,bj) +
     . ( turbq(i,j,L,1,bi,bj) + moistq(i,j,L,1,bi,bj) ) * 
     .                                      pinv(i,j)*86400*1000
      enddo
      enddo
      endif
    
c Longwave Heating (deg/day)
c --------------------------
      if (iradlw.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradlw+l-1,bi,bj) = qdiag(i,j,iradlw+l-1,bi,bj) + 
     . ( lwdt(i,j,l,bi,bj) + 
     .            dlwdtg (i,j,L,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) )
     .                      * pk(i,j,l,bi,bj)*pinv(i,j)*86400
      enddo
      enddo
      endif
                                                                                
c Longwave Heating Clear-Sky (deg/day)
c ------------------------------------
      if (ilwclr.ne.0) then                                                     
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ilwclr+l-1,bi,bj) = qdiag(i,j,ilwclr+l-1,bi,bj) +
     . ( lwdtclr(i,j,l,bi,bj) + 
     .             dlwdtg(i,j,L,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) )
     .                      * pk(i,j,l,bi,bj)*pinv(i,j)*86400
      enddo
      enddo
      endif
                                                                                
c Solar Radiative Heating (deg/day)
c ---------------------------------
      if (iradsw.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iradsw+l-1,bi,bj) = qdiag(i,j,iradsw+l-1,bi,bj) +
     .  + swdt(i,j,l,bi,bj)*radswt(i,j,bi,bj)*
     .                   pk(i,j,l,bi,bj)*pinv(i,j)*86400
      enddo
      enddo
      endif
                                                                                
c Clear Sky Solar Radiative Heating (deg/day)
c -------------------------------------------
      if (iswclr.ne.0) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iswclr+l-1,bi,bj) = qdiag(i,j,iswclr+l-1,bi,bj) +
     .           swdtclr(i,j,l,bi,bj)*radswt(i,j,bi,bj)*
     .                           pk(i,j,l,bi,bj)*pinv(i,j)*86400
      enddo
      enddo
      endif
                                                                                
c Averaged U-Field (m/sec)
c ------------------------
      if( iuwnd.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,iuwnd+L-1,bi,bj) = qdiag(i,j,iuwnd+L-1,bi,bj) + 
     .                                                 uphy(i,j,L,bi,bj)
      enddo
      enddo
      endif

c Averaged V-Field (m/sec)
c ------------------------
      if( ivwnd.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivwnd+L-1,bi,bj) = qdiag(i,j,ivwnd+L-1,bi,bj) + 
     .                                                 vphy(i,j,L,bi,bj)
      enddo
      enddo
      endif

c Averaged T-Field (deg)
c ----------------------
      if( itmpu.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,itmpu+L-1,bi,bj) = qdiag(i,j,itmpu+L-1,bi,bj) + 
     .                               thphy(i,j,L,bi,bj)*pk(i,j,L,bi,bj)
      enddo
      enddo
      endif
                                                                                
c Averaged QQ-Field (m/sec)**2
c ----------------------------
      if( itke.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,itke+L-1,bi,bj) = qdiag(i,j,itke+L-1,bi,bj) + qq(i,j,L)
      enddo
      enddo
      endif
                                                                                
c Averaged Q-Field (g/kg)
c -----------------------
      if( isphu.ne.0 ) then
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,isphu+L-1,bi,bj) = qdiag(i,j,isphu+L-1,bi,bj) + 
     .                                            sphy(i,j,L,bi,bj)*1000
      enddo
      enddo
      endif
                                                                                
      enddo

      ndiabu = ndiabu + 1
      ndiabv = ndiabv + 1
      ndiabt = ndiabt + 1
      ndiabq = ndiabq + 1
      nradlw = nradlw + 1                                                     
      nlwclr = nlwclr + 1                                                     
      nradsw = nradsw + 1                                                     
      nswclr = nswclr + 1                                                     
      nuwnd  = nuwnd  + 1
      nvwnd  = nvwnd  + 1                           
      ntmpu  = ntmpu  + 1                           
      ntke   = ntke   + 1
      nsphu  = nsphu  + 1

C **********************************************************************        

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
      qbar(i,j) = qbar(i,j) + 
     .             moistt(i,j,L,bi,bj)*pk(i,j,l,bi,bj)*dp(i,j,L,bi,bj)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivdtmoist,bi,bj) = qdiag(i,j,ivdtmoist,bi,bj) + 
     .      qbar(i,j)*pinv(i,j)*pinv(i,j)*86400
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
      qbar(i,j) = qbar(i,j) + 
     .             turbt(i,j,L,bi,bj)*pk(i,j,l,bi,bj)*dp(i,j,L,bi,bj)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivdtturb,bi,bj) = qdiag(i,j,ivdtturb,bi,bj) + 
     .      qbar(i,j)*pinv(i,j)*pinv(i,j)*86400
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
      qbar(i,j) = qbar(i,j) + ( lwdt(i,j,L,bi,bj) +
     .  dlwdtg(i,j,L,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) )
     .             *pk(i,j,l,bi,bj)*dp(i,j,L,bi,bj)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivdtradlw,bi,bj) = qdiag(i,j,ivdtradlw,bi,bj) + 
     .      qbar(i,j)*pinv(i,j)*pinv(i,j)*86400
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
      qbar(i,j) = qbar(i,j) + 
     .             swdt(i,j,L,bi,bj)*pk(i,j,l,bi,bj)*dp(i,j,L,bi,bj)
      enddo
      enddo
      enddo
      do j=jm1,jm2
      do i=im1,im2
      qdiag(i,j,ivdtradsw,bi,bj) = qdiag(i,j,ivdtradsw,bi,bj) + 
     . qbar(i,j)*radswt(i,j,bi,bj)*pinv(i,j)*pinv(i,j)*86400
      enddo
      enddo
      endif

      nvdtmoist = nvdtmoist + 1
      nvdtturb  = nvdtturb  + 1
      nvdtradlw = nvdtradlw + 1
      nvdtradsw = nvdtradsw + 1
 
      return
      end
