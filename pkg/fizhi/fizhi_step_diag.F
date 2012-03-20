C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_step_diag.F,v 1.20 2012/03/20 19:50:45 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      SUBROUTINE FIZHI_STEP_DIAG(myid,p,uphy,vphy,thphy,sphy,qq,pk,dp,
     &  radswt,radswg,swgclr,osr,osrclr,st4,dst4,tgz,tg0,radlwg,lwgclr,
     &  turbu,turbv,turbt,turbq,moistu,moistv,moistt,moistq,
     &  lwdt,swdt,lwdtclr,swdtclr,dlwdtg,
     &  im1,im2,jm1,jm2,Nrphys,Nbi,Nbj,bi,bj,ntracer)
C***********************************************************************
      IMPLICIT NONE

      INTEGER myid,im1,im2,jm1,jm2,Nrphys,Nbi,Nbj,bi,bj,ntracer
      _RL p(im2,jm2,Nbi,Nbj)
      _RL uphy(im2,jm2,Nrphys)
      _RL vphy(im2,jm2,Nrphys)
      _RL thphy(im2,jm2,Nrphys)
      _RL sphy(im2,jm2,Nrphys)
      _RL qq(im2,jm2,Nrphys,Nbi,Nbj),pk(im2,jm2,Nrphys,Nbi,Nbj)
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

      INTEGER  i,j,L
      _RL getcon, gravity
      _RL pinv(im2,jm2), qbar(im2,jm2),tmpdiag(im2,jm2)
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  diagnostics_is_on
      EXTERNAL diagnostics_is_on
#endif

C **********************************************************************

#ifdef ALLOW_DIAGNOSTICS
      do j=jm1,jm2
      do i=im1,im2
      pinv(i,j) = 1.0 / p(i,j,bi,bj)
      enddo
      enddo

c Surface Pressure (mb)
c ---------------------------------
      call diagnostics_fill(p(1,1,bi,bj),'PS      ',0,1,3,bi,bj,myid)

c Incident Solar Radiation (W/m**2)
c ---------------------------------
      call diagnostics_fill(radswt(1,1,bi,bj),'RADSWT  ',
     &                      0,1,3,bi,bj,myid)

c Net Solar Radiation at the Ground (W/m**2)
c ------------------------------------------
      if(diagnostics_is_on('RADSWG  ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = radswg(i,j,bi,bj)*radswt(i,j,bi,bj)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'RADSWG  ',0,1,3,bi,bj,myid)
      endif

c Net Clear Sky Solar Radiation at the Ground (W/m**2)
c ----------------------------------------------------
      if(diagnostics_is_on('SWGCLR  ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = swgclr(i,j,bi,bj)*radswt(i,j,bi,bj)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'SWGCLR  ',0,1,3,bi,bj,myid)
      endif

c Outgoing Solar Radiation at top (W/m**2)
c -----------------------------------------
      if(diagnostics_is_on('OSR     ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = (1.0-osr(i,j,bi,bj))*radswt(i,j,bi,bj)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'OSR     ',0,1,3,bi,bj,myid)
      endif

c Outgoing Clear Sky Solar Radiation at top (W/m**2)
c ---------------------------------------------------
      if(diagnostics_is_on('OSRCLR  ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = (1.0-osrclr(i,j,bi,bj))*radswt(i,j,bi,bj)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'OSRCLR  ',0,1,3,bi,bj,myid)
      endif

c Planetary Albedo
c ----------------
      if(diagnostics_is_on('PLALBEDO',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        if(radswt(i,j,bi,bj).ne.0.) then
         tmpdiag(i,j) = osr(i,j,bi,bj)
        else
         tmpdiag(i,j) = 0.
        endif
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'PLALBEDO',0,1,3,bi,bj,myid)
      endif

c Upward Longwave Flux at the Ground (W/m**2)
c -------------------------------------------
      if(diagnostics_is_on('LWGUP   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = st4(i,j,bi,bj)
     &                 + dst4(i,j,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj))
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'LWGUP   ',0,1,3,bi,bj,myid)
      endif

c Net Longwave Flux at the Ground (W/m**2)
c ----------------------------------------
      if(diagnostics_is_on('RADLWG  ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = radlwg(i,j,bi,bj) +
     &                  dst4(i,j,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj))
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'RADLWG  ',0,1,3,bi,bj,myid)
      endif

c Net Longwave Flux at the Ground Clear Sky (W/m**2)
c --------------------------------------------------
      if(diagnostics_is_on('LWGCLR  ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = lwgclr(i,j,bi,bj) +
     &                  dst4(i,j,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj))
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'LWGCLR  ',0,1,3,bi,bj,myid)
      endif

C **********************************************************************
      do L=1,Nrphys

c Total Diabatic U-Tendency (m/sec/day)
c -------------------------------------
      if(diagnostics_is_on('DIABU   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = (moistu (i,j,L,bi,bj)+turbu(i,j,L,bi,bj) )*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'DIABU   ',L,1,3,bi,bj,myid)
      endif

c Total Diabatic V-Tendency (m/sec/day)
c -------------------------------------
      if(diagnostics_is_on('DIABV   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = (moistv (i,j,L,bi,bj)+turbv(i,j,L,bi,bj) )*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'DIABV   ',L,1,3,bi,bj,myid)
      endif

c Total Diabatic T-Tendency (deg/day)
c -----------------------------------
      if(diagnostics_is_on('DIABT   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) =
     &   ( turbt(i,j,L,bi,bj) + moistt(i,j,L,bi,bj) +
     &      lwdt(i,j,L,bi,bj) +
     &      dlwdtg(i,j,L,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) +
     &      swdt(i,j,L,bi,bj)*radswt(i,j,bi,bj) )
     &      * pk(i,j,L,bi,bj)*pinv(i,j)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'DIABT   ',L,1,3,bi,bj,myid)
      endif

c Total Diabatic Q-Tendency (g/kg/day)
c ------------------------------------
      if(diagnostics_is_on('DIABQ   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) =
     & ( turbq(i,j,L,1,bi,bj) + moistq(i,j,L,1,bi,bj) ) *
     &                                      pinv(i,j)*86400*1000
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'DIABQ   ',L,1,3,bi,bj,myid)
      endif

c Longwave Heating (deg/day)
c --------------------------
      if(diagnostics_is_on('RADLW   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) =
     & ( lwdt(i,j,l,bi,bj) +
     &            dlwdtg (i,j,L,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) )
     &                      * pk(i,j,l,bi,bj)*pinv(i,j)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'RADLW   ',L,1,3,bi,bj,myid)
      endif

c Longwave Heating Clear-Sky (deg/day)
c ------------------------------------
      if(diagnostics_is_on('LWCLR   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) =
     & ( lwdtclr(i,j,l,bi,bj) +
     &            dlwdtg (i,j,L,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) )
     &                      * pk(i,j,l,bi,bj)*pinv(i,j)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'LWCLR   ',L,1,3,bi,bj,myid)
      endif

c Solar Radiative Heating (deg/day)
c ---------------------------------
      if(diagnostics_is_on('RADSW   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) =
     &  + swdt(i,j,l,bi,bj)*radswt(i,j,bi,bj)*
     &                   pk(i,j,l,bi,bj)*pinv(i,j)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'RADSW   ',L,1,3,bi,bj,myid)
      endif

c Clear Sky Solar Radiative Heating (deg/day)
c -------------------------------------------
      if(diagnostics_is_on('SWCLR   ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) =
     &  + swdtclr(i,j,l,bi,bj)*radswt(i,j,bi,bj)*
     &                   pk(i,j,l,bi,bj)*pinv(i,j)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'SWCLR   ',L,1,3,bi,bj,myid)
      endif

c Averaged U-Field (m/sec)
c ------------------------
      if(diagnostics_is_on('UWND    ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = uphy(i,j,L)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'UWND    ',L,1,3,bi,bj,myid)
      endif

c Averaged V-Field (m/sec)
c ------------------------
      if(diagnostics_is_on('VWND    ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = vphy(i,j,L)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'VWND    ',L,1,3,bi,bj,myid)
      endif

c Averaged T-Field (deg)
c ----------------------
      if(diagnostics_is_on('TMPU    ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = thphy(i,j,L)*pk(i,j,L,bi,bj)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'TMPU    ',L,1,3,bi,bj,myid)
      endif

c Averaged QQ-Field (m/sec)**2
c ----------------------------
      if(diagnostics_is_on('TKE     ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = qq(i,j,L,bi,bj)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'TKE     ',L,1,3,bi,bj,myid)
      endif

c Averaged Q-Field (g/kg)
c -----------------------
      if(diagnostics_is_on('SPHU    ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
        tmpdiag(i,j) = sphy(i,j,L) * 1000.
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'SPHU    ',L,1,3,bi,bj,myid)
      endif

      enddo

C **********************************************************************

c Vertically Averaged Moist-T Increment (K/day)
c ---------------------------------------------
      if(diagnostics_is_on('VDTMOIST',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
       qbar(i,j) = 0.0
       enddo
       enddo
       do L=1,Nrphys
       do j=jm1,jm2
       do i=im1,im2
       qbar(i,j) = qbar(i,j) +
     &             moistt(i,j,L,bi,bj)*pk(i,j,l,bi,bj)*dp(i,j,L,bi,bj)
       enddo
       enddo
       enddo
       do j=jm1,jm2
       do i=im1,im2
       tmpdiag(i,j) = qbar(i,j)*pinv(i,j)*pinv(i,j)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'VDTMOIST',0,1,3,bi,bj,myid)
      endif

c Vertically Averaged Turb-T Increment (K/day)
c --------------------------------------------
      if(diagnostics_is_on('VDTTURB ',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
       qbar(i,j) = 0.0
       enddo
       enddo
       do L=1,Nrphys
       do j=jm1,jm2
       do i=im1,im2
       qbar(i,j) = qbar(i,j) +
     &             turbt(i,j,L,bi,bj)*pk(i,j,l,bi,bj)*dp(i,j,L,bi,bj)
       enddo
       enddo
       enddo
       do j=jm1,jm2
       do i=im1,im2
       tmpdiag(i,j) = qbar(i,j)*pinv(i,j)*pinv(i,j)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'VDTTURB ',0,1,3,bi,bj,myid)
      endif

c Vertically Averaged RADLW Temperature Increment (K/day)
c -------------------------------------------------------
      if(diagnostics_is_on('VDTRADLW',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
       qbar(i,j) = 0.0
       enddo
       enddo
       do L=1,Nrphys
       do j=jm1,jm2
       do i=im1,im2
        qbar(i,j) = qbar(i,j) + ( lwdt(i,j,L,bi,bj) +
     &  dlwdtg(i,j,L,bi,bj)*(tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) )
     &             *pk(i,j,l,bi,bj)*dp(i,j,L,bi,bj)
       enddo
       enddo
       enddo
       do j=jm1,jm2
       do i=im1,im2
       tmpdiag(i,j) = qbar(i,j)*pinv(i,j)*pinv(i,j)*86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'VDTRADLW',0,1,3,bi,bj,myid)
      endif

c Vertically Averaged RADSW Temperature Increment (K/day)
c -------------------------------------------------------
      if(diagnostics_is_on('VDTRADSW',myid) ) then
       do j=jm1,jm2
       do i=im1,im2
       qbar(i,j) = 0.0
       enddo
       enddo
       do L=1,Nrphys
       do j=jm1,jm2
       do i=im1,im2
        qbar(i,j) = qbar(i,j) +
     &             swdt(i,j,L,bi,bj)*pk(i,j,l,bi,bj)*dp(i,j,L,bi,bj)
       enddo
       enddo
       enddo
       do j=jm1,jm2
       do i=im1,im2
       tmpdiag(i,j) = qbar(i,j) *
     &             radswt(i,j,bi,bj) * pinv(i,j) * pinv(i,j) * 86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'VDTRADSW',0,1,3,bi,bj,myid)
      endif

c Total Precipitable Water (g/cm^2)
c ---------------------------------------------
      if(diagnostics_is_on('TPW     ',myid) ) then
       gravity = getcon('GRAVITY')
       do j=jm1,jm2
       do i=im1,im2
       qbar(i,j) = 0.0
       enddo
       enddo
       do L=1,Nrphys
       do j=jm1,jm2
       do i=im1,im2
       qbar(i,j) = qbar(i,j) +
     &             sphy(i,j,L)*dp(i,j,L,bi,bj)
       enddo
       enddo
       enddo
       do j=jm1,jm2
       do i=im1,im2
       tmpdiag(i,j) = qbar(i,j)*10. _d 0 /gravity
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'TPW     ',0,1,3,bi,bj,myid)
      endif
#endif
      return
      end
