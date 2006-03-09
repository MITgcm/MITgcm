C $Header: /u/gcmpack/MITgcm/pkg/fizhi/do_fizhi.F,v 1.52 2006/03/09 00:00:36 molod Exp $
C $Name:  $
#include "FIZHI_OPTIONS.h"
       subroutine do_fizhi(myIter,myid,
     . idim1,idim2,jdim1,jdim2,Nrphin,Nsxin,Nsyin,im1,im2,jm1,jm2,bi,bj,
     . nchp,nchptot,nchpland,
     . uphy,vphy,thphy,sphy,pephy,lons,lats,Zsurf,
     . ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     . tgz,sst,sice,phis_var,landtype,fracland,emiss,albnirdr,albnirdf,
     . albvisdr,albvisdf,ityp,chfr,alai,agrn,igrd,chlat,chlon,
     . tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,snodep,capac,
     . o3,qstr,co2,cfc11,cfc12,cfc22,n2o,methane,
     . iras,nlwcld,cldtotlwin,cldraslwin,cldlsplwin,nlwlz,lwlzin,
     . nswcld,cldtotswin,cldrasswin,cldlspswin,nswlz,swlzin,imstturbsw,
     . imstturblw,qliqaveswin,qliqavelwin,fccaveswin,fccavelwin,
     . rainconin,rainlspin,snowfallin,
     . duphy,dvphy,dthphy,dsphy)
c-----------------------------------------------------------------------
c Interface routine to calculate physics increments - calls fizhi_driver.
c Purpose of this routine is to set up arrays local to fizhi and 'save'
c them from one iteration to the next, and act as interface between the
c model common blocks (held in fizhi_wrapper) and fizhi_driver. 
c Copies of variables that are 'shadowed' are made here without shadows
c for passing to fizhi_driver.
c Note: routine is called from inside a bi-bj loop
c
c-----------------------------------------------------------------------
      implicit none
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "chronos.h"

C Argument list declarations
      integer myIter,myid,im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
      integer Nrphin,Nsxin,Nsyin,bi,bj,nchp
      integer nchptot(Nsxin,Nsyin),nchpland(Nsxin,Nsyin)
      _RL uphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL vphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL thphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL sphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL pephy(idim1:idim2,jdim1:jdim2,Nrphin+1,Nsxin,Nsyin)
      _RS lons(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RS lats(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RS Zsurf(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RL ctmt(nchp,Nsxin,Nsyin),xxmt(nchp,Nsxin,Nsyin)
      _RL yymt(nchp,Nsxin,Nsyin)
      _RL zetamt(nchp,Nsxin,Nsyin)
      _RL xlmt(nchp,Nrphin,Nsxin,Nsyin),khmt(nchp,Nrphin,Nsxin,Nsyin)
      _RL tke(nchp,Nrphin,Nsxin,Nsyin)
      _RL tgz(im2,jm2,Nsxin,Nsyin)
      _RL sst(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RL sice(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RL phis_var(im2,jm2,Nsxin,Nsyin)
      integer landtype(im2,jm2,Nsxin,Nsyin)
      _RL fracland(im2,jm2,Nsxin,Nsyin),emiss(im2,jm2,10,Nsxin,Nsyin)
      _RL albvisdr(im2,jm2,Nsxin,Nsyin),albvisdf(im2,jm2,Nsxin,Nsyin)
      _RL albnirdr(im2,jm2,Nsxin,Nsyin),albnirdf(im2,jm2,Nsxin,Nsyin)
      _RL chfr(nchp,Nsxin,Nsyin),alai(nchp,Nsxin,Nsyin)
      _RL agrn(nchp,Nsxin,Nsyin)
      integer ityp(nchp,Nsxin,Nsyin),igrd(nchp,Nsxin,Nsyin)
      _RL chlat(nchp,Nsxin,Nsyin),chlon(nchp,Nsxin,Nsyin)
      _RL tcanopy(nchp,Nsxin,Nsyin),tdeep(nchp,Nsxin,Nsyin)
      _RL ecanopy(nchp,Nsxin,Nsyin),swetshal(nchp,Nsxin,Nsyin)
      _RL swetroot(nchp,Nsxin,Nsyin),swetdeep(nchp,Nsxin,Nsyin)
      _RL snodep(nchp,Nsxin,Nsyin),capac(nchp,Nsxin,Nsyin)
      _RL o3(im2,jm2,Nrphin,Nsxin,Nsyin)
      _RL qstr(im2,jm2,Nrphin,Nsxin,Nsyin)
      _RL co2,cfc11,cfc12,cfc22,n2o(Nrphin),methane(Nrphin)

      integer iras(Nsxin,Nsyin)
      integer nlwcld(Nsxin,Nsyin),nlwlz(Nsxin,Nsyin)
      integer nswcld(Nsxin,Nsyin),nswlz(Nsxin,Nsyin)
      integer imstturbsw(Nsxin,Nsyin),imstturblw(Nsxin,Nsyin)
      _RL cldtotlwin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL cldraslwin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL cldlsplwin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL lwlzin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL cldtotswin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL cldrasswin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL cldlspswin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL swlzin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL qliqaveswin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL qliqavelwin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL fccaveswin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL fccavelwin(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL rainlspin(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RL rainconin(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RL snowfallin(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)


      _RL duphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL dvphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL dthphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL dsphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)

c Local Variables
      integer ptracer,ntracer
      parameter (ptracer = 1)
      parameter (ntracer = 1)

      _RL xlats(sNx,sNy),xlons(sNx,sNy),sea_ice(sNx,sNy) 
      _RL surfZ(sNx,sNy)
      _RL p(sNx,sNy,Nsx,Nsy)
      _RL u(sNx,sNy,Nrphys),v(sNx,sNy,Nrphys),t(sNx,sNy,Nrphys)
      _RL q(sNx,sNy,Nrphys,ntracer)
      _RL pl(sNx,sNy,Nrphys,Nsx,Nsy),pkl(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL ple(sNx,sNy,Nrphys+1,Nsx,Nsy)
      _RL pkle(sNx,sNy,Nrphys+1,Nsx,Nsy)
      _RL dpres(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL lwdt(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL lwdtclr(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL swdt(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL swdtclr(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL turbu(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL turbv(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL turbt(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL turbq(sNx,sNy,Nrphys,ntracer,Nsx,Nsy)
      _RL moistu(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL moistv(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL moistt(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL moistq(sNx,sNy,Nrphys,ntracer,Nsx,Nsy)
      _RL radswt(sNx,sNy,Nsx,Nsy),radswg(sNx,sNy,Nsx,Nsy)
      _RL swgclr(sNx,sNy,Nsx,Nsy)
      _RL fdirpar(sNx,sNy,Nsx,Nsy),fdifpar(sNx,sNy,Nsx,Nsy)
      _RL osr(sNx,sNy,Nsx,Nsy),osrclr(sNx,sNy,Nsx,Nsy)
      _RL tg0(sNx,sNy,Nsx,Nsy),radlwg(sNx,sNy,Nsx,Nsy)
      _RL lwgclr(sNx,sNy,Nsx,Nsy),st4(sNx,sNy,Nsx,Nsy)
      _RL dst4(sNx,sNy,Nsx,Nsy),dlwdtg(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL qq(sNx,sNy,Nrphys,Nsx,Nsy)
      integer i,j,L
      _RL getcon, kappa, p0kappa, s0, ra
      _RL cosz(sNx,sNy)
      _RL cldtot_lw(sNx,sNy,Nrphys)
      _RL cldras_lw(sNx,sNy,Nrphys)
      _RL cldlsp_lw(sNx,sNy,Nrphys)
      _RL lwlz(sNx,sNy,Nrphys)
      _RL cldtot_sw(sNx,sNy,Nrphys)
      _RL cldras_sw(sNx,sNy,Nrphys)
      _RL cldlsp_sw(sNx,sNy,Nrphys)
      _RL swlz(sNx,sNy,Nrphys)
      _RL qliqavesw(sNx,sNy,Nrphys)
      _RL qliqavelw(sNx,sNy,Nrphys)
      _RL fccavesw(sNx,sNy,Nrphys)
      _RL fccavelw(sNx,sNy,Nrphys)
      _RL rainlsp(sNx,sNy)
      _RL raincon(sNx,sNy)
      _RL snowfall(sNx,sNy)

      _RL tempij(sNx,sNy)

      logical alarm
      external alarm

      common /saver/ lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq
      common /saver/ moistu,moistv,moistt,moistq
      common /saver/ radswg,swgclr,fdirpar,fdifpar,osr,osrclr,tg0,radlwg
      common /saver/ st4,dst4,dlwdtg
      common /saver/ qq
      common /saver/ pl,ple,dpres,pkle,pkl
 
C***********************************************************************
C Unshadow input arrays (and make 'fizhi theta' from true theta)
C***********************************************************************
      kappa = getcon('KAPPA')
      p0kappa = 1000.0 ** kappa
      S0 = getcon('S0')
      
      do j = jm1,jm2
      do i = im1,im2
       xlats(i,j) = lats(i,j,bi,bj)
       xlons(i,j) = lons(i,j,bi,bj)
       surfZ(i,j) = Zsurf(i,j,bi,bj)
      enddo
      enddo

      call astro ( nymd,nhms, xlats,xlons, im2*jm2, cosz,ra )
      do j=jm1,jm2
      do i=im1,im2
       radswt(i,j,bi,bj) = S0*(1.0/ra**2)*cosz(i,j)
      enddo
      enddo

      if( alarm('moist') .or. alarm('turb')   .or.
     .    alarm('radsw') .or. alarm('radlw') ) then

C compute pressures - all pressure are converted here to hPa
      do j = jm1,jm2
      do i = im1,im2
       ple(i,j,Nrphys+1,bi,bj) = pephy(i,j,Nrphys+1,bi,bj)/100.
       pkle(i,j,Nrphys+1,bi,bj)=(pephy(i,j,Nrphys+1,bi,bj)/100.) **kappa
       p(i,j,bi,bj) = pephy(i,j,Nrphys+1,bi,bj)/100.
       sea_ice(i,j) = sice(i,j,bi,bj)
       raincon(i,j) = rainconin(i,j,bi,bj)
       rainlsp(i,j) = rainlspin(i,j,bi,bj)
       snowfall(i,j) = snowfallin(i,j,bi,bj)
      enddo
      enddo
      do L = 1,Nrphys
      do j = jm1,jm2
      do i = im1,im2
       u(i,j,L) = uphy(i,j,L,bi,bj)
       v(i,j,L) = vphy(i,j,L,bi,bj)
       t(i,j,L) = thphy(i,j,L,bi,bj)/p0kappa
       q(i,j,L,1) = sphy(i,j,L,bi,bj)
       pl(i,j,L,bi,bj) = (pephy(i,j,L,bi,bj)+pephy(i,j,L+1,bi,bj))/200.
       dpres(i,j,L,bi,bj)=(pephy(i,j,L+1,bi,bj)-pephy(i,j,L,bi,bj))/100.
       ple(i,j,L,bi,bj) = pephy(i,j,L,bi,bj)/100.
       if (ple(i,j,L,bi,bj).gt.0.) then
        pkle(i,j,L,bi,bj) = ple(i,j,L,bi,bj) **kappa
       else
        pkle(i,j,L,bi,bj) = 0.
       endif
       cldtot_lw(i,j,L) = cldtotlwin(i,j,L,bi,bj)
       cldlsp_lw(i,j,L) = cldlsplwin(i,j,L,bi,bj)
       cldras_lw(i,j,L) = cldraslwin(i,j,L,bi,bj)
       lwlz(i,j,L) = lwlzin(i,j,L,bi,bj)
       qliqavelw(i,j,L) = qliqavelwin(i,j,L,bi,bj)
       fccavelw(i,j,L) = fccavelwin(i,j,L,bi,bj)
       cldtot_sw(i,j,L) = cldtotswin(i,j,L,bi,bj)
       cldlsp_sw(i,j,L) = cldlspswin(i,j,L,bi,bj)
       cldras_sw(i,j,L) = cldrasswin(i,j,L,bi,bj)
       swlz(i,j,L) = swlzin(i,j,L,bi,bj)
       qliqavesw(i,j,L) = qliqaveswin(i,j,L,bi,bj)
       fccavesw(i,j,L) = fccaveswin(i,j,L,bi,bj)
      enddo
      enddo
      enddo

      call pkappa (im2,jm2,Nrphys,ple(1,1,1,bi,bj),pkle(1,1,1,bi,bj),
     .                                                 pkl(1,1,1,bi,bj))

      call fizhi_driver(myid,im2,jm2,Nrphys,bi,bj,ptracer,ntracer,xlats,
     . xlons,p(1,1,bi,bj),u,v,t,q,pl(1,1,1,bi,bj),ple(1,1,1,bi,bj),
     . dpres(1,1,1,bi,bj),pkle(1,1,1,bi,bj),pkl(1,1,1,bi,bj),surfZ,
     . fracland(1,1,bi,bj),landtype(1,1,bi,bj),radswt(1,1,bi,bj),
     . phis_var(1,1,bi,bj),tgz(1,1,bi,bj),sea_ice,nchp,chlat(1,bi,bj),
     . chlon(1,bi,bj),igrd(1,bi,bj),nchptot(bi,bj),nchpland(bi,bj),
     . chfr(1,bi,bj),ityp(1,bi,bj),tcanopy(1,bi,bj),tdeep(1,bi,bj),
     . ecanopy(1,bi,bj),swetshal(1,bi,bj),swetroot(1,bi,bj),
     . swetdeep(1,bi,bj),capac(1,bi,bj),snodep(1,bi,bj),
     . ctmt(1,bi,bj),xxmt(1,bi,bj),yymt(1,bi,bj),zetamt(1,bi,bj),
     . xlmt(1,1,bi,bj),khmt(1,1,bi,bj),tke(1,1,bi,bj),
     . albvisdr(1,1,bi,bj),albvisdf(1,1,bi,bj),albnirdr(1,1,bi,bj),
     . albnirdf(1,1,bi,bj),emiss(1,1,1,bi,bj),alai(1,bi,bj),
     . agrn(1,bi,bj),
     . qstr(1,1,1,bi,bj),o3(1,1,1,bi,bj),
     . co2,cfc11,cfc12,cfc22,methane,n2o,
     . lwdt(1,1,1,bi,bj),lwdtclr(1,1,1,bi,bj),swdt(1,1,1,bi,bj),
     . swdtclr(1,1,1,bi,bj),turbu(1,1,1,bi,bj),turbv(1,1,1,bi,bj),
     . turbt(1,1,1,bi,bj),turbq(1,1,1,1,bi,bj),moistu(1,1,1,bi,bj),
     . moistv(1,1,1,bi,bj),moistt(1,1,1,bi,bj),moistq(1,1,1,1,bi,bj),
     . radswg(1,1,bi,bj),swgclr(1,1,bi,bj),fdirpar(1,1,bi,bj),
     . fdifpar(1,1,bi,bj),osr(1,1,bi,bj),osrclr(1,1,bi,bj),
     . tg0(1,1,bi,bj),radlwg(1,1,bi,bj),lwgclr(1,1,bi,bj),
     . st4(1,1,bi,bj),dst4(1,1,bi,bj),dlwdtg(1,1,1,bi,bj),
     . rainlsp,raincon,snowfall,iras(bi,bj),
     . nlwcld(bi,bj),cldtot_lw,cldras_lw,cldlsp_lw,nlwlz(bi,bj),lwlz,
     . nswcld(bi,bj),cldtot_sw,cldras_sw,cldlsp_sw,nswlz(bi,bj),swlz,
     . imstturbsw(bi,bj),imstturblw(bi,bj),qliqavesw,qliqavelw,
     . fccavesw,fccavelw,qq(1,1,1,bi,bj))

      do L = 1,Nrphys
      do j = jm1,jm2
      do i = im1,im2
       cldtotlwin(i,j,L,bi,bj) = cldtot_lw(i,j,L)
       cldlsplwin(i,j,L,bi,bj) = cldlsp_lw(i,j,L)
       cldraslwin(i,j,L,bi,bj) = cldras_lw(i,j,L)
       lwlzin(i,j,L,bi,bj) = lwlz(i,j,L)
       qliqavelwin(i,j,L,bi,bj) = qliqavelw(i,j,L)
       fccavelwin(i,j,L,bi,bj) = fccavelw(i,j,L)
       cldtotswin(i,j,L,bi,bj) = cldtot_sw(i,j,L)
       cldlspswin(i,j,L,bi,bj) = cldlsp_sw(i,j,L)
       cldrasswin(i,j,L,bi,bj) = cldras_sw(i,j,L)
       swlzin(i,j,L,bi,bj) = swlz(i,j,L)
       qliqaveswin(i,j,L,bi,bj) = qliqavesw(i,j,L)
       fccaveswin(i,j,L,bi,bj) = fccavesw(i,j,L)
      enddo
      enddo
      enddo

      do j = jm1,jm2
      do i = im1,im2
       rainconin(i,j,bi,bj) = raincon(i,j)
       rainlspin(i,j,bi,bj) = rainlsp(i,j)
       snowfallin(i,j,bi,bj) = snowfall(i,j)
      enddo
      enddo

      endif

      do L = 1,Nrphys
      do j = jm1,jm2
      do i = im1,im2
       duphy(i,j,L,bi,bj) = moistu(i,j,L,bi,bj) + turbu(i,j,L,bi,bj)
       dvphy(i,j,L,bi,bj) = moistv(i,j,L,bi,bj) + turbv(i,j,L,bi,bj)
       dthphy(i,j,L,bi,bj) = ((moistt(i,j,L,bi,bj)+turbt(i,j,L,bi,bj)+
     .   lwdt(i,j,L,bi,bj) + 
     .   dlwdtg(i,j,L,bi,bj) * (tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) +
     .   swdt(i,j,L,bi,bj)*radswt(i,j,bi,bj) )*p0kappa ) / p(i,j,bi,bj)
       dsphy(i,j,L,bi,bj) = (moistq(i,j,L,1,bi,bj)+turbq(i,j,L,1,bi,bj))
     .                                    /p(i,j,bi,bj)
      enddo
      enddo
      enddo

      call fizhi_step_diag(myid,p,u,v,t,q,qq,pkl,dpres,
     .  radswt,radswg,swgclr,osr,osrclr,st4,dst4,tgz,tg0,radlwg,lwgclr,
     .  turbu,turbv,turbt,turbq,moistu,moistv,moistt,moistq,
     .  lwdt,swdt,lwdtclr,swdtclr,dlwdtg,
     .  im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,bi,bj,ntracer)

      return
      end
