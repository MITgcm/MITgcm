C $Header: /u/gcmpack/MITgcm/pkg/fizhi/do_fizhi.F,v 1.22 2004/07/19 22:06:59 molod Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"
       subroutine do_fizhi(myid,uphy,vphy,thphy,sphy,pephy,lons,lats,
     . ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     . tgz,sice,phis_var,landtype,fracland,emiss,albnirdr,albnirdf,
     . albvisdr,albvisdf,ityp,chfr,alai,agrn,igrd,chlat,chlon,
     . tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,snodep,capac,
     . o3,qstr,co2,cfc11,cfc12,cfc22,n2o,methane,
     . idim1,idim2,jdim1,jdim2,Nrphin,Nsxin,Nsyin,im1,im2,jm1,jm2,bi,bj,
     . nchp,nchpland,
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
      integer myid,im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
      integer Nrphin,Nsxin,Nsyin,bi,bj,nchp,nchpland
      _RL uphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL vphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL thphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL sphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL pephy(idim1:idim2,jdim1:jdim2,Nrphin+1,Nsxin,Nsyin)
      _RL lons(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RL lats(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RL ctmt(nchp,Nsxin,Nsyin),xxmt(nchp,Nsxin,Nsyin)
      _RL yymt(nchp,Nsxin,Nsyin)
      _RL zetamt(nchp,Nsxin,Nsyin)
      _RL xlmt(nchp,Nrphin,Nsxin,Nsyin),khmt(nchp,Nrphin,Nsxin,Nsyin)
      _RL tke(nchp,Nrphin,Nsxin,Nsyin)
      _RL tgz(im2,jm2,Nsxin,Nsyin)
      _RL sice(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RL phis_var(im2,jm2,Nsxin,Nsyin),landtype(im2,jm2,Nsxin,Nsyin)
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
      _RL o3(im2,jm2,Nsxin,Nsyin),qstr(im2,jm2,Nsxin,Nsyin)
      _RL co2,cfc11,cfc12,cfc22,n2o(Nrphin),methane(Nrphin)
      _RL duphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL dvphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL dthphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL dsphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)

c Local Variables
      integer ptracer,ntracer
      parameter (ptracer = 1)
      parameter (ntracer = 1)
      integer iras,nlwcld,nlwlz,nswcld,nswlz
      integer imstturbsw,imstturblw

      real xlats(sNx,sNy), xlons(sNx,sNy), sea_ice(sNx,sNy) 
      real p(sNx,sNy,Nsx,Nsy)
      real u(sNx,sNy,Nrphys), v(sNx,sNy,Nrphys), t(sNx,sNy,Nrphys)
      real q(sNx,sNy,Nrphys,ntracer)
      real pl(sNx,sNy,Nrphys,Nsx,Nsy),pkl(sNx,sNy,Nrphys,Nsx,Nsy)
      real ple(sNx,sNy,Nrphys+1,Nsx,Nsy),pkle(sNx,sNy,Nrphys+1,Nsx,Nsy)
      real dpres(sNx,sNy,Nrphys,Nsx,Nsy)
      real lwdt(sNx,sNy,Nrphys,Nsx,Nsy),lwdtclr(sNx,sNy,Nrphys,Nsx,Nsy) 
      real swdt(sNx,sNy,Nrphys,Nsx,Nsy),swdtclr(sNx,sNy,Nrphys,Nsx,Nsy)
      real turbu(sNx,sNy,Nrphys,Nsx,Nsy),turbv(sNx,sNy,Nrphys,Nsx,Nsy)
      real turbt(sNx,sNy,Nrphys,Nsx,Nsy)
      real turbq(sNx,sNy,Nrphys,ntracer,Nsx,Nsy)
      real moistu(sNx,sNy,Nrphys,Nsx,Nsy),moistv(sNx,sNy,Nrphys,Nsx,Nsy)
      real moistt(sNx,sNy,Nrphys,Nsx,Nsy)
      real moistq(sNx,sNy,Nrphys,ntracer,Nsx,Nsy)
      real radswt(sNx,sNy,Nsx,Nsy),radswg(sNx,sNy,Nsx,Nsy)
      real swgclr(sNx,sNy,Nsx,Nsy)
      real fdirpar(sNx,sNy,Nsx,Nsy),fdifpar(sNx,sNy,Nsx,Nsy)
      real osr(sNx,sNy,Nsx,Nsy),osrclr(sNx,sNy,Nsx,Nsy)
      real tg0(sNx,sNy,Nsx,Nsy),radlwg(sNx,sNy,Nsx,Nsy)
      real lwgclr(sNx,sNy,Nsx,Nsy),st4(sNx,sNy,Nsx,Nsy)
      real dst4(sNx,sNy,Nsx,Nsy),dlwdtg(sNx,sNy,Nrphys,Nsx,Nsy)
      real rainlsp(sNx,sNy,Nsx,Nsy),raincon(sNx,sNy,Nsx,Nsy)
      real snowfall(sNx,sNy,Nsx,Nsy)
      real cldtot_lw(sNx,sNy,Nrphys,Nsx,Nsy)
      real clras_lw(sNx,sNy,Nrphys,Nsx,Nsy)
      real cldlsp_lw(sNx,sNy,Nrphys,Nsx,Nsy)
      real lwlz(sNx,sNy,Nrphys,Nsx,Nsy)
      real cldtot_sw(sNx,sNy,Nrphys,Nsx,Nsy)
      real clras_sw(sNx,sNy,Nrphys,Nsx,Nsy)
      real cldlsp_sw(sNx,sNy,Nrphys,Nsx,Nsy)
      real swlz(sNx,sNy,Nrphys,Nsx,Nsy)
      real qliqavesw(sNx,sNy,Nrphys,Nsx,Nsy)
      real qliqavelw(sNx,sNy,Nrphys,Nsx,Nsy)
      real fccavesw(sNx,sNy,Nrphys,Nsx,Nsy)
      real fccavelw(sNx,sNy,Nrphys,Nsx,Nsy)
      real qq(sNx,sNy,Nrphys,Nsx,Nsy)

      integer i,j,L
      real getcon, kappa, p0kappa, s0, ra
      real cosz(sNx,sNy)

      logical alarm
      external alarm

      save lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq
      save moistu,moistv,moistt,moistq
      save radswg,swgclr,fdirpar,fdifpar,osr,osrclr,tg0,radlwg
      save st4,dst4,dlwdtg,rainlsp,raincon,snowfall,iras
      save nlwcld,cldtot_lw,clras_lw,cldlsp_lw,nlwlz,lwlz
      save nswcld,cldtot_sw,clras_sw,cldlsp_sw,nswlz,swlz
      save imstturbsw,imstturblw,qliqavesw,qliqavelw,fccavesw,fccavelw
      save qq
      save pl,ple,dpres,pkle,pkl

C***********************************************************************
C Unshadow input arrays (and make 'fizhi theta' from true theta)
C***********************************************************************

      kappa = getcon('KAPPA')
      p0kappa = 1000.0 ** kappa
      S0 = getcon('S0')
      
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
       xlats(i,j) = lats(i,j,bi,bj)
       xlons(i,j) = lons(i,j,bi,bj)
       sea_ice(i,j) = sice(i,j,bi,bj)
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
       pkle(i,j,L,bi,bj) = (ple(i,j,L,bi,bj) /100.) **kappa
      enddo
      enddo
      enddo

      call pkappa (ple(1,1,1,bi,bj),pkle(1,1,1,bi,bj),im2,jm2,Nrphys,
     .                                                 pkl(1,1,1,bi,bj))

      call fizhi_driver(myid,im2,jm2,Nrphys,bi,bj,ptracer,ntracer,xlats,
     . xlons,p(1,1,bi,bj),u,v,t,q,pl(1,1,1,bi,bj),ple(1,1,1,bi,bj),
     . dpres(1,1,1,bi,bj),pkle(1,1,1,bi,bj),pkl(1,1,1,bi,bj),
     . fracland(1,1,bi,bj),landtype(1,1,bi,bj),radswt(1,1,bi,bj),
     . phis_var(1,1,bi,bj),tgz(1,1,bi,bj),sea_ice,nchp,chlat(1,bi,bj),
     . chlon(1,bi,bj),igrd(1,bi,bj),nchpland,chfr(1,bi,bj),ityp(1,bi,bj)
     . ,tcanopy(1,bi,bj),tdeep(1,bi,bj),ecanopy(1,bi,bj),
     . swetshal(1,bi,bj),swetroot(1,bi,bj),swetdeep(1,bi,bj),
     . capac(1,bi,bj),snodep(1,bi,bj),
     . ctmt(1,bi,bj),xxmt(1,bi,bj),yymt(1,bi,bj),zetamt(1,bi,bj),
     . xlmt(1,1,bi,bj),khmt(1,1,bi,bj),tke(1,1,bi,bj),
     . albvisdr(1,1,bi,bj),albvisdf(1,1,bi,bj),albnirdr(1,1,bi,bj),
     . albnirdf(1,1,bi,bj),emiss(1,1,1,bi,bj),alai(1,bi,bj),
     . agrn(1,bi,bj),
     . qstr(1,1,bi,bj),o3(1,1,bi,bj),co2,cfc11,cfc12,cfc22,methane,n2o,
     . lwdt(1,1,1,bi,bj),lwdtclr(1,1,1,bi,bj),swdt(1,1,1,bi,bj),
     . swdtclr(1,1,1,bi,bj),turbu(1,1,1,bi,bj),turbv(1,1,1,bi,bj),
     . turbt(1,1,1,bi,bj),turbq(1,1,1,1,bi,bj),moistu(1,1,1,bi,bj),
     . moistv(1,1,1,bi,bj),moistt(1,1,1,bi,bj),moistq(1,1,1,1,bi,bj),
     . radswg(1,1,bi,bj),swgclr(1,1,bi,bj),fdirpar(1,1,bi,bj),
     . fdifpar(1,1,bi,bj),osr(1,1,bi,bj),osrclr(1,1,bi,bj),
     . tg0(1,1,bi,bj),radlwg(1,1,bi,bj),lwgclr(1,1,bi,bj),
     . st4(1,1,bi,bj),dst4(1,1,bi,bj),dlwdtg(1,1,1,bi,bj),
     . rainlsp(1,1,bi,bj),raincon(1,1,bi,bj),snowfall(1,1,bi,bj),iras,
     . nlwcld,cldtot_lw(1,1,1,bi,bj),clras_lw(1,1,1,bi,bj),
     . cldlsp_lw(1,1,1,bi,bj),nlwlz,lwlz(1,1,1,bi,bj),
     . nswcld,cldtot_sw(1,1,1,bi,bj),clras_sw(1,1,1,bi,bj),
     . cldlsp_sw(1,1,1,bi,bj),nswlz,swlz(1,1,1,bi,bj),
     . imstturbsw,imstturblw,qliqavesw(1,1,1,bi,bj),
     . qliqavelw(1,1,1,bi,bj),fccavesw(1,1,1,bi,bj),
     . fccavelw(1,1,1,bi,bj),qq(1,1,1,bi,bj))

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

      call fizhi_step_diag(myid,p,uphy,vphy,thphy,sphy,qq,pkl,dpres,
     .  radswt,radswg,swgclr,osr,osrclr,st4,dst4,tgz,tg0,radlwg,lwgclr,
     .  turbu,turbv,turbt,turbq,moistu,moistv,moistt,moistq,
     .  lwdt,swdt,lwdtclr,swdtclr,dlwdtg,
     .  im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,bi,bj,ntracer)

      return
      end
