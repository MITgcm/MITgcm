C $Header: /u/gcmpack/MITgcm/pkg/fizhi/do_fizhi.F,v 1.21 2004/07/18 23:17:00 molod Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"
       subroutine do_fizhi(myid,uphy,vphy,thphy,sphy,pephy,lons,lats,
     .   ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     .   tgz,sice,phis_var,landtype,fracland,emiss,albnirdr,albnirdf,
     .   albvisdr,albvisdf,ityp,chfr,alai,agrn,igrd,chlat,chlon,
     .   tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,snodep,capac,
     .   o3,qstr,co2,cfc11,cfc12,cfc22,n2o,methane,
     .   idim1,idim2,jdim1,jdim2,Nrphys,Nsx,Nsy,im1,im2,jm1,jm2,bi,bj,
     .   nchp,nchpland,
     .   duphy,dvphy,dthphy,dsphy)
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
#include "chronos.h"

C Argument list declarations
      integer myid,im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
      integer Nrphys,Nsx,Nsy,bi,bj,nchp,nchpland
      _RL uphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL vphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL thphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL sphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL pephy(idim1:idim2,jdim1:jdim2,Nrphys+1,Nsx,Nsy)
      _RL lons(idim1:idim2,jdim1:jdim2,Nsx,Nsy)
      _RL lats(idim1:idim2,jdim1:jdim2,Nsx,Nsy)
      _RL ctmt(nchp,Nsx,Nsy),xxmt(nchp,Nsx,Nsy),yymt(nchp,Nsx,Nsy)
      _RL zetamt(nchp,Nsx,Nsy)
      _RL xlmt(nchp,Nrphys,Nsx,Nsy),khmt(nchp,Nrphys,Nsx,Nsy)
      _RL tke(nchp,Nrphys,Nsx,Nsy)
      _RL tgz(im2,jm2,Nsx,Nsy)
      _RL sice(idim1:idim2,jdim1:jdim2,Nsx,Nsy)
      _RL phis_var(im2,jm2,Nsx,Nsy),landtype(im2,jm2,Nsx,Nsy)
      _RL fracland(im2,jm2,Nsx,Nsy),emiss(im2,jm2,10,Nsx,Nsy)
      _RL albvisdr(im2,jm2,Nsx,Nsy),albvisdf(im2,jm2,Nsx,Nsy)
      _RL albnirdr(im2,jm2,Nsx,Nsy),albnirdf(im2,jm2,Nsx,Nsy)
      _RL chfr(nchp,Nsx,Nsy),alai(nchp,Nsx,Nsy),agrn(nchp,Nsx,Nsy)
      integer ityp(nchp,Nsx,Nsy),igrd(nchp,Nsx,Nsy)
      _RL chlat(nchp,Nsx,Nsy),chlon(nchp,Nsx,Nsy)
      _RL tcanopy(nchp,Nsx,Nsy),tdeep(nchp,Nsx,Nsy)
      _RL ecanopy(nchp,Nsx,Nsy),swetshal(nchp,Nsx,Nsy)
      _RL swetroot(nchp,Nsx,Nsy),swetdeep(nchp,Nsx,Nsy)
      _RL snodep(nchp,Nsx,Nsy),capac(nchp,Nsx,Nsy)
      _RL o3(im2,jm2,Nsx,Nsy),qstr(im2,jm2,Nsx,Nsy)
      _RL co2,cfc11,cfc12,cfc22,n2o(Nrphys),methane(Nrphys)
      _RL duphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dvphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dthphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dsphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
c
      integer ptracer,ntracer
      parameter (ptracer = 1)
      parameter (ntracer = 1)
      integer iras,nlwcld,nlwlz,nswcld,nswlz
      integer imstturbsw,imstturblw

      real xlats(im2,jm2), xlons(im2,jm2), sea_ice(im2,jm2) 
      real p(im2,jm2,Nsx,Nsy)
      real u(im2,jm2,Nrphys), v(im2,jm2,Nrphys), t(im2,jm2,Nrphys)
      real q(im2,jm2,Nrphys,ntracer)
      real pl(im2,jm2,Nrphys,Nsx,Nsy),pkl(im2,jm2,Nrphys,Nsx,Nsy)
      real ple(im2,jm2,Nrphys+1,Nsx,Nsy),pkle(im2,jm2,Nrphys+1,Nsx,Nsy)
      real dpres(im2,jm2,Nrphys,Nsx,Nsy)
      real lwdt(im2,jm2,Nrphys,Nsx,Nsy),lwdtclr(im2,jm2,Nrphys,Nsx,Nsy) 
      real swdt(im2,jm2,Nrphys,Nsx,Nsy),swdtclr(im2,jm2,Nrphys,Nsx,Nsy)
      real turbu(im2,jm2,Nrphys,Nsx,Nsy),turbv(im2,jm2,Nrphys,Nsx,Nsy)
      real turbt(im2,jm2,Nrphys,Nsx,Nsy)
      real turbq(im2,jm2,Nrphys,ntracer,Nsx,Nsy)
      real moistu(im2,jm2,Nrphys,Nsx,Nsy),moistv(im2,jm2,Nrphys,Nsx,Nsy)
      real moistt(im2,jm2,Nrphys,Nsx,Nsy)
      real moistq(im2,jm2,Nrphys,ntracer,Nsx,Nsy)
      real radswt(im2,jm2,Nsx,Nsy),radswg(im2,jm2,Nsx,Nsy)
      real swgclr(im2,jm2,Nsx,Nsy)
      real fdirpar(im2,jm2,Nsx,Nsy),fdifpar(im2,jm2,Nsx,Nsy)
      real osr(im2,jm2,Nsx,Nsy),osrclr(im2,jm2,Nsx,Nsy)
      real tg0(im2,jm2,Nsx,Nsy),radlwg(im2,jm2,Nsx,Nsy)
      real lwgclr(im2,jm2,Nsx,Nsy),st4(im2,jm2,Nsx,Nsy)
      real dst4(im2,jm2,Nsx,Nsy),dlwdtg(im2,jm2,Nrphys,Nsx,Nsy)
      real rainlsp(im2,jm2,Nsx,Nsy),raincon(im2,jm2,Nsx,Nsy)
      real snowfall(im2,jm2,Nsx,Nsy)
      real cldtot_lw(im2,jm2,Nrphys,Nsx,Nsy)
      real clras_lw(im2,jm2,Nrphys,Nsx,Nsy)
      real cldlsp_lw(im2,jm2,Nrphys,Nsx,Nsy)
      real lwlz(im2,jm2,Nrphys,Nsx,Nsy)
      real cldtot_sw(im2,jm2,Nrphys,Nsx,Nsy)
      real clras_sw(im2,jm2,Nrphys,Nsx,Nsy)
      real cldlsp_sw(im2,jm2,Nrphys,Nsx,Nsy)
      real swlz(im2,jm2,Nrphys,Nsx,Nsy)
      real qliqavesw(im2,jm2,Nrphys,Nsx,Nsy)
      real qliqavelw(im2,jm2,Nrphys,Nsx,Nsy)
      real fccavesw(im2,jm2,Nrphys,Nsx,Nsy)
      real fccavelw(im2,jm2,Nrphys,Nsx,Nsy)
      real qq(im2,jm2,Nrphys,Nsx,Nsy)

      integer i,j,L
      real getcon, kappa, p0kappa, s0, ra
      real cosz(im2,jm2)

      logical alarm
      external alarm

      save

c     save lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq
c     save moistu,moistv,moistt,moistq
c     save radswg,swgclr,fdirpar,fdifpar,osr,osrclr,tg0,radlwg
c     save st4,dst4,dlwdtg,rainlsp,raincon,snowfall,iras
c     save nlwcld,cldtot_lw,clras_lw,cldlsp_lw,nlwlz,lwlz
c     save nswcld,cldtot_sw,clras_sw,cldlsp_sw,nswlz,swlz,
c     save imstturbsw,imstturblw,qliqavesw,qliqavelw,fccavesw,fccavelw
c     save qq
c     save pl,ple,dpres,pkle,pkl

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
