C $Header: /u/gcmpack/MITgcm/pkg/fizhi/do_fizhi.F,v 1.10 2004/06/16 19:19:49 molod Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"
       subroutine do_fizhi(myid,uphy,vphy,thphy,sphy,pephy,lons,lats,
     .   ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     .   tgz,sst,sice,phis_var,landtype,fracland,emiss,albnidr,albnirdf,
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

C Argument list declarations
      integer myid,im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
      integer Nrphys,Nsx,Nsy,bi,bj,nchp,nchpland,
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
      _RL tgz(im2,jm2)
      _RL sst(idim1:idim2,jdim1:jdim2,Nsx,Nsy)
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
      _RL snodep(nchp,Nsx,Nsy),capac(nchp,Nsx,Nsy),
      _RL o3(im2,jm2,Nsx,Nsy),qstr(im2,jm2,Nsx,Nsy)
      _RL co2,cfc11,cfc12,cfc22,n2o(Nrphys),methane(Nrphys)
      _RL duphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dvphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dthphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dsphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
c
      integer ptracer,ntracer
      integer iras,nlwcld,nlwlz,nswcld,nswlz
      integer imstturbsw,imstturblw

      real xlats(im2,jm2), xlons(im2,jm2), sea_ice(im2,jm2), p(im2,jm2)
      real u(im2,jm2,Nrphys), v(im2,jm2,Nrphys), t(im2,jm2,Nrphys)
      real q(im2,jm2,Nrphys,ntracer)
      real pl(im2,jm2,Nrphys),ple(im2,jm2,Nrphys+1)
      real dpres(im2,jm2,Nrphys),pkht(im2,jm2,Nrphys)
      real lwdt(im2,jm2,Nrphys),lwdtclr(im2,jm2,Nrphys) 
      real swdt(im2,jm2,Nrphys),swdtclr(im2,jm2,Nrphys)
      real turbu(im2,jm2,Nrphys),turbv(im2,jm2,Nrphys)
      real turbt(im2,jm2,Nrphys),turbq(im2,jm2,Nrphys)
      real moistu(im2,jm2,Nrphys),moistv(im2,jm2,Nrphys)
      real moistt(im2,jm2,Nrphys),moistq(im2,jm2,Nrphys)
      real radswt(im2,jm2),radswg(im2,jm2),swgclr(im2,jm2)
      real albedo(im2,jm2),fdirpar(im2,jm2),fdifpar(im2,jm2)
      real osr(im2,jm2),osrclr(im2,jm2)
      real tg0(im2,jm2),tg0c(im2,jm2),radlwg(im2,jm2),st4(im2,jm2)
      real dst4(im2,jm2),dlwdtg(im2,jm2),lwclr(im2,jm2)
      real rainlsp(im2,jm2),raincon(im2,jm2),snowfall(im2,jm2)
      real cldtot_lw(im2,jm2,Nrphys),clras_lw(im2,jm2,Nrphys)
      real cldlsp_lw(im2,jm2,Nrphys),lwlz(im2,jm2,Nrphys)
      real cldtot_sw(im2,jm2,Nrphys),clras_sw(im2,jm2,Nrphys)
      real cldlsp_sw(im2,jm2,Nrphys),swlz(im2,jm2,Nrphys)
      real qliqavesw(im2,jm2,Nrphys),qliqavelw(im2,jm2,Nrphys)
      real fccavesw(im2,jm2,Nrphys),fccavelw(im2,jm2,Nrphys)
      real qq(im2,jm2,Nrphys)
      real u2m(im2,jm2),v2m(im2,jm2),t2m(im2,jm2),q2m(im2,jm2)
      real u10m(im2,jm2),v10m(im2,jm2),t10m(im2,jm2),q10m(im2,jm2)
      real gwet(im2,jm2),snow(im2,jm2)

      integer i,j,L

      call fizhi_driver(myid,im2,jm2,Nrphys,ptracer,ntracer,xlats,xlons,
     . p,u,v,t,q,pl,ple,dpres,pkht,pkl,fracland,landtype,radswt,
     . phis_var,tgz,sea_ice,
     . nchp,chlat,chlon,igrd,nchpland,chfr,ityp,
     . tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,capac,snodep,
     . ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     . albvisdr,albvisdf,albnirdr,albnirdf,emiss,alai,agrn,
     . qstr,o3,co2,cfc11,cfc12,cfc22,methane,n2o,
     . lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq,
     . moistu,moistv,moistt,moistq,
     . radswg,swgclr,albedo,fdirpar,fdifpar,osr,osrclr,tg0,tg0c,radlwg,
     . st4,dst4,dlwdtg,lwclr,rainlsp,raincon,snowfall,iras,nlwcld,
     . cldtot_lw,clras_lw,cldlsp_lw,nlwlz,lwlz,nswcld,cldtot_sw,
     . clras_sw,cldlsp_sw,nswlz,swlz,imstturbsw,imstturblw,qliqavesw,
     . qliqavelw,fccavesw,fccavelw,qq,u2m,v2m,t2m,q2m,u10m,v10m,t10m,
     . q10m,gwet,snow)

      return
      end
