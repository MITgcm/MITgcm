C $Header: /u/gcmpack/MITgcm/verification/fizhi-gridalt-hs/code/do_fizhi.F,v 1.4 2004/09/30 16:11:00 molod Exp $
C $Name:  $
#include "FIZHI_OPTIONS.h"
       subroutine do_fizhi(myid,
     . idim1,idim2,jdim1,jdim2,Nrphin,Nsxin,Nsyin,im1,im2,jm1,jm2,bi,bj,
     . nchp,nchptot,nchpland,
     . uphy,vphy,thphy,sphy,pephy,lons,lats,
     . ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     . tgz,sst,sice,phis_var,landtype,fracland,emiss,albnirdr,albnirdf,
     . albvisdr,albvisdf,ityp,chfr,alai,agrn,igrd,chlat,chlon,
     . tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,snodep,capac,
     . o3,qstr,co2,cfc11,cfc12,cfc22,n2o,methane,
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
      integer Nrphin,Nsxin,Nsyin,bi,bj,nchp
      integer nchptot(Nsxin,Nsyin),nchpland(Nsxin,Nsyin)
      _RL uphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL vphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL thphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL sphy(idim1:idim2,jdim1:jdim2,Nrphin,Nsxin,Nsyin)
      _RL pephy(idim1:idim2,jdim1:jdim2,Nrphin+1,Nsxin,Nsyin)
      _RS lons(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
      _RS lats(idim1:idim2,jdim1:jdim2,Nsxin,Nsyin)
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

      _RL xlats(sNx,sNy),xlons(sNx,sNy),sea_ice(sNx,sNy)
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
      _RL rainlsp(sNx,sNy,Nsx,Nsy),raincon(sNx,sNy,Nsx,Nsy)
      _RL snowfall(sNx,sNy,Nsx,Nsy)
      _RL cldtot_lw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL cldras_lw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL cldlsp_lw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL lwlz(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL cldtot_sw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL cldras_sw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL cldlsp_sw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL swlz(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL qliqavesw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL qliqavelw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL fccavesw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL fccavelw(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL qq(sNx,sNy,Nrphys,Nsx,Nsy)

      integer i,j,L
      _RL getcon, kappa, p0kappa, s0, ra
      _RL cosz(sNx,sNy)

      _RL tempij(sNx,sNy)
      _RL tempi(2)

      _RL kF,sigma_b,ks,ka,deg2rad,pi,atm_po,atm_kappa,termp,kv,kT
      _RL term1,term2,thetalim,thetaeq,recip_p0g

      logical alarm
      external alarm
      
c     save lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq
c     save moistu,moistv,moistt,moistq
c     save radswg,swgclr,fdirpar,fdifpar,osr,osrclr,tg0,radlwg
c     save st4,dst4,dlwdtg,rainlsp,raincon,snowfall,iras
c     save nlwcld,cldtot_lw,cldras_lw,cldlsp_lw,nlwlz,lwlz
c     save nswcld,cldtot_sw,cldras_sw,cldlsp_sw,nswlz,swlz
c     save imstturbsw,imstturblw,qliqavesw,qliqavelw,fccavesw,fccavelw
c     save qq
c     save pl,ple,dpres,pkle,pkl
       
      common /saver/ lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq
      common /saver/ moistu,moistv,moistt,moistq
      common /saver/ radswg,swgclr,fdirpar,fdifpar,osr,osrclr,tg0,radlwg
      common /saver/ st4,dst4,dlwdtg,rainlsp,raincon,snowfall
      common /saver/ cldtot_lw,cldras_lw,cldlsp_lw,lwlz
      common /saver/ cldtot_sw,cldras_sw,cldlsp_sw,swlz
      common /saver/ imstturbsw,imstturblw,qliqavesw,qliqavelw,fccavesw
      common /saver/ fccavelw
      common /saver/ qq
      common /saver/ pl,ple,dpres,pkle,pkl
      common /saver/ nlwcld,nlwlz
      common /saver/ nswcld,nswlz
      common /saver/ iras

C***********************************************************************
C Unshadow input arrays (and make 'fizhi theta' from true theta)
C***********************************************************************

Creal if( (nhms.eq.nhms0) .and. (nymd.eq.nymd0) ) then
Creal  _BEGIN_MASTER(myid)
Creal  if(myid.eq.1.and.bi.eq.1) print *,' Initializing fizhi arrays '
Creal  _END_MASTER(myid)
Creal  imstturblw = 0
Creal  imstturbsw = 0
Creal  iras = 0
Creal  nlwcld = 0
Creal  nlwlz = 0
Creal  nswcld = 0
Creal  nswlz = 0
Creal  do L = 1,Nrphys
Creal  do j = jm1,jm2
Creal  do i = im1,im2
Creal   swlz(i,j,L,bi,bj) = 0.
Creal   lwlz(i,j,L,bi,bj) = 0.
Creal   qliqavesw(i,j,L,bi,bj) = 0.
Creal   qliqavelw(i,j,L,bi,bj) = 0.
Creal   fccavesw(i,j,L,bi,bj) = 0.
Creal   fccavelw(i,j,L,bi,bj) = 0.
Creal   cldtot_sw(i,j,L,bi,bj) = 0.
Creal   cldras_sw(i,j,L,bi,bj) = 0.
Creal   cldlsp_sw(i,j,L,bi,bj) = 0.
Creal   cldtot_lw(i,j,L,bi,bj) = 0.
Creal   cldras_lw(i,j,L,bi,bj) = 0.
Creal   cldlsp_lw(i,j,L,bi,bj) = 0.
Creal   lwdt(i,j,L,bi,bj) = 0.
Creal   swdt(i,j,L,bi,bj) = 0.
Creal   turbt(i,j,L,bi,bj) = 0.
Creal   moistt(i,j,L,bi,bj) = 0.
Creal   turbq(i,j,L,1,bi,bj) = 0.
Creal   moistq(i,j,L,1,bi,bj) = 0.
Creal   turbu(i,j,L,bi,bj) = 0.
Creal   moistu(i,j,L,bi,bj) = 0.
Creal   turbv(i,j,L,bi,bj) = 0.
Creal   moistv(i,j,L,bi,bj) = 0.
Creal  enddo
Creal  enddo
Creal  enddo
Creal  do j = jm1,jm2
Creal  do i = im1,im2
Creal   rainlsp(i,j,bi,bj) = 0.
Creal   raincon(i,j,bi,bj) = 0.
Creal   snowfall(i,j,bi,bj) = 0.
Creal  enddo
Creal  enddo
Creal endif
Creal
Creal kappa = getcon('KAPPA')
Creal p0kappa = 1000.0 ** kappa
Creal S0 = getcon('S0')
Creal  
Creal do j = jm1,jm2
Creal do i = im1,im2
Creal  xlats(i,j) = lats(i,j,bi,bj)
Creal  xlons(i,j) = lons(i,j,bi,bj)
Creal enddo
Creal enddo
Creal
Creal call astro ( nymd,nhms, xlats,xlons, im2*jm2, cosz,ra )
Creal do j=jm1,jm2
Creal do i=im1,im2
Creal  radswt(i,j,bi,bj) = S0*(1.0/ra**2)*cosz(i,j)
Creal enddo
Creal enddo
Creal
Creal if( alarm('moist') .or. alarm('turb')   .or.
Creal.    alarm('radsw') .or. alarm('radlw') ) then
Creal
Crealpute pressures - all pressure are converted here to hPa
Creal do j = jm1,jm2
Creal do i = im1,im2
Creal  ple(i,j,Nrphys+1,bi,bj) = pephy(i,j,Nrphys+1,bi,bj)/100.
Creal  pkle(i,j,Nrphys+1,bi,bj)=(pephy(i,j,Nrphys+1,bi,bj)/100.) **kappa
Creal  p(i,j,bi,bj) = pephy(i,j,Nrphys+1,bi,bj)/100.
Creal  sea_ice(i,j) = sice(i,j,bi,bj)
Creal enddo
Creal enddo
Creal do L = 1,Nrphys
Creal do j = jm1,jm2
Creal do i = im1,im2
Creal  u(i,j,L) = uphy(i,j,L,bi,bj)
Creal  v(i,j,L) = vphy(i,j,L,bi,bj)
Creal  t(i,j,L) = thphy(i,j,L,bi,bj)/p0kappa
Creal  q(i,j,L,1) = sphy(i,j,L,bi,bj)
Creal  pl(i,j,L,bi,bj) = (pephy(i,j,L,bi,bj)+pephy(i,j,L+1,bi,bj))/200.
Creal  dpres(i,j,L,bi,bj)=(pephy(i,j,L+1,bi,bj)-pephy(i,j,L,bi,bj))/100.
Creal  ple(i,j,L,bi,bj) = pephy(i,j,L,bi,bj)/100.
Creal  if (ple(i,j,L,bi,bj).gt.0.) then
Creal   pkle(i,j,L,bi,bj) = ple(i,j,L,bi,bj) **kappa
Creal  else
Creal   pkle(i,j,L,bi,bj) = 0.
Creal  endif
Creal enddo
Creal enddo
Creal enddo
Creal
Creal call pkappa (im2,jm2,Nrphys,ple(1,1,1,bi,bj),pkle(1,1,1,bi,bj),
Creal.                                                 pkl(1,1,1,bi,bj))
Creal
Creal call fizhi_driver(myid,im2,jm2,Nrphys,bi,bj,ptracer,ntracer,xlats,
Creal. xlons,p(1,1,bi,bj),u,v,t,q,pl(1,1,1,bi,bj),ple(1,1,1,bi,bj),
Creal. dpres(1,1,1,bi,bj),pkle(1,1,1,bi,bj),pkl(1,1,1,bi,bj),
Creal. fracland(1,1,bi,bj),landtype(1,1,bi,bj),radswt(1,1,bi,bj),
Creal. phis_var(1,1,bi,bj),tgz(1,1,bi,bj),sea_ice,nchp,chlat(1,bi,bj),
Creal. chlon(1,bi,bj),igrd(1,bi,bj),nchptot(bi,bj),nchpland(bi,bj),
Creal. chfr(1,bi,bj),ityp(1,bi,bj),tcanopy(1,bi,bj),tdeep(1,bi,bj),
Creal. ecanopy(1,bi,bj),swetshal(1,bi,bj),swetroot(1,bi,bj),
Creal. swetdeep(1,bi,bj),capac(1,bi,bj),snodep(1,bi,bj),
Creal. ctmt(1,bi,bj),xxmt(1,bi,bj),yymt(1,bi,bj),zetamt(1,bi,bj),
Creal. xlmt(1,1,bi,bj),khmt(1,1,bi,bj),tke(1,1,bi,bj),
Creal. albvisdr(1,1,bi,bj),albvisdf(1,1,bi,bj),albnirdr(1,1,bi,bj),
Creal. albnirdf(1,1,bi,bj),emiss(1,1,1,bi,bj),alai(1,bi,bj),
Creal. agrn(1,bi,bj),
Creal. qstr(1,1,1,bi,bj),o3(1,1,1,bi,bj),
Creal. co2,cfc11,cfc12,cfc22,methane,n2o,
Creal. lwdt(1,1,1,bi,bj),lwdtclr(1,1,1,bi,bj),swdt(1,1,1,bi,bj),
Creal. swdtclr(1,1,1,bi,bj),turbu(1,1,1,bi,bj),turbv(1,1,1,bi,bj),
Creal. turbt(1,1,1,bi,bj),turbq(1,1,1,1,bi,bj),moistu(1,1,1,bi,bj),
Creal. moistv(1,1,1,bi,bj),moistt(1,1,1,bi,bj),moistq(1,1,1,1,bi,bj),
Creal. radswg(1,1,bi,bj),swgclr(1,1,bi,bj),fdirpar(1,1,bi,bj),
Creal. fdifpar(1,1,bi,bj),osr(1,1,bi,bj),osrclr(1,1,bi,bj),
Creal. tg0(1,1,bi,bj),radlwg(1,1,bi,bj),lwgclr(1,1,bi,bj),
Creal. st4(1,1,bi,bj),dst4(1,1,bi,bj),dlwdtg(1,1,1,bi,bj),
Creal. rainlsp(1,1,bi,bj),raincon(1,1,bi,bj),snowfall(1,1,bi,bj),iras,
Creal. nlwcld,cldtot_lw(1,1,1,bi,bj),cldras_lw(1,1,1,bi,bj),
Creal. cldlsp_lw(1,1,1,bi,bj),nlwlz,lwlz(1,1,1,bi,bj),
Creal. nswcld,cldtot_sw(1,1,1,bi,bj),cldras_sw(1,1,1,bi,bj),
Creal. cldlsp_sw(1,1,1,bi,bj),nswlz,swlz(1,1,1,bi,bj),
Creal. imstturbsw,imstturblw,qliqavesw(1,1,1,bi,bj),
Creal. qliqavelw(1,1,1,bi,bj),fccavesw(1,1,1,bi,bj),
Creal. fccavelw(1,1,1,bi,bj),qq(1,1,1,bi,bj))
Creal
Creal do L = 1,Nrphys
Creal do j = jm1,jm2
Creal do i = im1,im2
Creal  duphy(i,j,L,bi,bj) = moistu(i,j,L,bi,bj) + turbu(i,j,L,bi,bj)
Creal  dvphy(i,j,L,bi,bj) = moistv(i,j,L,bi,bj) + turbv(i,j,L,bi,bj)
Creal  dthphy(i,j,L,bi,bj) = ((moistt(i,j,L,bi,bj)+turbt(i,j,L,bi,bj)+
Creal.   lwdt(i,j,L,bi,bj) + 
Creal.   dlwdtg(i,j,L,bi,bj) * (tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) +
Creal.   swdt(i,j,L,bi,bj)*radswt(i,j,bi,bj) )*p0kappa ) / p(i,j,bi,bj)
Creal  dsphy(i,j,L,bi,bj) = (moistq(i,j,L,1,bi,bj)+turbq(i,j,L,1,bi,bj))
Creal.                                    /p(i,j,bi,bj)
Creal enddo
Creal enddo
Creal enddo
Creal
Creal endif
Creal
Creal call fizhi_step_diag(myid,p,uphy,vphy,thphy,sphy,qq,pkl,dpres,
Creal.  radswt,radswg,swgclr,osr,osrclr,st4,dst4,tgz,tg0,radlwg,lwgclr,
Creal.  turbu,turbv,turbt,turbq,moistu,moistv,moistt,moistq,
Creal.  lwdt,swdt,lwdtclr,swdtclr,dlwdtg,
Creal.  im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,bi,bj,ntracer)
Creal

       kF=1. _d 0/86400. _d 0
       sigma_b = 0.7 _d 0
       ka=1. _d 0/(40. _d 0*86400. _d 0)
       ks=1. _d 0/(4. _d 0 *86400. _d 0)
       pi = getcon('PI')
       atm_kappa = getcon('KAPPA')
       atm_po = getcon('ATMPOPA')
       deg2rad = getcon('DEG2RAD')

       do L = 1,Nrphys
        do j = jm1,jm2
        do i = im1,im2
         recip_P0g= 1. _d 0 / pephy(i,j,Nrphys+1,bi,bj)
c U  and V terms:
         termP=0.5 _d 0*((pephy(i,j,L,bi,bj)+pephy(i,j,L+1,bi,bj))
     &                   *recip_P0g )
         kV=kF*MAX( 0. _d 0, (termP-sigma_b)/(1. _d 0-sigma_b) )
         duphy(i,j,L,bi,bj)= -kV*uphy(i,j,L,bi,bj)
         dvphy(i,j,L,bi,bj)= -kV*vphy(i,j,L,bi,bj)
       
c T terms
C--   Forcing term(s)
         term1=60. _d 0*(sin(lats(I,J,bi,bj)*deg2rad)**2)
         termP=0.5 _d 0*( pephy(i,j,L,bi,bj) + pephy(i,j,L+1,bi,bj) )
         term2=10. _d 0*log(termP/atm_po)
     &            *(cos(lats(I,J,bi,bj)*deg2rad)**2)
         thetaLim = 200. _d 0/ ((termP/atm_po)**atm_kappa)
         thetaEq=315. _d 0-term1-term2
         thetaEq=MAX(thetaLim,thetaEq)
         kT=ka+(ks-ka)
     &     *MAX(0. _d 0,
     &       (termP*recip_P0g-sigma_b)/(1. _d 0-sigma_b) )
     &     *COS((lats(I,J,bi,bj)*deg2rad))**4
         if(termP*recip_P0g.gt.0.04)then
          dthphy(i,j,L,bi,bj)=- kT*( thphy(I,J,L,bi,bj)-thetaEq )
         else 
          dthphy(i,j,L,bi,bj)=0.
         endif

c S terms (hs runs dry - no moisture)
C--   Forcing term(s)
         dsphy(i,j,L,bi,bj)=0.
      
        enddo
        enddo
       enddo

      return
      end
