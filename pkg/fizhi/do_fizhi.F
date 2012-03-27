C $Header: /u/gcmpack/MITgcm/pkg/fizhi/do_fizhi.F,v 1.55 2012/03/27 15:48:27 jmc Exp $
C $Name:  $
#include "FIZHI_OPTIONS.h"
       SUBROUTINE DO_FIZHI(myIter,myid,
     & idim1,idim2,jdim1,jdim2,Nrphin,nSxIn,nSyIn,im1,im2,jm1,jm2,bi,bj,
     & turbStart, nchp,nchptot,nchpland,
     & uphy,vphy,thphy,sphy,pephy,lons,lats,Zsurf,
     & ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     & tgz,sst,sice,phis_var,landtype,fracland,emiss,albnirdr,albnirdf,
     & albvisdr,albvisdf,ityp,chfr,alai,agrn,igrd,chlat,chlon,
     & tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,snodep,capac,
     & o3,qstr,co2,cfc11,cfc12,cfc22,n2o,methane,
     & iras,nlwcld,cldtotlwin,cldraslwin,cldlsplwin,nlwlz,lwlzin,
     & nswcld,cldtotswin,cldrasswin,cldlspswin,nswlz,swlzin,imstturbsw,
     & imstturblw,qliqaveswin,qliqavelwin,fccaveswin,fccavelwin,
     & rainconin,rainlspin,snowfallin,
     & duphy,dvphy,dthphy,dsphy)
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
      IMPLICIT NONE
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "chronos.h"

C Argument list declarations
      INTEGER myIter,myid,im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
      INTEGER Nrphin,nSxIn,nSyIn,bi,bj,nchp
      LOGICAL turbStart
      INTEGER nchptot(nSxIn,nSyIn),nchpland(nSxIn,nSyIn)
      _RL uphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL vphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL thphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL sphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL pephy(idim1:idim2,jdim1:jdim2,Nrphin+1,nSxIn,nSyIn)
      _RS lons(idim1:idim2,jdim1:jdim2,nSxIn,nSyIn)
      _RS lats(idim1:idim2,jdim1:jdim2,nSxIn,nSyIn)
      _RS Zsurf(idim1:idim2,jdim1:jdim2,nSxIn,nSyIn)
      _RL ctmt(nchp,nSxIn,nSyIn),xxmt(nchp,nSxIn,nSyIn)
      _RL yymt(nchp,nSxIn,nSyIn)
      _RL zetamt(nchp,nSxIn,nSyIn)
      _RL xlmt(nchp,Nrphin,nSxIn,nSyIn),khmt(nchp,Nrphin,nSxIn,nSyIn)
      _RL tke(nchp,Nrphin,nSxIn,nSyIn)
      _RL tgz(im2,jm2,nSxIn,nSyIn)
      _RL sst(idim1:idim2,jdim1:jdim2,nSxIn,nSyIn)
      _RL sice(idim1:idim2,jdim1:jdim2,nSxIn,nSyIn)
      _RL phis_var(im2,jm2,nSxIn,nSyIn)
      INTEGER landtype(im2,jm2,nSxIn,nSyIn)
      _RL fracland(im2,jm2,nSxIn,nSyIn),emiss(im2,jm2,10,nSxIn,nSyIn)
      _RL albvisdr(im2,jm2,nSxIn,nSyIn),albvisdf(im2,jm2,nSxIn,nSyIn)
      _RL albnirdr(im2,jm2,nSxIn,nSyIn),albnirdf(im2,jm2,nSxIn,nSyIn)
      _RL chfr(nchp,nSxIn,nSyIn),alai(nchp,nSxIn,nSyIn)
      _RL agrn(nchp,nSxIn,nSyIn)
      INTEGER ityp(nchp,nSxIn,nSyIn),igrd(nchp,nSxIn,nSyIn)
      _RL chlat(nchp,nSxIn,nSyIn),chlon(nchp,nSxIn,nSyIn)
      _RL tcanopy(nchp,nSxIn,nSyIn),tdeep(nchp,nSxIn,nSyIn)
      _RL ecanopy(nchp,nSxIn,nSyIn),swetshal(nchp,nSxIn,nSyIn)
      _RL swetroot(nchp,nSxIn,nSyIn),swetdeep(nchp,nSxIn,nSyIn)
      _RL snodep(nchp,nSxIn,nSyIn),capac(nchp,nSxIn,nSyIn)
      _RL o3(im2,jm2,Nrphin,nSxIn,nSyIn)
      _RL qstr(im2,jm2,Nrphin,nSxIn,nSyIn)
      _RL co2,cfc11,cfc12,cfc22,n2o(Nrphin),methane(Nrphin)

      INTEGER iras(nSxIn,nSyIn)
      INTEGER nlwcld(nSxIn,nSyIn),nlwlz(nSxIn,nSyIn)
      INTEGER nswcld(nSxIn,nSyIn),nswlz(nSxIn,nSyIn)
      INTEGER imstturbsw(nSxIn,nSyIn),imstturblw(nSxIn,nSyIn)
      _RL cldtotlwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL cldraslwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL cldlsplwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL lwlzin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL cldtotswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL cldrasswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL cldlspswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL swlzin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL qliqaveswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL qliqavelwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL fccaveswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL fccavelwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL rainlspin(idim1:idim2,jdim1:jdim2,nSxIn,nSyIn)
      _RL rainconin(idim1:idim2,jdim1:jdim2,nSxIn,nSyIn)
      _RL snowfallin(idim1:idim2,jdim1:jdim2,nSxIn,nSyIn)


      _RL duphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL dvphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL dthphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)
      _RL dsphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxIn,nSyIn)

c Local Variables
      INTEGER ptracer,ntracer
      PARAMETER (ptracer = 1)
      PARAMETER (ntracer = 1)

      _RL xlats(sNx,sNy),xlons(sNx,sNy),sea_ice(sNx,sNy)
      _RL surfZ(sNx,sNy)
      _RL p(sNx,sNy,nSx,nSy)
      _RL u(sNx,sNy,Nrphys),v(sNx,sNy,Nrphys),t(sNx,sNy,Nrphys)
      _RL q(sNx,sNy,Nrphys,ntracer)
      _RL pl(sNx,sNy,Nrphys,nSx,nSy),pkl(sNx,sNy,Nrphys,nSx,nSy)
      _RL ple(sNx,sNy,Nrphys+1,nSx,nSy)
      _RL pkle(sNx,sNy,Nrphys+1,nSx,nSy)
      _RL dpres(sNx,sNy,Nrphys,nSx,nSy)
      _RL lwdt(sNx,sNy,Nrphys,nSx,nSy)
      _RL lwdtclr(sNx,sNy,Nrphys,nSx,nSy)
      _RL swdt(sNx,sNy,Nrphys,nSx,nSy)
      _RL swdtclr(sNx,sNy,Nrphys,nSx,nSy)
      _RL turbu(sNx,sNy,Nrphys,nSx,nSy)
      _RL turbv(sNx,sNy,Nrphys,nSx,nSy)
      _RL turbt(sNx,sNy,Nrphys,nSx,nSy)
      _RL turbq(sNx,sNy,Nrphys,ntracer,nSx,nSy)
      _RL moistu(sNx,sNy,Nrphys,nSx,nSy)
      _RL moistv(sNx,sNy,Nrphys,nSx,nSy)
      _RL moistt(sNx,sNy,Nrphys,nSx,nSy)
      _RL moistq(sNx,sNy,Nrphys,ntracer,nSx,nSy)
      _RL radswt(sNx,sNy,nSx,nSy),radswg(sNx,sNy,nSx,nSy)
      _RL swgclr(sNx,sNy,nSx,nSy)
      _RL fdirpar(sNx,sNy,nSx,nSy),fdifpar(sNx,sNy,nSx,nSy)
      _RL osr(sNx,sNy,nSx,nSy),osrclr(sNx,sNy,nSx,nSy)
      _RL tg0(sNx,sNy,nSx,nSy),radlwg(sNx,sNy,nSx,nSy)
      _RL lwgclr(sNx,sNy,nSx,nSy),st4(sNx,sNy,nSx,nSy)
      _RL dst4(sNx,sNy,nSx,nSy),dlwdtg(sNx,sNy,Nrphys,nSx,nSy)
      _RL qq(sNx,sNy,Nrphys,nSx,nSy)
      INTEGER i,j,L
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

      LOGICAL alarm
      EXTERNAL alarm

      COMMON /saver/ lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq
      COMMON /saver/ moistu,moistv,moistt,moistq
      COMMON /saver/ radswg,swgclr,fdirpar,fdifpar,osr,osrclr,tg0,radlwg
      COMMON /saver/ st4,dst4,dlwdtg,lwgclr
      COMMON /saver/ qq
      COMMON /saver/ p,pl,ple,dpres,pkle,pkl

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

#ifdef FIZHI_USE_FIXED_DAY
      call astro ( 20040321,nhms, xlats,xlons, im2*jm2, cosz,ra )
#else
      call astro ( nymd,nhms, xlats,xlons, im2*jm2, cosz,ra )
#endif
      do j=jm1,jm2
      do i=im1,im2
       radswt(i,j,bi,bj) = S0*(1.0/ra**2)*cosz(i,j)
      enddo
      enddo

      if( alarm('moist') .or. alarm('turb')   .or.
     &    alarm('radsw') .or. alarm('radlw') ) then

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
     &                                                 pkl(1,1,1,bi,bj))

      call fizhi_driver(myid,im2,jm2,Nrphys,bi,bj,ptracer,ntracer,
     & turbStart, xlats, xlons,
     & p(1,1,bi,bj),u,v,t,q,pl(1,1,1,bi,bj),ple(1,1,1,bi,bj),
     & dpres(1,1,1,bi,bj),pkle(1,1,1,bi,bj),pkl(1,1,1,bi,bj),surfZ,
     & fracland(1,1,bi,bj),landtype(1,1,bi,bj),radswt(1,1,bi,bj),
     & phis_var(1,1,bi,bj),tgz(1,1,bi,bj),sea_ice,nchp,chlat(1,bi,bj),
     & chlon(1,bi,bj),igrd(1,bi,bj),nchptot(bi,bj),nchpland(bi,bj),
     & chfr(1,bi,bj),ityp(1,bi,bj),tcanopy(1,bi,bj),tdeep(1,bi,bj),
     & ecanopy(1,bi,bj),swetshal(1,bi,bj),swetroot(1,bi,bj),
     & swetdeep(1,bi,bj),capac(1,bi,bj),snodep(1,bi,bj),
     & ctmt(1,bi,bj),xxmt(1,bi,bj),yymt(1,bi,bj),zetamt(1,bi,bj),
     & xlmt(1,1,bi,bj),khmt(1,1,bi,bj),tke(1,1,bi,bj),
     & albvisdr(1,1,bi,bj),albvisdf(1,1,bi,bj),albnirdr(1,1,bi,bj),
     & albnirdf(1,1,bi,bj),emiss(1,1,1,bi,bj),alai(1,bi,bj),
     & agrn(1,bi,bj),
     & qstr(1,1,1,bi,bj),o3(1,1,1,bi,bj),
     & co2,cfc11,cfc12,cfc22,methane,n2o,
     & lwdt(1,1,1,bi,bj),lwdtclr(1,1,1,bi,bj),swdt(1,1,1,bi,bj),
     & swdtclr(1,1,1,bi,bj),turbu(1,1,1,bi,bj),turbv(1,1,1,bi,bj),
     & turbt(1,1,1,bi,bj),turbq(1,1,1,1,bi,bj),moistu(1,1,1,bi,bj),
     & moistv(1,1,1,bi,bj),moistt(1,1,1,bi,bj),moistq(1,1,1,1,bi,bj),
     & radswg(1,1,bi,bj),swgclr(1,1,bi,bj),fdirpar(1,1,bi,bj),
     & fdifpar(1,1,bi,bj),osr(1,1,bi,bj),osrclr(1,1,bi,bj),
     & tg0(1,1,bi,bj),radlwg(1,1,bi,bj),lwgclr(1,1,bi,bj),
     & st4(1,1,bi,bj),dst4(1,1,bi,bj),dlwdtg(1,1,1,bi,bj),
     & rainlsp,raincon,snowfall,iras(bi,bj),
     & nlwcld(bi,bj),cldtot_lw,cldras_lw,cldlsp_lw,nlwlz(bi,bj),lwlz,
     & nswcld(bi,bj),cldtot_sw,cldras_sw,cldlsp_sw,nswlz(bi,bj),swlz,
     & imstturbsw(bi,bj),imstturblw(bi,bj),qliqavesw,qliqavelw,
     & fccavesw,fccavelw,qq(1,1,1,bi,bj))

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
     &   lwdt(i,j,L,bi,bj) +
     &   dlwdtg(i,j,L,bi,bj) * (tgz(i,j,bi,bj)-tg0(i,j,bi,bj)) +
     &   swdt(i,j,L,bi,bj)*radswt(i,j,bi,bj) )*p0kappa ) / p(i,j,bi,bj)
       dsphy(i,j,L,bi,bj) = (moistq(i,j,L,1,bi,bj)+turbq(i,j,L,1,bi,bj))
     &                                    /p(i,j,bi,bj)
      enddo
      enddo
      enddo

      call fizhi_step_diag(myid,p,u,v,t,q,qq,pkl,dpres,
     &  radswt,radswg,swgclr,osr,osrclr,st4,dst4,tgz,tg0,radlwg,lwgclr,
     &  turbu,turbv,turbt,turbq,moistu,moistv,moistt,moistq,
     &  lwdt,swdt,lwdtclr,swdtclr,dlwdtg,
     &  im1,im2,jm1,jm2,Nrphys,nSx,nSy,bi,bj,ntracer)

      return
      end
