C $Header: /u/gcmpack/MITgcm/verification/fizhi-gridalt-hs/code/do_fizhi.F,v 1.7 2004/10/21 16:27:33 molod Exp $
C $Name:  $
#include "FIZHI_OPTIONS.h"
       subroutine do_fizhi(myid,
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
      _RL tempi(2)

      _RL kF,sigma_b,ks,ka,deg2rad,pi,atm_po,atm_kappa,termp,kv,kT
      _RL term1,term2,thetalim,thetaeq,recip_p0g

      logical alarm
      external alarm
      
C***********************************************************************

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
