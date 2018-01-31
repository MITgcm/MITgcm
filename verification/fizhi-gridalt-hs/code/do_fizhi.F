#include "FIZHI_OPTIONS.h"
       SUBROUTINE DO_FIZHI(myIter,myid,
     & idim1,idim2,jdim1,jdim2,Nrphin,nSxin,nSyin,im1,im2,jm1,jm2,bi,bj,
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
      INTEGER Nrphin,nSxin,nSyin,bi,bj,nchp
      LOGICAL turbStart
      INTEGER nchptot(nSxin,nSyin),nchpland(nSxin,nSyin)
      _RL uphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL vphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL thphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL sphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL pephy(idim1:idim2,jdim1:jdim2,Nrphin+1,nSxin,nSyin)
      _RS lons(idim1:idim2,jdim1:jdim2,nSxin,nSyin)
      _RS lats(idim1:idim2,jdim1:jdim2,nSxin,nSyin)
      _RS Zsurf(idim1:idim2,jdim1:jdim2,nSxin,nSyin)
      _RL ctmt(nchp,nSxin,nSyin),xxmt(nchp,nSxin,nSyin)
      _RL yymt(nchp,nSxin,nSyin)
      _RL zetamt(nchp,nSxin,nSyin)
      _RL xlmt(nchp,Nrphin,nSxin,nSyin),khmt(nchp,Nrphin,nSxin,nSyin)
      _RL tke(nchp,Nrphin,nSxin,nSyin)
      _RL tgz(im2,jm2,nSxin,nSyin)
      _RL sst(idim1:idim2,jdim1:jdim2,nSxin,nSyin)
      _RL sice(idim1:idim2,jdim1:jdim2,nSxin,nSyin)
      _RL phis_var(im2,jm2,nSxin,nSyin)
      INTEGER landtype(im2,jm2,nSxin,nSyin)
      _RL fracland(im2,jm2,nSxin,nSyin),emiss(im2,jm2,10,nSxin,nSyin)
      _RL albvisdr(im2,jm2,nSxin,nSyin),albvisdf(im2,jm2,nSxin,nSyin)
      _RL albnirdr(im2,jm2,nSxin,nSyin),albnirdf(im2,jm2,nSxin,nSyin)
      _RL chfr(nchp,nSxin,nSyin),alai(nchp,nSxin,nSyin)
      _RL agrn(nchp,nSxin,nSyin)
      INTEGER ityp(nchp,nSxin,nSyin),igrd(nchp,nSxin,nSyin)
      _RL chlat(nchp,nSxin,nSyin),chlon(nchp,nSxin,nSyin)
      _RL tcanopy(nchp,nSxin,nSyin),tdeep(nchp,nSxin,nSyin)
      _RL ecanopy(nchp,nSxin,nSyin),swetshal(nchp,nSxin,nSyin)
      _RL swetroot(nchp,nSxin,nSyin),swetdeep(nchp,nSxin,nSyin)
      _RL snodep(nchp,nSxin,nSyin),capac(nchp,nSxin,nSyin)
      _RL o3(im2,jm2,Nrphin,nSxin,nSyin)
      _RL qstr(im2,jm2,Nrphin,nSxin,nSyin)
      _RL co2,cfc11,cfc12,cfc22,n2o(Nrphin),methane(Nrphin)

      INTEGER iras(nSxin,nSyin)
      INTEGER nlwcld(nSxin,nSyin),nlwlz(nSxin,nSyin)
      INTEGER nswcld(nSxin,nSyin),nswlz(nSxin,nSyin)
      INTEGER imstturbsw(nSxin,nSyin),imstturblw(nSxin,nSyin)
      _RL cldtotlwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL cldraslwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL cldlsplwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL lwlzin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL cldtotswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL cldrasswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL cldlspswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL swlzin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL qliqaveswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL qliqavelwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL fccaveswin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL fccavelwin(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL rainlspin(idim1:idim2,jdim1:jdim2,nSxin,nSyin)
      _RL rainconin(idim1:idim2,jdim1:jdim2,nSxin,nSyin)
      _RL snowfallin(idim1:idim2,jdim1:jdim2,nSxin,nSyin)


      _RL duphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL dvphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL dthphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)
      _RL dsphy(idim1:idim2,jdim1:jdim2,Nrphin,nSxin,nSyin)


c Local Variables
      INTEGER ptracer,ntracer
      PARAMETER (ptracer = 1)
      PARAMETER (ntracer = 1)

      _RL xlats(sNx,sNy),xlons(sNx,sNy),sea_ice(sNx,sNy)
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
      _RL tempi(2)

      _RL kF,sigma_b,ks,ka,deg2rad,pi,atm_po,atm_kappa,termp,kv,kT
      _RL term1,term2,thetalim,thetaeq,recip_p0g

      LOGICAL alarm
      EXTERNAL alarm

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
