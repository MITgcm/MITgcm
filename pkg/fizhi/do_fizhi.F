C $Header: /u/gcmpack/MITgcm/pkg/fizhi/do_fizhi.F,v 1.9 2004/06/15 22:18:27 molod Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"
       subroutine do_fizhi(uphy,vphy,thphy,sphy,pephy,lons,lats,
     .   ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     .   sst,sice,phis_var,landtype,emiss,albnidr,albnirdf,
     .   albvisdr,albvisdf,ityp,chfr,alai,agrn,igrd,chlat,chlon,
     .   tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,snodep,capac,
     .   o3,qstr,co2,cfc11,cfc12,cfc22,n2o,methane,
     .   idim1,idim2,jdim1,jdim2,Nrphys,Nsx,Nsy,im1,im2,jm1,jm2,bi,bj,
     .   nchp,
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

      integer im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
      integer Nrphys,Nsx,Nsy,bi,bj,nchp
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
      _RL duphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dvphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dthphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
      _RL dsphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
c
      integer i,j,L

      call fizhi_driver (nymd,nhms,myid,im,jm,lm,ptracer,ntracer,
     . ndmoist,ndturb,ndlw,ndsw,xlats,xlons,
     . p,u,v,t,q,pl,ple,dpres,pkht,pkl,fracland,landtype,radswt,
     . maxtyp,surftype,tilefrac,phis_var,sea_ice,
     . nchp,chlat,chlon,igrd,nchpland,chfr,ityp,
     . tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,capac,snodep,
     . ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     . stratq,nwatlevs,nwatlats,watlevs,watlats,ozone,nozolevs,
     . albvisdr,albvisdf,albnirdr,albnirdf,emiss,alai,agrn,
     . nozolats,ozolevs,ozolats,co2,cfc11,cfc12,cfc22,methane,n2o,
     . lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq,
     . moistu,moistv,moistt,moistq,
     . radswg,swgclr,albedo,fdirpar,fdifpar,osr,osrclr,tg0,tg0c,radlwg,
     . st4,dst4,dlwdtg,lwclr,rainlsp,raincon,snowfall,iras,nlwcld,
     . cldtot_lw,clras_lw,cldlsp_lw,nlwlz,lwlz,nswcld,cldtot_sw,
     . clras_sw,cldlsp_sw,nswlz,swlz,imstturbsw,imstturblw,qliqavesw,
     . qliqavelw,fccavesw,fccavelw,qq,u2m,v2m,t2m,q2m,u10m,v10m,t10m,
     . q10m,tgz,gwet,snow,chemo3,chemq,diag2d,ndiag2d,diag3d,ndiag3d)


      return
      end
