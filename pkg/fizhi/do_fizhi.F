C $Header: /u/gcmpack/MITgcm/pkg/fizhi/do_fizhi.F,v 1.6 2004/06/15 16:06:03 molod Exp $
C $Name:  $
       subroutine do_fizhi(uphy,vphy,thphy,sphy,pephy,
     .                              ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     .im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,idim1,idim2,jdim1,jdim2,bi,bj,nchp,
     .                                         duphy,dvphy,dthphy,dsphy)
c-----------------------------------------------------------------------
c Interface routine to calculate physics increments - call geos-3 driver
c Purpose of this routine is to pull things from model common blocks
c to send to physdrv. Also to take output from there and put into model
c common blocks.
c
c-----------------------------------------------------------------------
      implicit none
#include "CPP_EEOPTIONS.h"

      integer im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
      integer Nrphys,Nsx,Nsy,bi,bj,nchp
      _RL uphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
      _RL vphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
      _RL thphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
      _RL pephy(im1:im2,jm1:jm2,Nrphys+1,Nsx,Nsy)
      _RL sphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
      _RL ctmt(nchp,Nsx,Nsy),xxmt(nchp,Nsx,Nsy),yymt(nchp,Nsx,Nsy)
      _RL zetamt(nchp,Nsx,Nsy)
      _RL xlmt(nchp,Nrphys,Nsx,Nsy),khmt(nchp,Nrphys,Nsx,Nsy)
      _RL tke(nchp,Nrphys,Nsx,Nsy)
      _RL duphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
      _RL dvphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
      _RL dthphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
      _RL dsphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
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
