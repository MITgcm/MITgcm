C $Header: /u/gcmpack/MITgcm/pkg/fizhi/do_fizhi.F,v 1.3 2004/06/08 15:25:27 molod Exp $
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
#include "CPP_OPTIONS.h"

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

       call physdrv(         )

       return
       end
