       subroutine do_fizhi(uphy,vphy,thphy,sphy,
     .                              ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     .im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,idim1,idim2,jdim1,jdim2,bi,bj,nchp,
     .                                         duphy,dvphy,dthphy,dsphy)
c-----------------------------------------------------------------------
c Dummy routine to calculate physics increments - here set them to whatever
c
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"

       integer im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
       integer Nrphys,Nsx,Nsy,bi,bj,nchp
       _RL uphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL vphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL thphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
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

       do L = 1,Nrphys
        do j = jdim1,jdim2
        do i = idim1,idim2
         duphy(i,j,L,bi,bj) = 2.e-4*L
         dvphy(i,j,L,bi,bj) = 0.5e-4*L
         dthphy(i,j,L,bi,bj) = 4.e-4*L
         dsphy(i,j,L,bi,bj) = 2.e-7*L
        enddo
        enddo
       enddo

       return
       end
