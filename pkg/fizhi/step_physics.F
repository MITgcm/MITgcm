       subroutine step_physics(uphy,vphy,thphy,sphy,dt,
     .  im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,idim1,idim2,jdim1,jdim2,bi,bj,
     .                                         duphy,dvphy,dthphy,dsphy)
c-----------------------------------------------------------------------
c Routine to step physics state forward using the increments 
c
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"                                                               
       integer im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
       integer Nrphys,Nsx,Nsy,bi,bj
       _RL uphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL vphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL thphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL sphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL duphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL dvphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL dthphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL dsphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL dt
c
       integer i,j,L

       do L = 1,Nrphys
        do j = jdim1,jdim2
        do i = idim1,idim2
         uphy(i,j,L,bi,bj) = uphy(i,j,L,bi,bj) + duphy(i,j,L,bi,bj) * dt
         vphy(i,j,L,bi,bj) = vphy(i,j,L,bi,bj) + dvphy(i,j,L,bi,bj) * dt
         thphy(i,j,L,bi,bj)=thphy(i,j,L,bi,bj) + dthphy(i,j,L,bi,bj)* dt
         sphy(i,j,L,bi,bj) = sphy(i,j,L,bi,bj) + dsphy(i,j,L,bi,bj) * dt
        enddo
        enddo
       enddo

       return
       end
