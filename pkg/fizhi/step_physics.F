C $Header: /u/gcmpack/MITgcm/pkg/fizhi/step_physics.F,v 1.6 2004/08/29 19:39:42 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       subroutine step_physics(uphy,vphy,thphy,sphy,dt,
     .  idim1,idim2,jdim1,jdim2,Nrphys,Nsx,Nsy,im1,im2,jm1,jm2,bi,bj,
     .                                         duphy,dvphy,dthphy,dsphy)
c-----------------------------------------------------------------------
c Routine to step physics state forward using the increments 
c
c-----------------------------------------------------------------------
       implicit none
       integer im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
       integer Nrphys,Nsx,Nsy,bi,bj
       _RL uphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
       _RL vphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
       _RL thphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
       _RL sphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
       _RL duphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
       _RL dvphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
       _RL dthphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
       _RL dsphy(idim1:idim2,jdim1:jdim2,Nrphys,Nsx,Nsy)
       _RL dt
c
       integer i,j,L

       do L = 1,Nrphys
        do j = jm1,jm2
        do i = im1,im2
         uphy(i,j,L,bi,bj) = uphy(i,j,L,bi,bj) + duphy(i,j,L,bi,bj) * dt
         vphy(i,j,L,bi,bj) = vphy(i,j,L,bi,bj) + dvphy(i,j,L,bi,bj) * dt
         thphy(i,j,L,bi,bj)=thphy(i,j,L,bi,bj) + dthphy(i,j,L,bi,bj)* dt
         sphy(i,j,L,bi,bj) = sphy(i,j,L,bi,bj) + dsphy(i,j,L,bi,bj) * dt
        enddo
        enddo
       enddo

       return
       end
