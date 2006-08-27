C $Header: /u/gcmpack/MITgcm/pkg/fizhi/AtoC.F,v 1.8 2006/08/27 18:27:36 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       subroutine AtoC(myThid,fieldin1,fieldin2,mask,idim1,idim2,
     .  jdim1,jdim2,numlevs,Nsx,Nsy,im1,im2,jm1,jm2,fieldout1,fieldout2)
c----------------------------------------------------------------------
c  Subroutine AtoC - Routine to map a velocity component quantity
c         from the A-Grid to the C-Grid. 
c         This includes doing an exchange to fill the halo region, and 
c         then a linear average with the appropriate topography mask.
c         Also: Set up "bi, bj loop" here.
c
c  Input: myThid
c         fieldin1    Field on a-grid to move to a-grid (1st component)
c         fieldin2    Field on a-grid to move to a-grid (2nd component)
c         mask        Topography [0,1] mask - 1 to indicate above ground
c         idim1,idim2,jdim1,jdim2   Indeces in x and y for computations
c         numlevs     Number of vertical levels
c         Nsx, Nsy    
c         im1,im2,jm1,jm2   Span of fields in x and y
c
c Output: fieldout1   Field mapped to C-Grid (1st component)
c         fieldout2   Field mapped to C-Grid (2nd component)
c
c Call:   exch_uv_agrid_3d_RL - exchange on a-grid
c-----------------------------------------------------------------------
       implicit none
#include "EEPARAMS.h"

       integer myThid, numlevs
       integer Nsx, Nsy
       integer idim1, idim2, jdim1, jdim2, im1, im2, jm1, jm2
       _RS mask(idim1:idim2,jdim1:jdim2,numlevs,Nsx,Nsy)
       _RL fieldin1(idim1:idim2,jdim1:jdim2,numlevs,Nsx,Nsy)
       _RL fieldin2(idim1:idim2,jdim1:jdim2,numlevs,Nsx,Nsy)
       _RL fieldout1(idim1:idim2,jdim1:jdim2,numlevs,Nsx,Nsy)
       _RL fieldout2(idim1:idim2,jdim1:jdim2,numlevs,Nsx,Nsy)

       integer i, j, L, bi, bj
       _RL tmpfld1(idim1:idim2,jdim1:jdim2)
       _RL tmpfld2(idim1:idim2,jdim1:jdim2)

c Call the exchange routine to fill in the halo regions
       CALL EXCH_UV_AGRID_3D_RL(
     U                           fieldin1, fieldin2,
     I                           .TRUE., numlevs, myThid )

c Now take average 
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

        do L = 1,numlevs
         do j = jdim1,jdim2
          do i = idim1,idim2
           tmpfld1(i,j) = fieldin1(i,j,L,bi,bj)*mask(i,j,L,bi,bj)
           tmpfld2(i,j) = fieldin2(i,j,L,bi,bj)*mask(i,j,L,bi,bj)
          enddo
         enddo
         do j = jm1,jm2
         do i = im1,im2
c        do j = jm2,jm1,-1
c        do i = im2,im1,-1
          if( (mask(i-1,j,L,bi,bj).ne.0.) .or.
     .                                  (mask(i,j,L,bi,bj).ne.0.) ) then
c          fieldout1(i,j,L,bi,bj) = 
c    .       ( fieldin1(i-1,j,L,bi,bj)*mask(i-1,j,L,bi,bj) +
c    .         fieldin1(i,j,L,bi,bj)*mask(i,j,L,bi,bj) ) /
c    .           ( mask(i-1,j,L,bi,bj) + mask(i,j,L,bi,bj) )
           fieldout1(i,j,L,bi,bj) = 
     .           ( tmpfld1(i-1,j) + tmpfld1(i,j) ) /
     .           ( mask(i-1,j,L,bi,bj) + mask(i,j,L,bi,bj) )
          else
           fieldout1(i,j,L,bi,bj) = 0.
          endif
          if( (mask(i,j-1,L,bi,bj).ne.0.) .or.
     .                                  (mask(i,j,L,bi,bj).ne.0.) ) then
c          fieldout2(i,j,L,bi,bj) = 
c    .       ( fieldin2(i,j-1,L,bi,bj)*mask(i,j-1,L,bi,bj) +
c    .         fieldin2(i,j,L,bi,bj)*mask(i,j,L,bi,bj) ) /
c    .           ( mask(i,j,L,bi,bj) + mask(i,j-1,L,bi,bj) )
           fieldout2(i,j,L,bi,bj) = 
     .           ( tmpfld2(i,j-1) + tmpfld2(i,j) ) /
     .           ( mask(i,j,L,bi,bj) + mask(i,j-1,L,bi,bj) )
          else
           fieldout2(i,j,L,bi,bj) = 0.
          endif
         enddo
         enddo
        enddo

       enddo
       enddo

       return
       end
