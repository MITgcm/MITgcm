       subroutine CtoA(myThid,fieldin1,fieldin2,mask1,mask2,im1,im2,jm1,
     .  jm2,numlevs,Nsx,Nsy,idim1,idim2,jdim1,jdim2,fieldout1,fieldout2)
c----------------------------------------------------------------------
c  Subroutine CtoA - Routine to map a velocity component quantity
c         from the C-Grid to the A-Grid. 
c         This includes doing an exchange to fill the halo region, and 
c         then a linear average with the appropriate topography mask.
c         Also: Set up "bi, bj loop" here.
c
c  Input: myThid
c         fieldin1    Field on c-grid to move to a-grid (1st component)
c         fieldin2    Field on c-grid to move to a-grid (2nd component)
c         mask1       Topography [0,1] mask - 1 to indicate above ground
c         mask2       Topography [0,1] mask - 1 to indicate above ground
c         im1,im2,jm1,jm2   Indeces in x and y for computations
c         numlevs     Number of vertical levels
c         Nsx, Nsy    
c         idim1,idim2,jdim1,jdim2   Span of fields in x and y
c
c Output: fieldout1   Field mapped to A-Grid (1st component)
c         fieldout2   Field mapped to A-Grid (2nd component)
c
c Call: exchange on c-grid
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "EEPARAMS.h"

       integer myThid, numlevs
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       integer Nsx, Nsy
       _RS mask1(im1:im2,jm1:jm2,numlevs,Nsx,Nsy)
       _RS mask2(im1:im2,jm1:jm2,numlevs,Nsx,Nsy)
       _RL fieldin1(im1:im2,jm1:jm2,numlevs,Nsx,Nsy)
       _RL fieldin2(im1:im2,jm1:jm2,numlevs,Nsx,Nsy)
       _RL fieldout1(im1:im2,jm1:jm2,numlevs,Nsx,Nsy)
       _RL fieldout2(im1:im2,jm1:jm2,numlevs,Nsx,Nsy)

       integer i, j, L, bi, bj
       logical withSigns
       data withSigns/.TRUE./

c Call the c-grid exchange routine to fill in the halo regions
       call exch_uv_xyz_RL(fieldin1,fieldin2,withSigns,myThid)

c Now take average
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

        do L = 1,numlevs
         do j = jdim1-1,jdim2+1
         do i = idim1-1,idim2+1
          if( (mask1(i,j,L,bi,bj).ne.0.) .or. 
     .                               (mask1(i+1,j,L,bi,bj).ne.0.) ) then
           fieldout1(i,j,L,bi,bj) = 
     .        ( fieldin1(i,j,L,bi,bj)*mask1(i,j,L,bi,bj) + 
     .          fieldin1(i+1,j,L,bi,bj)*mask1(i+1,j,L,bi,bj) ) /
     .            ( mask1(i,j,L,bi,bj) + mask1(i+1,j,L,bi,bj) )
          else
           fieldout1(i,j,L,bi,bj) = 0.
          endif
          if( (mask2(i,j,L,bi,bj).ne.0.) .or. 
     .                               (mask2(i,j+1,L,bi,bj).ne.0.) ) then
           fieldout2(i,j,L,bi,bj) = 
     .        ( fieldin2(i,j,L,bi,bj)*mask2(i,j,L,bi,bj) + 
     .          fieldin2(i,j+1,L,bi,bj)*mask2(i,j+1,L,bi,bj) ) /
     .            ( mask2(i,j,L,bi,bj) + mask2(i,j+1,L,bi,bj) )
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
