       subroutine update_chemistry_exports (myTime, myIter, myThid)
c----------------------------------------------------------------------
c  Subroutine update_chemistry_exports - 'Wrapper' routine to update
c        the fields related to the earth's chemistry that are needed
c        by fizhi.
c        Also: Set up "bi, bj loop" and some timers and clocks here.
c
c Call:  interp_chemistry
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "chemistry_coms.h"
#include "gridalt_mapping.h"
#include "EEPARAMS.h"

       integer myTime, myIter, myThid

c pe on physics grid refers to bottom edge
       _RL pephy(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys+1,nSx,nSy)

       integer i, j, L, bi, bj
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
                                                                                    
       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       idim1 = 1
       idim2 = sNx
       jdim1 = 1
       jdim2 = sNy
                                                                                    
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

C Compute pressures on physics grid
        do j = 1,sNy
        do i = 1,sNx
         pephy(i,j,1,bi,bj)=Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
c        do L = 1,Nr
c         pephy(i,j,1,bi,bj)=pephy(i,j,1,bi,bj) -
c    .                                  (1.-hfacC(i,j,L,bi,bj))*drF(L)
c        enddo
         do L = 2,Nrphys+1
          pephy(i,j,L,bi,bj)=pephy(i,j,L-1,bi,bj)-dpphys(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pephy(i,j,Nrphys+1,bi,bj).lt.1.e-5)
     .                               pephy(i,j,Nrphys+1,bi,bj) = 1.e-5
        enddo
        enddo

call interp chemistry
c reminder - lats are in yC and lons in xC in GRID.h

       enddo
       enddo

       return
       end
