       subroutine gridalt_update(myThid)
c***********************************************************************
c subroutine gridalt_update
c 
c Purpose: Update the pressure thicknesses of the layers of the 
c          alternative vertical grid (used now for atmospheric physics).
c
c Calculate: dpphys    - new delta r (p*) edge-to-edge of physics grid
c                        using dpphys0 (initial value) and rstarfacC
c
c***********************************************************************
       implicit none
c
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "fizhi_SIZE.h"
#include "gridalt_mapping.h"
#include "SURFACE.h"

       integer myThid

       integer im1,im2,jm1,jm2
       integer i1,i2,j1,j2,bi,bj
       integer i,j,L

       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       i1 = 1
       i2 = sNx
       j1 = 1
       j2 = sNy
                                                                                         
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
c
        do L = 1,Nrphys
         do j = j1,j2
         do i = i1,i2+1
          dpphys(i,j,L,bi,bj) = dpphys0(i,j,L,bi,bj) *
     .                                   rstarfacC(i,j,bi,bj)
         enddo
         enddo
        enddo
c
       enddo
       enddo

       return
       end
