       subroutine update_earth_exports (myTime, myIter, myThid)
c----------------------------------------------------------------------
c  Subroutine update_earth_exports - 'Wrapper' routine to update
c        the fields related to the earth's surface that are needed
c        by fizhi.
c        Also: Set up "bi, bj loop" and some timers and clocks here.
c
c Call:  getalb    (Set the 4 albedos based on veg type and time)
c        getemiss  (Set the surface emissivity based on the veg type
c                              and the snow depth)
c        getlgr    (Set the leaf area index and surface greenness, 
c                              based on veg type and month) 
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "GRID.h"
#include "land_SIZE.h"
#include "fizhi_SIZE.h"
#include "DYNVARS.h"
#include "fizhi_coms.h"
#include "gridalt_mapping.h"
#include "land_coms.h"
#include "EEPARAMS.h"

       integer myTime, myIter, myThid

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

       enddo
       enddo

       return
       end
