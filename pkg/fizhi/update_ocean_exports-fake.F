       subroutine update_ocean_exports (myTime, myIter, myThid)
c----------------------------------------------------------------------
c  Subroutine update_ocean_exports - 'Wrapper' routine to update
c        the fields related to the ocean's surface that are needed
c        by fizhi.
c        Also: Set up "bi, bj loop" and some timers and clocks here.
c
c Call:  getsst  (Return the current sst field-read dataset if needed)
c        getsice (Return the current sea ice field-read data if needed)
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "GRID.h"
#include "ocean_coms.h"
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

c      call getsst
c      call getsice

       enddo
       enddo

       return
       end
