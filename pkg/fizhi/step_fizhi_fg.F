       subroutine step_fizhi_fg (myTime, myIter, myThid, dt)
c-----------------------------------------------------------------------
c  Subroutine step_fizhi_fg - 'Wrapper' routine to advance
c        the physics state and make a 'first guess' at the new
c        value. At this point, increment with the physics 
c        tendency only. 
c        Also: Set up "bi, bj loop" and some timers and clocks here.
c Call: step_physics
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "land_SIZE.h"
#include "fizhi_coms.h"
#include "EEPARAMS.h"

       integer myTime, myIter, myThid 

       integer i, j, L, bi, bj
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       _RL dt

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

c  Step forward the physics state using physics tendencies
        call step_physics(uphy,vphy,thphy,sphy,dt,im1,im2,jm1,jm2,
     .      Nrphys,Nsx,Nsy,1,sNx,1,sNy,bi,bj,duphy,dvphy,dthphy,dsphy)

       enddo
       enddo

       return
       end
