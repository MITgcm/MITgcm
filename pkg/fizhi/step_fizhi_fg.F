C $Header: /u/gcmpack/MITgcm/pkg/fizhi/step_fizhi_fg.F,v 1.11 2004/10/22 14:52:14 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
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
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "gridalt_mapping.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

       integer myIter, myThid 
       _RL myTime

       integer bi, bj
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       _RL dt

       _RL tempij(sNx,sNy)
       integer i,j,L

       idim1 = 1-OLx
       idim2 = sNx+OLx
       jdim1 = 1-OLy
       jdim2 = sNy+OLy
       im1 = 1
       im2 = sNx
       jm1 = 1
       jm2 = sNy

       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

c  Step forward the physics state using physics tendencies
        call step_physics(uphy,vphy,thphy,sphy,dt,idim1,idim2,
     .      jdim1,jdim2,
     .      Nrphys,Nsx,Nsy,1,sNx,1,sNy,bi,bj,duphy,dvphy,dthphy,dsphy)

      if(2.eq.1 )then
      print *,' In step fizhi fg, new fizhi fields ',bi,' dt= ',dt
      do L = 1,Nrphys
       do j = jm1,jm2
       do i = im1,im2
        tempij(i,j) = uphy(i,j,L,bi,bj)
       enddo
       enddo
c      print *,' uphy at level ',l,' ',tempij
      enddo
      do L = 1,Nrphys
       do j = jm1,jm2
       do i = im1,im2
        tempij(i,j) = vphy(i,j,L,bi,bj)
       enddo
       enddo
c      print *,' vphy at level ',l,' ',tempij
      enddo
      do L = 1,Nrphys
       do j = jm1,jm2
       do i = im1,im2
        tempij(i,j) = thphy(i,j,L,bi,bj)
       enddo
       enddo
       print *,' thphy at level ',l,' ',tempij
      enddo
      do L = 1,Nrphys
       do j = jm1,jm2
       do i = im1,im2
        tempij(i,j) = sphy(i,j,L,bi,bj)
       enddo
       enddo
       print *,' sphy at level ',l,' ',tempij
      enddo
      endif

        call qcheck (idim1,idim2,jdim1,jdim2,Nrphys,Nsx,Nsy,im1,im2,
     .       jm1,jm2,bi,bj,dpphys,sphy)

      if(2.eq.1 )then
      print *,' In step fizhi fg after qcheck ',bi
      do L = 1,Nrphys
       do j = jm1,jm2
       do i = im1,im2
        tempij(i,j) = sphy(i,j,L,bi,bj)
       enddo
       enddo
       print *,' sphy after qcheck at level ',l,' ',tempij
      enddo
      endif

       enddo
       enddo

       return
       end
