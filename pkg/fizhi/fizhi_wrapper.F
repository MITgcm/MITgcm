       subroutine fizhi_wrapper (myTime, myIter, myThid)
c-----------------------------------------------------------------------
c  Subroutine fizhi_wrapper - 'Wrapper' routine to interface
c        with physics driver. 
c        1) Set up "bi, bj loop"  and some timers and clocks. 
c        2) Call do_fizhi - driver for physics which computes tendencies
c        3) Interpolate tendencies to dynamics grid in vertical
c        4) Convert u,v tendencies to C-Grid
c
c Calls: do_fizhi (get u,v,t,s tend, step tke, etc and tc, etc. forward)
c        phys2dyn (4 calls - all physics tendencies)
c        AtoC (u and v tendencies)
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
#include "SURFACE.h"

       integer myTime, myIter, myThid 

c pe on dynamics and physics grid refers to bottom edge
       _RL pephy(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nrphys+1,nSx,nSy)
       _RL pedyn(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr+1,nSx,nSy)

       integer i, j, L, Lbotij, bi, bj
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

c  Construct the physics grid pressures
        do j = 1,sNy
        do i = 1,sNx
         pephy(i,j,1,bi,bj)=(Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj))/
     .                       rstarExpC(i,j,bi,bj)
         do L = 2,Nrphys+1
          pephy(i,j,L,bi,bj)=pephy(i,j,L-1,bi,bj)-dpphys(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pephy(i,j,Nrphys+1,bi,bj).lt.1.e-5)
     .                               pephy(i,j,Nrphys+1,bi,bj) = 1.e-5
        enddo
        enddo
C Build pressures on dynamics grid
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nr
          pedyn(i,j,L,bi,bj) = 0.
         enddo
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         Lbotij = ksurfC(i,j,bi,bj)
         if(Lbotij.ne.0.) 
     . pedyn(i,j,Lbotij,bi,bj) = (Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj))/
     .                         rstarExpC(i,j,bi,bj)
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         Lbotij = ksurfC(i,j,bi,bj)
         do L = Lbotij+1,Nr+1
          pedyn(i,j,L,bi,bj) = pedyn(i,j,L-1,bi,bj) - 
     .                        drF(L-1)*hfacC(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pedyn(i,j,Nr+1,bi,bj).lt.1.e-5)
     .                               pedyn(i,j,Nr+1,bi,bj) = 1.e-5
        enddo
        enddo
       enddo
       enddo

       CALL TIMER_START ('DO_FIZHI          [FIZHI_WRAPPER]',mythid)
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
c
c Compute physics increments
        call do_fizhi(uphy,vphy,thphy,sphy,pephy,
     .                              ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     .                  xC,yC,
     .            im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,1,sNx,1,sNy,bi,bj,nchp,
     .                                         duphy,dvphy,dthphy,dsphy)
       enddo
       enddo
       CALL TIMER_STOP ('DO_FIZHI          [FIZHI_WRAPPER]',mythid)

       CALL TIMER_START ('PHYS2DYN          [FIZHI_WRAPPER]',mythid)
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
c Interpolate (A-Grid) physics increments to dynamics grid
        call phys2dyn(duphy,pephy,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,
     .       1,sNx,1,sNy,bi,bj,pedyn,ksurfC,Nr,nlperdyn,guphy)
        call phys2dyn(dvphy,pephy,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,
     .       1,sNx,1,sNy,bi,bj,pedyn,ksurfC,Nr,nlperdyn,gvphy)
        call phys2dyn(dthphy,pephy,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,
     .       1,sNx,1,sNy,bi,bj,pedyn,ksurfC,Nr,nlperdyn,gthphy)
        call phys2dyn(dsphy,pephy,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,
     .       1,sNx,1,sNy,bi,bj,pedyn,ksurfC,Nr,nlperdyn,gsphy)

       enddo
       enddo
       CALL TIMER_STOP ('PHYS2DYN          [FIZHI_WRAPPER]',mythid)

c Convert guphy and gvphy from A-grid to C-grid for use by dynamics
       CALL TIMER_START ('ATOC              [FIZHI_WRAPPER]',mythid)
       call AtoC(myThid,guphy,gvphy,maskC,im1,im2,jm1,jm2,Nr,
     .                      Nsx,Nsy,1,sNx,1,sNy,guphy,gvphy)
       CALL TIMER_STOP ('ATOC              [FIZHI_WRAPPER]',mythid)

       CALL TIMER_START ('EXCHANGES         [FIZHI_WRAPPER]',mythid)
c Call the c-grid exchange routine to fill in the halo regions (du,dv)
       call exch_uv_xyz_RL(guphy,gvphy,.TRUE.,myThid)
c Call the a-grid exchange routine to fill in the halo regions (dth,ds)
       call exch_RL_cube(gthphy,OLx, OLx, OLy, OLy, Nr,OLx, OLy,
     .            FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       call exch_RL_cube(gsphy,OLx, OLx, OLy, OLy, Nr,OLx, OLy,
     .            FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       CALL TIMER_STOP ('EXCHANGES         [FIZHI_WRAPPER]',mythid)

      return
      end
