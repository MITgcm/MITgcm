       subroutine fizhi_init_vars (myThid)
c-----------------------------------------------------------------------
c  Routine to initialise the fizhi state.
c  
c  Input: myThid       - Process number calling this routine
c
c  Notes: 
c   1) For a Cold Start - 
c      This routine takes the initial condition on the dynamics grid
c      and interpolates to the physics grid to initialize the state
c      variables that are on both grids. It initializes the variables
c      of the turbulence scheme to 0., and the land state from a model
c      climatology.
c   2) For a Restart, read the fizhi pickup file
c   3) The velocity component physics fields are on an A-Grid
c
c Calls: dyn2phys (x4)
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "land_SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "gridalt_mapping.h"
#include "fizhi_coms.h"
#include "land_coms.h"
#include "EEPARAMS.h"
#include "SURFACE.h"
#include "PARAMS.h"

       integer myThid

c pe on dynamics and physics grid refers to bottom edge
       _RL pephy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys+1,nSx,nSy)
       _RL pedyn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1,nSx,nSy)
       _RL windphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
       _RL udyntemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
       _RL vdyntemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

       integer i, j, L, bi, bj, Lbotij
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2

       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       idim1 = 1
       idim2 = sNx
       jdim1 = 1
       jdim2 = sNy

      IF ( startTime.EQ.0. .AND. nIter0.EQ.0 ) THEN
      print *,' In fizhi_init_vars: Cold start '

       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

C Build pressures on dynamics grid
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nr
          pedyn(i,j,L,bi,bj) = 0.
         enddo
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx+1
         Lbotij = ksurfC(i,j,bi,bj)
         if(Lbotij.ne.0.) 
     .    pedyn(i,j,Lbotij,bi,bj) = Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
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
C Build pressures on physics grid
        do j = 1,sNy
        do i = 1,sNx
         pephy(i,j,1,bi,bj)=Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
c        do L = 1,Nr
c         pephy(i,j,1,bi,bj)=pephy(i,j,1,bi,bj) -
c    .                                    (1.-hfacC(i,j,L,bi,bj))*drF(L)
c        enddo
         do L = 2,Nrphys+1
          pephy(i,j,L,bi,bj)=pephy(i,j,L-1,bi,bj)-dpphys0(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pephy(i,j,Nrphys+1,bi,bj).lt.1.e-5)
     .                               pephy(i,j,Nrphys+1,bi,bj) = 1.e-5
        enddo
        enddo
c
c Create an initial wind magnitude field on the physics grid -
c   Use a log wind law with z0=1cm, u*=1 cm/sec,
c   do units and get u = .025*ln(dP*10), with dP in pa.
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         windphy(i,j,L,bi,bj) = 0.025 *
     .             log((pephy(i,j,1,bi,bj)-pephy(i,j,L+1,bi,bj))*10.)
        enddo
        enddo
        enddo

       enddo
       enddo
                                                                                   
c Create initial fields on phys. grid - Move Dynamics u and v to A-Grid
       call CtoA(myThid,uvel,vvel,maskW,maskS,im1,im2,jm1,jm2,Nr,
     .                     Nsx,Nsy,1,sNx,1,sNy,udyntemp,vdyntemp)

       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

c Create initial fields on phys. grid - interpolate from dyn. grid
        call dyn2phys(udyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,
     .    1,sNx,1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,1,uphy)
        call dyn2phys(vdyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,
     .    1,sNx,1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,1,vphy)
        call dyn2phys(theta,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,
     .   1,sNx,1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,0,thphy)
        call dyn2phys(salt,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,
     .    1,sNx,1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,0,sphy)

c Now initialize tke, xlmt, khmt, xxmt, yymt, ctmt, zetamt,
c Now initialize land state too - tcanopy, etc...
       do L = 1,Nrphys
        do i = 1,nchp
         tke(i,L,bi,bj) = 0.
         xlmt(i,L,bi,bj) = 0.
         khmt(i,L,bi,bj) = 0.
        enddo
       enddo
c Now initialize land state too - tcanopy, etc... ZERO FOR NOW, 
c                                              READ CLIM FOR REAL
       do i = 1,nchp
        tcanopy(i) = 0.
        tdeep(i) = 0.
        ecanopy(i) = 0.
        swetshal(i) = 0.
        swetroot(i) = 0.
        swetdeep(i) = 0.
        capac(i) = 0.
        snodep(i) = 0.
       enddo

       enddo
       enddo

      ELSE
      print *,' In fizhi_init_vars: Read from restart '
                                                                                   
C--   Read fizhi package state variables from pickup file
        call fizhi_read_pickup( nIter0, myThid )
                                                                                   
      ENDIF

       return
       end
