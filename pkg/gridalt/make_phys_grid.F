C $Header: /u/gcmpack/MITgcm/pkg/gridalt/make_phys_grid.F,v 1.7 2006/07/06 13:47:28 molod Exp $
C $Name:  $

       subroutine make_phys_grid(drF,hfacC,im1,im2,jm1,jm2,Nr,
     . Nsx,Nsy,i1,i2,j1,j2,bi,bj,Nrphys,Lbot,dpphys,numlevphys,nlperdyn)
c***********************************************************************
c subroutine make_phys_grid
c 
c Purpose: Define the grid that the will be used to run the high-end
c          atmospheric physics.
c
c Algorithm: Fit additional levels of some (~) known thickness in 
c          between existing levels of the grid used for the dynamics
c
c Need:    Information about the dynamics grid vertical spacing
c     
c Input:   drF         - delta r (p*) edge-to-edge
c          hfacC       - fraction of grid box above topography
c          im1, im2    - beginning and ending i - dimensions
c          jm1, jm2    - beginning and ending j - dimensions
c          Nr          - number of levels in dynamics grid
c          Nsx,Nsy     - number of processes in x and y direction
c          i1, i2      - beginning and ending i - index to fill
c          j1, j2      - beginning and ending j - index to fill
c          bi, bj      - x-dir and y-dir index of process
c          Nrphys      - number of levels in physics grid
c
c Output:  dpphys      - delta r (p*) edge-to-edge of physics grid
c          numlevphys  - number of levels used in the physics
c          nlperdyn    - physics level number atop each dynamics layer
c
c NOTES: 1) Pressure levs are built up from bottom, using p0, ps and dp:
c              p(i,j,k)=p(i,j,k-1) + dp(k)*ps(i,j)/p0(i,j)
c        2) Output dp's are aligned to fit EXACTLY between existing
c           levels of the dynamics vertical grid
c        3) IMPORTANT! This routine assumes the levels are numbered
c           from the bottom up, ie, level 1 is the surface.
c           IT WILL NOT WORK OTHERWISE!!!
c        4) This routine does NOT work for surface pressures less 
c           (ie, above in the atmosphere) than about 350 mb
c***********************************************************************
       implicit none
c
#include "CPP_OPTIONS.h"

       integer im1,im2,jm1,jm2,Nr,Nsx,Nsy,Nrphys
       integer i1,i2,j1,j2,bi,bj
       integer numlevphys
       _RL hfacC(im1:im2,jm1:jm2,Nr,Nsx,Nsy)
       _RL dpphys(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL drF(Nr)
       integer Lbot(im1:im2,jm1:jm2,Nsx,Nsy)
       integer nlperdyn(im1:im2,jm1:jm2,Nr,Nsx,Nsy)
c
       integer i,j,L,Lbotij,Lnew
c Require 12 (or 15) levels near the surface (300 mb worth) for fizhi. 
c   the dp's are in the dptry arrays:
       integer ntry,ntry10,ntry40
       data ntry10 /12/
       data ntry40 /15/
       _RL dptry(15),dptry10(12),dptry40(15) 
       _RL dptry_accum(15)
       data dptry10/300.000,600.000,1000.000,1400.000,1700.000,2500.000,
     .            2500.000,2500.000,2500.000,5000.000,5000.000,5000.000/
       data dptry40/300.000,600.000,1000.000,1400.000,1700.000,
     .            2500.000,2500.000,2500.000,2500.000,2500.000,2500.000,
     .            2500.000,2500.000,2500.000,2500.000/
       _RL deltap, dpstar_accum
       integer nlbotmax, nstart, nlevs, nlphys, ndone
       integer nextra
c
       if( (Nr.eq.10) .or. (Nr.eq.20) ) then
        ntry = ntry10
        do L = 1,ntry
         dptry(L) = dptry10(L)
        enddo
       elseif(Nr.eq.40) then
        ntry = ntry40
        do L = 1,ntry
         dptry(L) = dptry40(L)
        enddo
       else
        print *,' Dont know how to make fizhi grid '
        stop
       endif
c
       nextra = ntry
c
       do L = 1,Nr
        do j = j1,j2
        do i = i1,i2+1
         nlperdyn(i,j,L,bi,bj) = 0
        enddo
        enddo
       enddo
c
c Figure out how many physics levels there will be
c   (need 12 between sfc and 300 mb above it - see how many
c    there are in the dynamics if the surface pressure is at
c    the sum of drF, ie, the maximum dynamics grid layers possible)
       nlevs = 0
       dpstar_accum = 0.
       do L = 1,Nr
        dpstar_accum = dpstar_accum + drF(L)
        if(dpstar_accum.le.30000.) nlevs = nlevs+1
       enddo
       numlevphys = Nr - nlevs + ntry + 1
c
       dptry_accum(1) = dptry(1)
       do Lnew = 2,ntry
        dptry_accum(Lnew) = dptry_accum(Lnew-1) + dptry(Lnew)
       enddo
c
c      do for each grid point:
       do j = j1,j2
       do i = i1,i2
        Lbotij = Lbot(i,j,bi,bj)
c
c Find the maximum number of physics levels to fit in the bottom level
c
        nlbotmax = 0
        do Lnew = 1,ntry
         if ( (nlbotmax.eq.0) .and. 
     . (dptry_accum(Lnew).gt.(hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij))))then
          nlbotmax = Lnew
         endif
        enddo
        if(nlbotmax.eq.0)then
         nlbotmax = ntry
        endif
c
c See how many of the physics levs can fit in the bottom level
c
        nlphys = 0
        deltap = 0.
        do Lnew = 1,nlbotmax
c Test to see if the next physics level fits, if yes, add it
         if((hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij)).ge.
     .                                           deltap+dptry(Lnew))then
          nlphys = nlphys + 1
          dpphys(i,j,nlphys,bi,bj) = dptry(Lnew)
          deltap = deltap + dptry(Lnew)
         else
c If the level does not fit, decide whether to make a new thinner
c  one or make the one below a bit thicker
          if((dptry(Lnew-1)+(hfacC(i,j,Lbotij,bi,bj)*
     .             drF(Lbotij)-deltap)) .gt. (dptry(Lnew-1)*1.5) ) then
c Add a new thin layer
           nlphys = nlphys + 1
           dpphys(i,j,nlphys,bi,bj) = 
     .                      (hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij))-deltap
          else
c Make the one below thicker
           dpphys(i,j,nlphys,bi,bj) = dpphys(i,j,nlphys,bi,bj) + 
     .                      (hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij)-deltap)
          endif
          deltap = deltap+(hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij)-deltap)
         endif
        enddo
c
        nlperdyn(i,j,Lbotij,bi,bj) = nlphys
c
c Now proceed upwards - see how many physics levels fit in each
c   subsequent dynamics level - go through all 12 required levels
c                
        do L = Lbotij+1,Nr
         ndone = 0
         if(nlphys.lt.ntry)then
          deltap = 0.
          nstart = nlphys + 1
          do Lnew = nstart,ntry
           if((hfacC(i,j,L,bi,bj)*drF(L)).ge.deltap+dptry(Lnew))then
            nlphys = nlphys + 1
            dpphys(i,j,nlphys,bi,bj) = dptry(Lnew)
            deltap = deltap + dptry(Lnew)
            ndone = 0
           elseif (ndone.eq.0) then
c If the level does not fit, decide whether to make a new thinner
c  one or make the one below a bit thicker
            ndone = 1
            if( (dptry(Lnew-1)+(hfacC(i,j,L,bi,bj)*drF(L)-deltap))
     .                    .gt. (dptry(Lnew-1)*1.5) ) then
c Add a new thin layer
             nlphys = nlphys + 1
             dpphys(i,j,nlphys,bi,bj) = 
     .                             (hfacC(i,j,L,bi,bj)*drF(L))-deltap
             deltap = hfacC(i,j,L,bi,bj)*drF(L)
            else
c Make the one below thicker
             dpphys(i,j,nlphys,bi,bj) = dpphys(i,j,nlphys,bi,bj) +
     .                      (hfacC(i,j,L,bi,bj)*drF(L)-deltap)
             deltap = hfacC(i,j,L,bi,bj)*drF(L)
            endif
           endif
          enddo
C Need one more peice of logic - if we finished Lnew loop and
C  now we are done adding new physics layers, we need to be sure
C  that we are at the edge of a dynamics layer. if not, we need
C  to add one more layer.
          if(nlphys.ge.ntry)then
           if(abs(deltap-hfacC(i,j,L-1,bi,bj)*drF(L-1)).gt.0.001)then
            nlphys = nlphys + 1
            dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L-1,bi,bj)*drF(L-1)
     .                                                          - deltap
           endif
          endif

         elseif(nlphys.eq.ntry)then
c Mostly done with new layers - make sure we end at dynamics edge,
c      if not, make one more thinner (thinner than dyn grid) layer
          if(abs(deltap-hfacC(i,j,L-1,bi,bj)*drF(L-1)).gt.0.001)then
           nlphys = nlphys + 1
           dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L-1,bi,bj)*drF(L-1) 
     .                                                          - deltap
           nlphys = nlphys + 1
           dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L,bi,bj)*drF(L)
          else
           nlphys = nlphys + 1
           dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L,bi,bj)*drF(L)
          endif
         else
c we are done adding new physics layers, just copy the rest
c    of the dynamics grid onto the physics grid
          nlphys = nlphys + 1
          dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L,bi,bj)*drF(L)
         endif
         nlperdyn(i,j,L,bi,bj) = nlphys
        enddo
c
c  All done adding layers - if we need more to make numlevphys, put
c     them as thin (1 mb) layers near the top
       if(nlphys.lt.numlevphys)then
        nlevs = numlevphys-nlphys
        dpphys(i,j,nlphys,bi,bj) = dpphys(i,j,nlphys,bi,bj)-100. * nlevs
        do Lnew = nlphys+1,numlevphys
         dpphys(i,j,Lnew,bi,bj) = 100.
        enddo
        nlperdyn(i,j,Nr,bi,bj) = numlevphys
       endif
c END OF LOOP OVER GRID POINTS

       enddo
       enddo

       return
       end
