C $Header: /u/gcmpack/MITgcm/pkg/gridalt/make_phys_grid.F,v 1.12 2012/08/12 01:28:03 jmc Exp $
C $Name:  $

#include "GRIDALT_OPTIONS.h"

       SUBROUTINE MAKE_PHYS_GRID(drF,hfacC,im1,im2,jm1,jm2,Nr,
     & nSx,nSy,i1,i2,j1,j2,bi,bj,Nrphys,Lbot,dpphys,numlevphys,nlperdyn)
C***********************************************************************
C SUBROUTINE MAKE_PHYS_GRID
C
C Purpose: Define the grid that the will be used to run the high-end
C          atmospheric physics.
C
C Algorithm: Fit additional levels of some (~) known thickness in
C          between existing levels of the grid used for the dynamics
C
C Need:    Information about the dynamics grid vertical spacing
C
C Input:   drF         - delta r (p*) edge-to-edge
C          hfacC       - fraction of grid box above topography
C          im1, im2    - beginning and ending i - dimensions
C          jm1, jm2    - beginning and ending j - dimensions
C          Nr          - number of levels in dynamics grid
C          nSx,nSy     - number of processes in x and y direction
C          i1, i2      - beginning and ending i - index to fill
C          j1, j2      - beginning and ending j - index to fill
C          bi, bj      - x-dir and y-dir index of process
C          Nrphys      - number of levels in physics grid
C
C Output:  dpphys      - delta r (p*) edge-to-edge of physics grid
C          numlevphys  - number of levels used in the physics
C          nlperdyn    - physics level number atop each dynamics layer
C
C NOTES: 1) Pressure levs are built up from bottom, using p0, ps and dp:
C              p(i,j,k)=p(i,j,k-1) + dp(k)*ps(i,j)/p0(i,j)
C        2) Output dp(s) are aligned to fit EXACTLY between existing
C           levels of the dynamics vertical grid
C        3) IMPORTANT! This routine assumes the levels are numbered
C           from the bottom up, ie, level 1 is the surface.
C           IT WILL NOT WORK OTHERWISE!!!
C        4) This routine does NOT work for surface pressures less
C           (ie, above in the atmosphere) than about 350 mb
C***********************************************************************

       IMPLICIT NONE

       integer im1,im2,jm1,jm2,Nr,nSx,nSy,Nrphys
       integer i1,i2,j1,j2,bi,bj
       integer numlevphys
       _RS hfacC(im1:im2,jm1:jm2,Nr,nSx,nSy)
       _RL dpphys(im1:im2,jm1:jm2,Nrphys,nSx,nSy)
       _RS drF(Nr)
       integer Lbot(im1:im2,jm1:jm2,nSx,nSy)
       integer nlperdyn(im1:im2,jm1:jm2,Nr,nSx,nSy)

       integer i,j,L,Lbotij,Lnew
C Require 12 (or 15) levels near the surface (300 mb worth) for fizhi.
C   the dp(s) are in the dptry arrays:
       integer ntry,ntry10,ntry40
       parameter (ntry10 = 12)
       parameter (ntry40 = 12)
       _RL dptry(15),dptry10(ntry10),dptry40(ntry40)
       _RL bot_thick,bot_thick40
       _RL dptry_accum(15)
       data dptry10/300.000,600.000,1000.000,1400.000,1700.000,2500.000,
     &            2500.000,2500.000,2500.000,5000.000,5000.000,5000.000/
       data dptry40/300.000,600.000, 800.000, 800.000,1250.000,
     &            1250.000,2500.000,2500.000,2500.000,2500.000,2500.000,
     &            2500.000/
       data bot_thick40/20000.000/
       _RL deltap, dpstar_accum
       integer nlbotmax, nstart, nlevs, nlphys, ndone
       _RL thindp

       if( (Nr.eq.10) .or. (Nr.eq.20) ) then
        ntry = ntry10
        bot_thick = bot_thick40
        do L = 1,ntry
         dptry(L) = dptry10(L)
        enddo
       elseif((Nr.eq.40).or.(Nr.eq.46).or.(Nr.eq.70)) then
        ntry = ntry40
        bot_thick = bot_thick40
        do L = 1,ntry
         dptry(L) = dptry40(L)
        enddo
       else
        print *,' Dont know how to make fizhi grid '
        stop
       endif

       thindp=100.
       if(Nr.eq.70)thindp=0.02

       do L = 1,Nr
        do j = j1,j2
        do i = i1,i2+1
         nlperdyn(i,j,L,bi,bj) = 0
        enddo
        enddo
       enddo

C Figure out how many physics levels there will be
C   (need 12 between sfc and 300 mb above it - see how many
C    there are in the dynamics if the surface pressure is at
C    the sum of drF, ie, the maximum dynamics grid layers possible)
       nlevs = 0
       dpstar_accum = 0.
       do L = 1,Nr
        dpstar_accum = dpstar_accum + drF(L)
        if(dpstar_accum.le.bot_thick) nlevs = nlevs+1
       enddo
       numlevphys = Nr - nlevs + ntry + 1

       dptry_accum(1) = dptry(1)
       do Lnew = 2,ntry
        dptry_accum(Lnew) = dptry_accum(Lnew-1) + dptry(Lnew)
       enddo

C      do for each grid point:
       do j = j1,j2
       do i = i1,i2
        Lbotij = Lbot(i,j,bi,bj)

C Find the maximum number of physics levels to fit in the bottom level

        nlbotmax = 0
        do Lnew = 1,ntry
         if ( (nlbotmax.eq.0) .and.
     & (dptry_accum(Lnew).gt.(hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij))))then
          nlbotmax = Lnew
         endif
        enddo
        if(nlbotmax.eq.0)then
         nlbotmax = ntry
        endif

C See how many of the physics levs can fit in the bottom level

        nlphys = 0
        deltap = 0.
        do Lnew = 1,nlbotmax
C Test to see if the next physics level fits, if yes, add it
         if((hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij)).ge.
     &                                           deltap+dptry(Lnew))then
          nlphys = nlphys + 1
          dpphys(i,j,nlphys,bi,bj) = dptry(Lnew)
          deltap = deltap + dptry(Lnew)
         else
C If the level does not fit, decide whether to make a new thinner
C  one or make the one below a bit thicker
          if((dptry(Lnew-1)+(hfacC(i,j,Lbotij,bi,bj)*
     &             drF(Lbotij)-deltap)) .gt. (dptry(Lnew-1)*1.5) ) then
C Add a new thin layer
           nlphys = nlphys + 1
           dpphys(i,j,nlphys,bi,bj) =
     &                      (hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij))-deltap
          else
C Make the one below thicker
           dpphys(i,j,nlphys,bi,bj) = dpphys(i,j,nlphys,bi,bj) +
     &                      (hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij)-deltap)
          endif
          deltap = deltap+(hfacC(i,j,Lbotij,bi,bj)*drF(Lbotij)-deltap)
         endif
        enddo

        nlperdyn(i,j,Lbotij,bi,bj) = nlphys

C Now proceed upwards - see how many physics levels fit in each
C   subsequent dynamics level - go through all 12 required levels

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
C If the level does not fit, decide whether to make a new thinner
C  one or make the one below a bit thicker
            ndone = 1
            if( (dptry(Lnew-1)+(hfacC(i,j,L,bi,bj)*drF(L)-deltap))
     &                    .gt. (dptry(Lnew-1)*1.5) ) then
C Add a new thin layer
             nlphys = nlphys + 1
             dpphys(i,j,nlphys,bi,bj) =
     &                             (hfacC(i,j,L,bi,bj)*drF(L))-deltap
             deltap = hfacC(i,j,L,bi,bj)*drF(L)
            else
C Make the one below thicker
             dpphys(i,j,nlphys,bi,bj) = dpphys(i,j,nlphys,bi,bj) +
     &                      (hfacC(i,j,L,bi,bj)*drF(L)-deltap)
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
     &                                                          - deltap
           endif
          endif

         elseif(nlphys.eq.ntry)then
C Mostly done with new layers - make sure we end at dynamics edge,
C      if not, make one more thinner (thinner than dyn grid) layer
          if(abs(deltap-hfacC(i,j,L-1,bi,bj)*drF(L-1)).gt.0.001)then
           nlphys = nlphys + 1
           dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L-1,bi,bj)*drF(L-1)
     &                                                          - deltap
           nlphys = nlphys + 1
           dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L,bi,bj)*drF(L)
          else
           nlphys = nlphys + 1
           dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L,bi,bj)*drF(L)
          endif
         else
C we are done adding new physics layers, just copy the rest
C    of the dynamics grid onto the physics grid
          nlphys = nlphys + 1
          dpphys(i,j,nlphys,bi,bj) = hfacC(i,j,L,bi,bj)*drF(L)
         endif
         nlperdyn(i,j,L,bi,bj) = nlphys
        enddo

C  All done adding layers - if we need more to make numlevphys, put
C     them as thin (1 mb) layers near the top
       if(nlphys.lt.numlevphys)then
        nlevs = numlevphys-nlphys
        dpphys(i,j,nlphys,bi,bj)=dpphys(i,j,nlphys,bi,bj)-thindp*nlevs
        do Lnew = nlphys+1,numlevphys
         dpphys(i,j,Lnew,bi,bj) = thindp
        enddo
        nlperdyn(i,j,Nr,bi,bj) = numlevphys
       endif
C END OF LOOP OVER GRID POINTS

       enddo
       enddo

       return
       end
