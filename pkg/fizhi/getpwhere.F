C $Header: /u/gcmpack/MITgcm/pkg/fizhi/getpwhere.F,v 1.6 2007/07/08 20:24:02 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       subroutine getpwhere(myThid,numpress,pressures,levpressures)
c***********************************************************************
c subroutine getpwhere
c 
c Purpose: Approximate (!) the level at which the mid-level pressure 
c          is less than (ie, above in the atmosphere) a given value.
c
c Algorithm: Assume surface pressure of 1000 mb, and the pressure
c          thicknesses set in make_phys_grid, with drF thicknesses above
c
c Need:    Information about the dynamics grid vertical spacing
c     
c Input:   myThid       - process(or) number
c          numpres      - Number of pressures to process
c          pressures    - Pressure values to find levels for
c
c Output:  levpressures - Array of levels at which pressures are found
c                         These pressure levels correspond to the fizhi
c                         levels, and assume that the levels are counted
c                         from top to bottom (CRITICAL!)
c
c NOTE: The new physics levels specified here MUST correspond to the
c       physics levels specified in make_phys_grid from gridalt package.
c***********************************************************************
       implicit none
c
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "GRID.h"

       integer myThid,numpress
       _RL pressures(numpress)
       integer levpressures(numpress)
c
       integer n,L,dynlev
c Code that MUST correspond to make_phys_grid in the gridalt package!
c Require 12 bottom levels (300 mb worth) for the physics, 
c    Counting from bottom to top, the dp's are:
       integer ntry,ntry10,ntry40
       parameter (ntry40=15)
       parameter (ntry10=12)
       _RL dptry(ntry40), dptry10(ntry10), dptry40(ntry40)
       _RL dptry_pedge(ntry40+1)
       _RL rF_pmid(Nr),rF_edge(Nr+1)
       _RL pref(Nrphys) 
       integer plevref(Nrphys)
       data dptry10 /3.00, 6.00,10.00,14.00,17.00,25.00,
     .              25.00,25.00,25.00,50.00,50.00,50.00/
       data dptry40 /3.00, 6.00,10.00,14.00,17.00,25.00,
     .              25.00,25.00,25.00,25.00,25.00,25.00,
     .              25.00,25.00,25.00/

       if( (Nr.eq.10).or.(Nr.eq.20) ) then
        ntry = ntry10
        do L = 1,ntry
         dptry(L) = dptry10(L)
        enddo
       elseif ((Nr.eq.40).or.(Nr.eq.70)) then
        ntry = ntry40
        do L = 1,ntry
         dptry(L) = dptry40(L)
        enddo
       else
        print *,' Dont know how to set levels for given pressures '
        stop
       endif

c define the mid pressure for the levels that are specified - bottom 300 mb.
       dptry_pedge(1) = 1000.
       do L = 2,ntry+1
        dptry_pedge(L) = dptry_pedge(L-1) - dptry(L-1)
       enddo
       do L = 1,ntry
        pref(L) = (dptry_pedge(L) + dptry_pedge(L+1))/2.
       enddo

c define the rest of the mid pressures from the dynamics levels
       rF_edge(1) = 1000.
       do L = 2,Nr+1
        rF_edge(L) = rF_edge(L-1) - (drF(L-1)/100.)
       enddo
       do L = 1,Nr
        rF_pmid(L) = (rF_edge(L) + rF_edge(L+1))/2.
       enddo

       dynlev = 0
       do L = 1,Nr
        if(rF_pmid(L).ge.pref(ntry)) dynlev = L
       enddo
       if(rF_pmid(dynlev).ge.pref(ntry)-25.) then
        do L = ntry+1,Nrphys-1
         pref(L) = rF_pmid(dynlev+L-ntry)
        enddo
       else
        pref(ntry) = rF_pmid(dynlev)
        do L = ntry+1,Nrphys-1
         pref(L) = rF_pmid(dynlev+L-ntry+1)
        enddo
       endif
c Add top level DP of 1 mb - p mid is at 0.5 mb (or 0.05 mb for strat version)
       pref(Nrphys) = 0.5
       if(Nr.eq.70)pref(Nrphys) = 0.005
c
       do n = 1,numpress
        do L = 1,Nrphys
         if(pref(L).ge.pressures(n)) plevref(n) = L
        enddo
c
c and now flip the level numbers for the top down counting in fizhi
c
        levpressures(n) = Nrphys+1-plevref(n)
       enddo

       return
       end
