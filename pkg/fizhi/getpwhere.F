C $Header: /u/gcmpack/MITgcm/pkg/fizhi/getpwhere.F,v 1.4 2004/07/27 04:10:57 molod Exp $
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
       integer ntry
       parameter (ntry=12)
       _RL dptry(ntry), dptry_pedge(ntry+1)
       _RL drF_pedge(Nr+1),drF_pmid(Nr),drF_accum(Nr)
       _RL pref(Nrphys) 
       integer plevref(Nrphys)
       data dptry /300.000, 600.000,1000.000,1400.000,1700.000,2500.000,
     .            2500.000,2500.000,2500.000,5000.000,5000.000,5000.000/
c
c define the mid pressure for the levels that are specified - bottom 300 mb.
c      dptry_pedge(1) = 100000.
c      do L = 2,ntry+1
c       dptry_pedge(L) = dptry_pedge(L-1) - dptry(L)
c      enddo
c      do L = 1,ntry
c       pref(L) = (dptry_pedge(L) + dptry_pedge(L+1))/2.
c      enddo

c define the rest of the mid pressures from the dynamics levels
c      drF_accum(1) = drF(1)
c      do L = 2,Nr
c       drF_accum(L) = drF_accum(L-1) + drF(L)
c      enddo
c      do L = 1,Nr
c       drF_pmid(L) = (drF_pedge(L) + drF_pedge(L+1))/2.
c      enddo

c      do L = 1,Nr
c       if(drF_pmid(L).ge.pref(ntry)) dynlev = L
c      enddo
c      if(drF_pmid(dynlev).ge.pref(ntry)-2500.) then
c       do L = ntry+1,Nrphys
c        pref(L) = drF_pmid(dynlev+L-ntry)
c       enddo
c      else
c       pref(ntry) = drF_pmid(dynlev)
c       do L = ntry+1,Nrphys
c        pref(L) = drF_pmid(dynlev+L-ntry+1)
c       enddo
c      endif
c
c      do n = 1,numpress
c       do L = 1,Nrphys
c        if(pref(L).ge.pressures(n)) plevref(n) = L
c       enddo
c
c and now flip the level numbers for the top down counting in fizhi
c
c       do L = 1,Nrphys
c        levpressures(n) = Nrphys+1-plevref(n)
c       enddo
c      enddo

       levpressures(1) = 16
       levpressures(2) = 7
       levpressures(3) = 6
       levpressures(4) = 3
       levpressures(5) = 1

       return
       end
