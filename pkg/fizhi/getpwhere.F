C $Header: /u/gcmpack/MITgcm/pkg/fizhi/getpwhere.F,v 1.2 2004/07/16 16:11:36 molod Exp $
C $Name:  $

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
#include "CPP_OPTIONS.h"
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
       real dptry(ntry), dptry_pedge(ntry+1)
       real drF_pedge(Nr+1),drF_pmid(Nr),drF_accum(Nr)
       real pref(Nrphys) 
       integer plevref(Nrphys)
       data dptry /300.000, 600.000,1000.000,1400.000,1700.000,2500.000,
     .            2500.000,2500.000,2500.000,5000.000,5000.000,5000.000/
c
c define the mid pressure for the levels that are specified - bottom 300 mb.
       dptry_pedge(1) = 100000.
       do L = 2,ntry+1
        dptry_pedge(L) = dptry_pedge(L-1) - dptry(L)
       enddo
       do L = 1,ntry
        pref(L) = (dptry_pedge(L) + dptry_pedge(L+1))/2.
       enddo

c define the rest of the mid pressures from the dynamics levels
       drF_accum(1) = drF(1)
       do L = 2,Nr
        drF_accum(L) = drF_accum(L-1) + drF(L)
       enddo
       do L = 1,Nr
        drF_pmid(L) = (drF_pedge(L) + drF_pedge(L+1))/2.
       enddo

       do L = 1,Nr
        if(drF_pmid(L).ge.pref(ntry)) dynlev = L
       enddo
       if(drF_pmid(dynlev).ge.pref(ntry)-2500.) then
        do L = ntry+1,Nrphys
         pref(L) = drF_pmid(dynlev+L-ntry)
        enddo
       else
        pref(ntry) = drF_pmid(dynlev)
        do L = ntry+1,Nrphys
         pref(L) = drF_pmid(dynlev+L-ntry+1)
        enddo
       endif
c
       do n = 1,numpress
        do L = 1,Nrphys
         if(pref(L).ge.pressures(n)) plevref(n) = L
        enddo
c
c and now flip the level numbers for the top down counting in fizhi
c
        do L = 1,Nrphys
         levpressures(n) = Nrphys+1-plevref(n)
        enddo
       enddo

       return
       end
