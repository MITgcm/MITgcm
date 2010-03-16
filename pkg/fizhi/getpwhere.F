C $Header: /u/gcmpack/MITgcm/pkg/fizhi/getpwhere.F,v 1.11 2010/03/16 00:19:33 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
#undef CHECK_GETPWHERE

       subroutine getpwhere(myThid,numpress,pressures,levpressures)
C***********************************************************************
C subroutine getpwhere
C
C Purpose: Approximate (!) the level at which the mid-level pressure
C          is less than (ie, above in the atmosphere) a given value.
C
C Algorithm: Assume surface pressure of 1000 mb, and the pressure
c          thicknesses set in make_phys_grid, with drF thicknesses above
C
C Need:    Information about the dynamics grid vertical spacing
C
C Input:   myThid       - process(or) number
C          numpres      - Number of pressures to process
C          pressures    - Pressure values to find levels for
C
C Output:  levpressures - Array of levels at which pressures are found
C                         These pressure levels correspond to the fizhi
C                         levels, and assume that the levels are counted
C                         from top to bottom (CRITICAL!)
C
C NOTE: The new physics levels specified here MUST correspond to the
C       physics levels specified in make_phys_grid from gridalt package.
C***********************************************************************
       implicit none
c
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "GRID.h"
#ifdef CHECK_GETPWHERE
#include "EEPARAMS.h"
#include "PARAMS.h"
#endif /* CHECK_GETPWHERE */

       integer myThid,numpress
       _RL pressures(numpress)
       integer levpressures(numpress)
c
#ifdef CHECK_GETPWHERE
       INTEGER ioUnit, upperBnd
c      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif /* CHECK_GETPWHERE */
       integer n,L,dynlev
C Code that MUST correspond to make_phys_grid in the gridalt package!
C Require 12 bottom levels (300 mb worth) for the physics,
C    Counting from bottom to top, the dp(s) are:
       integer ntry,ntry10,ntry40
       parameter (ntry40=15)
       parameter (ntry10=12)
       _RL dptry(ntry40), dptry10(ntry10), dptry40(ntry40)
       _RL dptry_pedge(ntry40+1)
#ifdef TRY_NEW_GETPWHERE
       _RL rC_dyn(Nr), dpTop
#else
       _RL rF_pmid(Nr),rF_edge(Nr+1)
#endif
       _RL pref(Nrphys)
       integer tmplev
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

C define the mid pressure for the levels that are specified - bottom 300 mb.
#ifdef TRY_NEW_GETPWHERE
       dptry_pedge(1) = rF(1)*1. _d -2
#else  /* TRY_NEW_GETPWHERE */
       dptry_pedge(1) = 1000.
#endif /* TRY_NEW_GETPWHERE */
       do L = 1,ntry
        dptry_pedge(L+1) = dptry_pedge(L) - dptry(L)
       enddo
       do L = 1,ntry
        pref(L) = (dptry_pedge(L) + dptry_pedge(L+1))/2.
       enddo
#ifdef CHECK_GETPWHERE
       ioUnit = errorMessageUnit
       WRITE(ioUnit,'(A)') '===== GETPWHERE: CHECK start ====='
       WRITE(ioUnit,'(4(A,I6),A)') '  Nr =', Nr,
     &                        ' , Nrphys=', Nrphys,
     &                        ' , ntry  =', ntry,
     &                        ' , pref(1:ntry):'
       WRITE(ioUnit,'(10F10.4)') (pref(L),L=1,ntry)
#endif /* CHECK_GETPWHERE */

#ifdef TRY_NEW_GETPWHERE
C top levels DP of 1 mb (or 0.01 mb for strat version)
       dpTop = 1. _d 0
       IF (Nr.EQ.70) dpTop = 1. _d -2
       DO L = ntry+1,Nrphys
        pref(L) = (Nrphys-L+0.5)*dpTop
       ENDDO
C define the rest of the mid pressures from the dynamics levels
       DO L = 1,Nr
        rC_dyn(L) = rC(L)*1. _d -2
       ENDDO

       dynlev = 0
       DO L = 1,Nr
         IF ( rC_dyn(L).GE.dptry_pedge(ntry+1) ) dynlev = L
       ENDDO
       DO L = ntry+1,MIN(Nrphys,ntry+Nr-dynlev)
         pref(L) = rC_dyn(dynlev+L-ntry)
       ENDDO
#else  /* TRY_NEW_GETPWHERE */
C define the rest of the mid pressures from the dynamics levels
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
#ifdef CHECK_GETPWHERE
       IF ( rF_pmid(dynlev).ge.pref(ntry)-25. ) THEN
         upperBnd = ntry+Nr-dynlev
       ELSE
         upperBnd = ntry+Nr-dynlev-1
       ENDIF
       WRITE(ioUnit,'(1(A,I5),A)') ' Up-Bnd=', upperBnd,
     &                           ' , rF_pmid:'
       WRITE(ioUnit,'(10F10.4)') rF_pmid
       IF ( upperBnd.LT.Nrphys-1 ) THEN
         WRITE(ioUnit,'(A)')
     &   'ERROR: exeeding "rF_pmid" array bounds => pref ill defined'
c        STOP 'ABNORMAL END: S/R GETPWHERE'
       ENDIF
#endif /* CHECK_GETPWHERE */
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
C Add top level DP of 1 mb - p mid is at 0.5 mb (or 0.05 mb for strat version)
       pref(Nrphys) = 0.5
       if(Nr.eq.70)pref(Nrphys) = 0.005
#endif /* TRY_NEW_GETPWHERE */

       DO n = 1,numpress
        DO L = 1,Nrphys
         IF ( pref(L).GE.pressures(n) ) tmplev = L
        ENDDO

C and now flip the level numbers for the top down counting in fizhi
        levpressures(n) = Nrphys+1-tmplev
       ENDDO

#ifdef CHECK_GETPWHERE
       WRITE(ioUnit,'(3(A,I5),A)') ' dynlev=', dynlev,
     &                           ' , numpress=', numpress,
     &                           ' , pressures:'
       WRITE(ioUnit,'(10F10.4)') pressures
       WRITE(ioUnit,'(A)') 'pref(ntry:Nrphys):'
       WRITE(ioUnit,'(10F10.4)') (pref(L),L=ntry,Nrphys)
       WRITE(ioUnit,'(A)') 'levpressures:'
       WRITE(ioUnit,'(20I5)') levpressures
       WRITE(ioUnit,'(A)') '===== GETPWHERE: CHECK end   ====='
#endif /* CHECK_GETPWHERE */

       RETURN
       END
