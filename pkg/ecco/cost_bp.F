C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_bp.F,v 1.5 2012/08/06 20:41:54 heimbach Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_bp(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_bp
c     ==================================================================
c
c     o Evaluate cost function contribution of bottom pressure anoamlies 
c       => GRACE data
c
c     started: Gael Forget Oct-2009
c
c     ==================================================================
c     SUBROUTINE cost_bp
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ecco_cost.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#include "DYNVARS.h"
#ifdef ALLOW_PROFILES
#include "profiles.h"
#endif

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#ifdef ALLOW_BP_COST_CONTRIBUTION
#ifdef ALLOW_SMOOTH

c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer irec
      integer ilps

      logical doglobalread
      logical ladinit

      _RL bpdifmean ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL bpdifanom ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL bpdatmean ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL bpdatanom ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL bpcount ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL junk,junkweight

      character*(80) fname
      character*(80) fname4test
      character*(MAX_LEN_MBUF) msgbuf
      _RL fac

      _RL offset
      _RL offset_sum

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

c--   Initialise local variables.
cgf convert phibot from m2/s2 to cm
      fac = 1. _d 2 / 9.81 _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sny
            do i = 1,snx
              bpdifmean(i,j,bi,bj) = 0. _d 0
              bpdifanom(i,j,bi,bj) = 0. _d 0
              bpdatmean(i,j,bi,bj) = 0. _d 0
              bpdatanom(i,j,bi,bj) = 0. _d 0
              bpcount(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      doglobalread = .false.
      ladinit      = .false.

      write(fname(1:80),'(80a)') ' '
      ilps=ilnblnk( bpbarfile )
      write(fname(1:80),'(2a,i10.10)')
     &     bpbarfile(1:ilps),'.',optimcycle

c--   ============
c--   Mean values.
c--   ============

      do irec = 1, nmonsrec

c--     Compute the mean over all bpdat records.
        call active_read_xy( fname, bpbar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_bpbar_mean_dummy )

        call cost_bp_read( irec, mythid )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if ( (bpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &               (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                  bpdifmean(i,j,bi,bj) = bpdifmean(i,j,bi,bj) +
     &              ( fac*bpbar(i,j,bi,bj) - bpdat(i,j,bi,bj) )
                  bpdatmean(i,j,bi,bj) = bpdatmean(i,j,bi,bj) +
     &              bpdat(i,j,bi,bj) 
                  bpcount(i,j,bi,bj) = bpcount(i,j,bi,bj) + 1. _d 0
                endif
              enddo
            enddo
          enddo
        enddo

      enddo

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if (bpcount(i,j,bi,bj).GT. 0. _d 0) then
                  bpdifmean(i,j,bi,bj) = 
     &              bpdifmean(i,j,bi,bj)/bpcount(i,j,bi,bj)
                  bpdatmean(i,j,bi,bj) = 
     &              bpdatmean(i,j,bi,bj)/bpcount(i,j,bi,bj)
                endif
              enddo
            enddo
          enddo
        enddo

c--   ==========
c--   Anomalies.
c--   ==========

c--   Loop over records for the second time.
      do irec = 1, nmonsrec

        call active_read_xy( fname, bpbar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_bpbar_mean_dummy )

        call cost_bp_read( irec, mythid )

c--    Compute field of anomalies
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if ( (bpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &               (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                  bpdifanom(i,j,bi,bj) = 
     &              ( fac*bpbar(i,j,bi,bj) - bpdat(i,j,bi,bj) ) 
     &              - bpdifmean(i,j,bi,bj)
                  bpdatanom(i,j,bi,bj) =
     &              bpdat(i,j,bi,bj) - bpdatmean(i,j,bi,bj)
                else
                  bpdifanom(i,j,bi,bj) = 0. _d 0
                  bpdatanom(i,j,bi,bj) = 0. _d 0
                endif
              enddo
            enddo
          enddo
        enddo

c--    Remove global mean value
      offset     = 0. _d 0
      offset_sum = 0. _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sny
            do i = 1,snx
              if ( (bpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &             (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                offset  = offset + RA(i,j,bi,bj)*bpdifanom(i,j,bi,bj)
                offset_sum = offset_sum + RA(i,j,bi,bj)
              endif
            enddo
          enddo
        enddo
      enddo

      _GLOBAL_SUM_RL( offset     , mythid )
      _GLOBAL_SUM_RL( offset_sum , mythid )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if ((offset_sum.GT. 0. _d 0).AND.
     &              (bpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &              (maskc(i,j,1,bi,bj).NE. 0. _d 0)) then
                  bpdifanom(i,j,bi,bj) = bpdifanom(i,j,bi,bj)
     &                                 - offset/offset_sum
                endif
              enddo
            enddo
          enddo
        enddo

c--    Smooth field of anomalies
#ifdef ALLOW_BP_COST_OUTPUT
      write(fname4test(1:80),'(1a)') 'bpdifanom_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,bpdifanom,irec,1,mythid)
      write(fname4test(1:80),'(1a)') 'bpdatanom_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,bpdatanom,irec,1,mythid)
#endif

      call smooth_basic2D(bpdifanom,maskc,750000. _d 0,5000,mythid)

#ifdef ALLOW_BP_COST_OUTPUT
      call smooth_basic2D(bpdatanom,maskc,750000. _d 0,5000,mythid)

      write(fname4test(1:80),'(1a)') 'bpdifanom_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,bpdifanom,irec,1,mythid)
      write(fname4test(1:80),'(1a)') 'bpdatanom_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,bpdatanom,irec,1,mythid)
#endif

c--    Compute cost function
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if ( (wbp(i,j,bi,bj).NE. 0. _d 0).AND.
     &               (bpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &               (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                  junk = bpdifanom(i,j,bi,bj)
                  objf_bp(bi,bj) = objf_bp(bi,bj)
     &              + junk*junk*wbp(i,j,bi,bj)
                  num_bp(bi,bj) = num_bp(bi,bj) + 1. _d 0
                endif
              enddo
            enddo
          enddo
        enddo

      enddo

#endif /* ifdef ALLOW_SMOOTH */
#endif /* ifdef ALLOW_BP_COST_CONTRIBUTION */

      end
