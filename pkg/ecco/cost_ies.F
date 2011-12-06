C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_ies.F,v 1.1 2011/12/06 04:26:26 mmazloff Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ies(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_ies
c     ==================================================================
c
c     o Evaluate cost function contribution of invertted echo sounders
c       => uses travel time (daily average) 
c
c     started: Matt Mazloff May-2010
c
c     ==================================================================
c     SUBROUTINE cost_ies
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ecco_cost.h"
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

#ifdef ALLOW_IESTAU_COST_CONTRIBUTION

c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer ilps

      logical doglobalread
      logical ladinit

      _RL iesmean ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL datmean ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL iescount ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL junk,junkweight

      character*(80) fname
      character*(80) fname4test
      character*(MAX_LEN_MBUF) msgbuf

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Initialise local variables.

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              iesmean(i,j,bi,bj) = 0. _d 0
              datmean(i,j,bi,bj) = 0. _d 0
              iescount(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      doglobalread = .false.
      ladinit      = .false.

      write(fname(1:80),'(80a)') ' '
      ilps=ilnblnk( iestaubarfile )
      write(fname(1:80),'(2a,i10.10)')
     &     iestaubarfile(1:ilps),'.',optimcycle

c--   ============
c--   Mean values.
c--   ============

      do irec = 1,ndaysrec

c--     Compute the mean over all iesdat records.
        call active_read_xy( fname, iestaubar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_iestaubar_mean_dummy )

        call cost_ies_read( irec, mythid )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
      if (iesmask(i,j,bi,bj).NE.0.) then
                iesmean(i,j,bi,bj) = iesmean(i,j,bi,bj) +
     &              iestaubar(i,j,bi,bj) 
                datmean(i,j,bi,bj) = datmean(i,j,bi,bj) +
     &                iesdat(i,j,bi,bj) 
                iescount(i,j,bi,bj) = iescount(i,j,bi,bj) +1.
      endif
              enddo
            enddo
          enddo
        enddo
      enddo

CMM done accumulating -- now average
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
      if (iescount(i,j,bi,bj).GT.0.) then
       iesmean(i,j,bi,bj) = iesmean(i,j,bi,bj)/iescount(i,j,bi,bj)
       datmean(i,j,bi,bj) = datmean(i,j,bi,bj)/iescount(i,j,bi,bj)
CMM(
c      print*,'CMM:IES DEBUG: i,j,iescount = ',i,j,iescount(i,j,bi,bj)
CMM)
      endif
              enddo
            enddo
          enddo
        enddo

CMM( output means
c      CALL WRITE_FLD_XY_RL( 'DiagnosIESmean', ' ', iesmean,
c     &                           optimcycle, mythid )
c      CALL WRITE_FLD_XY_RL( 'DiagIESobsMean', ' ', datmean,
c     &                           optimcycle, mythid )
CMM( DEBUG STUFF
c      CALL WRITE_FLD_XY_RL( 'DiagnosIEScount', ' ', iescount,
c     &                           optimcycle, mythid )
c
c      CALL WRITE_FLD_XY_RL( 'DiaIESwght', ' ', wies,
c     &                           optimcycle, mythid )

CMM)

c--   ==========
c--   Cost
c--   ==========

c--   Loop over records for the second time.
      do irec = 1, ndaysrec

        call active_read_xy( fname, iestaubar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_iestaubar_mean_dummy )

        call cost_ies_read( irec, mythid )

c--    Compute cost function
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                junkweight = wies(i,j,bi,bj)*iesmask(i,j,bi,bj)
                junk       = (iestaubar(i,j,bi,bj) - iesmean(i,j,bi,bj))
     &                      -(iesdat(i,j,bi,bj) - datmean(i,j,bi,bj))
                objf_ies(bi,bj) = objf_ies(bi,bj)
     &              + junk*junk*junkweight
                if ( junkweight .ne. 0. )
     &               num_ies(bi,bj) = num_ies(bi,bj) + 1. _d 0
C  for now dont penalize mean misfit.....depths likely different
C and would need offset
CMM(
c                if ( iescount(i,j,bi,bj) .ne. 0. ) then
c      print*,'CMM:IESdbg1: i,j,irec,junkweight= ',i,j,irec,junkweight
c      print*,'CMM:IESdbg2: wies,iesmask= '
c     &  ,wies(i,j,bi,bj),iesmask(i,j,bi,bj)
c                endif
CMM)
              enddo
            enddo
          enddo
        enddo

      enddo

#endif

      end
