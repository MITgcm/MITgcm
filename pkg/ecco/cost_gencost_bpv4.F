C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_bpv4.F,v 1.3 2014/11/02 22:31:04 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


      subroutine cost_gencost_bpv4(
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_gencost_bpv4
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
#include "DYNVARS.h"

#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_ECCO
#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer irec
      integer il

      logical doglobalread
      logical ladinit

      _RL locbpbar(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL locbpdat(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL locbpmask(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL locwbp(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL bpdifmean ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL bpdifanom ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL bpdatmean ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL bpdatanom ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL bpcount ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL junk

      character*(80) fname
      character*(80) fname4test

      _RL fac
      _RL offset
      _RL offset_sum

      integer k, kgen

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)


      kgen=0
      do k=1,NGENCOST
      if ( (gencost_barfile(k)(1:2).EQ.'bp').AND.
     &     (using_gencost(k)) ) kgen=k
      enddo

      if (kgen.GT.0) then

c-- initialise local variables
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
              locwbp(i,j,bi,bj) = 0. _d 0
              locbpbar(i,j,bi,bj) = 0. _d 0
              locbpdat(i,j,bi,bj) = 0. _d 0
              locbpmask(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      doglobalread = .false.
      ladinit      = .false.

c-- map global variable to local variables

      do bj = jtlo,jthi
       do bi = itlo,ithi
        do j = 1,sny
         do i = 1,snx
           locwbp(i,j,bi,bj) = gencost_weight(i,j,bi,bj,kgen)
         enddo
        enddo
       enddo
      enddo

c--

#ifdef ALLOW_CTRL
      write(fname(1:80),'(80a)') ' '
      il=ilnblnk( gencost_barfile(kgen) )
      write(fname(1:80),'(2a,i10.10)')
     &     gencost_barfile(kgen)(1:il),'.',eccoiter
#endif

c--   ============
c--   Mean values.
c--   ============

      do irec = 1, nmonsrec

c--     Compute the mean over all bpdat records.
        call active_read_xy( fname, locbpbar, irec, doglobalread,
     &                       ladinit, eccoiter, mythid,
     &                       gencost_dummy(kgen) )

        call cost_bp_read( gencost_datafile(kgen), 
     &       gencost_startdate(1,kgen),
     &       locbpdat, locbpmask, irec, mythid )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if ( (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &               (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                  bpdifmean(i,j,bi,bj) = bpdifmean(i,j,bi,bj) +
     &              ( fac*locbpbar(i,j,bi,bj) - locbpdat(i,j,bi,bj) )
                  bpdatmean(i,j,bi,bj) = bpdatmean(i,j,bi,bj) +
     &              locbpdat(i,j,bi,bj)
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

        call active_read_xy( fname, locbpbar, irec, doglobalread,
     &                       ladinit, eccoiter, mythid,
     &                       gencost_dummy(kgen) )

        call cost_bp_read( gencost_datafile(kgen), 
     &       gencost_startdate(1,kgen),
     &       locbpdat, locbpmask, irec, mythid )

c--    Compute field of anomalies
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if ( (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &               (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                  bpdifanom(i,j,bi,bj) =
     &              ( fac*locbpbar(i,j,bi,bj) - locbpdat(i,j,bi,bj) )
     &              - bpdifmean(i,j,bi,bj)
                  bpdatanom(i,j,bi,bj) =
     &              locbpdat(i,j,bi,bj) - bpdatmean(i,j,bi,bj)
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
              if ( (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
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
     &              (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &              (maskc(i,j,1,bi,bj).NE. 0. _d 0)) then
                  bpdifanom(i,j,bi,bj) = bpdifanom(i,j,bi,bj)
     &                                 - offset/offset_sum
                endif
              enddo
            enddo
          enddo
        enddo

c--    Smooth field of anomalies
      if (gencost_outputlevel(kgen).GT.0) then
      write(fname4test(1:80),'(1a)') 'bpdifanom_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,bpdifanom,irec,1,mythid)
      write(fname4test(1:80),'(1a)') 'bpdatanom_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,bpdatanom,irec,1,mythid)
      endif

#ifdef ALLOW_SMOOTH
      if ( useSMOOTH )
     &  call smooth_basic2D(bpdifanom,maskc,750000. _d 0,5000,mythid)
#endif

      if (gencost_outputlevel(kgen).GT.0) then
#ifdef ALLOW_SMOOTH
      if ( useSMOOTH ) 
     &  call smooth_basic2D(bpdatanom,maskc,750000. _d 0,5000,mythid)
#endif

      write(fname4test(1:80),'(1a)') 'bpdifanom_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,bpdifanom,irec,1,mythid)
      write(fname4test(1:80),'(1a)') 'bpdatanom_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,bpdatanom,irec,1,mythid)
      endif

c--    Compute cost function
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
c-- map to global cost variables
                if ( (locwbp(i,j,bi,bj).NE. 0. _d 0).AND.
     &               (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &               (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                   junk = bpdifanom(i,j,bi,bj)
                   objf_gencost(kgen,bi,bj) = objf_gencost(kgen,bi,bj)
     &               + junk*junk*locwbp(i,j,bi,bj)
                   num_gencost(kgen,bi,bj) = num_gencost(kgen,bi,bj)
     &               + 1. _d 0
                endif
              enddo
            enddo
          enddo
        enddo

      enddo

      endif !if (kgen.GT.0) then

#endif /* ifdef ALLOW_GENCOST_CONTRIBUTION */
#endif /* ifdef ALLOW_ECCO */

      end
