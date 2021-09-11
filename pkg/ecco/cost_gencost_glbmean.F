#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_glbmean(
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_gencost_glbmean
c     ==================================================================
c
c     o Evaluate cost function contribution of global mean time series
c        of OBP and SSH
c
c     started: Ou Wang Nov-2015
c
c     ==================================================================
c     SUBROUTINE cost_gencost_glbmean
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_ECCO
#ifdef ALLOW_GENCOST_CONTRIBUTION
#ifdef ALLOW_GENCOST_1D

c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer irec
      integer il

      logical doglobalread
      logical ladinit

      _RL locbar(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL locw

      _RL locbarmean ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL locbaranom ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL loccount ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL junk, junkweight

      character*(80) fname

      _RL fac
      _RL locbarglbmean
      _RL locbarglbmean_sum

      integer k, kgen
      integer locnrec
      integer ilo, ihi
      integer locunit
      _RL dataglbmean ( N1DDATA )
      _RL meandataglbmean, meandataglbmeannu
      _RL locmask ( N1DDATA )

      character*(max_len_mbuf) msgbuf

c     == external functions ==

      integer  ilnblnk, ifnblnk
      external ilnblnk, ifnblnk

      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      do k=1,NGENCOST
      kgen=0
      if (((gencost_name(k).EQ.'gmbp') .OR.
     &     (gencost_name(k).EQ.'gmsl'))
     &   .AND.( gencost_is1d(k) )
     &   .AND.(using_gencost(k)) ) kgen=k

      if (kgen.GT.0) then
      locw   = gencost_wei1d(kgen)

      if(locw .NE. 0. _d 0) then

c-- initialise local variables
      fac = 1. _d 0
c convert phibot from m2/s2 to cm
      if(gencost_name(k).EQ.'gmbp') fac = 1. _d 2 * recip_gravity
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sny
            do i = 1,snx
              locbarmean(i,j,bi,bj) = 0. _d 0
              locbaranom(i,j,bi,bj) = 0. _d 0
              loccount(i,j,bi,bj) = 0. _d 0
              locbar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      doglobalread = .false.
      ladinit      = .false.

c-- map global variable to local variables

      locnrec = gencost_nrec(kgen)

      meandataglbmean = 0. _d 0
      meandataglbmeannu = 0. _d 0
      do irec = 1, N1DDATA
       dataglbmean(irec) = 0. _d 0
       locmask(irec) = 0. _d 0
      enddo

      do irec = 1, locnrec
         dataglbmean(irec) =  gencost_1DDATA(irec,kgen)
         if ( gencost_1DDATA(irec,kgen).gt.gencost_spmin(kgen) .and.
     &        gencost_1DDATA(irec,kgen).lt.gencost_spmax(kgen) .and.
     &        gencost_1DDATA(irec,kgen).ne.gencost_spzero(kgen) ) then
            locmask(irec) = 1. _d 0
            meandataglbmean = meandataglbmean + dataglbmean(irec)
            meandataglbmeannu = meandataglbmeannu + 1. _d 0
         endif
      enddo
      if(meandataglbmeannu.NE.0. _d 0)
     &   meandataglbmean = meandataglbmean / meandataglbmeannu

C now remove the time-mean from the data
      do irec = 1, locnrec
         if(locmask(irec).EQ.1. _d 0) then
           dataglbmean(irec) = dataglbmean(irec)
     &      - meandataglbmean
         endif
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

      do irec = 1, locnrec

        if(locmask(irec) .NE. 0. _d 0) then
c--     Compute the mean over all bpdat records.
        call active_read_xy( fname, locbar, irec, doglobalread,
     &                       ladinit, eccoiter, mythid,
     &                       gencost_dummy(kgen) )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if ( maskc(i,j,1,bi,bj).NE. 0. _d 0 ) then
                  locbarmean(i,j,bi,bj) = locbarmean(i,j,bi,bj) +
     &               fac*locbar(i,j,bi,bj)
                  loccount(i,j,bi,bj) = loccount(i,j,bi,bj) + 1. _d 0
                endif
              enddo
            enddo
          enddo
        enddo
        endif

      enddo

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if (loccount(i,j,bi,bj).GT. 0. _d 0) then
                  locbarmean(i,j,bi,bj) =
     &              locbarmean(i,j,bi,bj)/loccount(i,j,bi,bj)
                endif
              enddo
            enddo
          enddo
        enddo

c--   ==========
c--   Anomalies.
c--   ==========

c--   Loop over records for the second time.
      do irec = 1, locnrec

        if(locmask(irec) .NE. 0. _d 0) then
        call active_read_xy( fname, locbar, irec, doglobalread,
     &                       ladinit, eccoiter, mythid,
     &                       gencost_dummy(kgen) )

c--    Compute field of anomalies
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
                if ( maskc(i,j,1,bi,bj).NE. 0. _d 0) then
                  locbaranom(i,j,bi,bj) =
     &              fac*locbar(i,j,bi,bj) - locbarmean(i,j,bi,bj)
                else
                  locbaranom(i,j,bi,bj) = 0. _d 0
                endif
              enddo
            enddo
          enddo
        enddo

c--    Remove global mean value
      locbarglbmean     = 0. _d 0
      locbarglbmean_sum = 0. _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sny
            do i = 1,snx
              if ( maskc(i,j,1,bi,bj).NE. 0. _d 0) then
                locbarglbmean  = locbarglbmean +
     &           RA(i,j,bi,bj)*locbaranom(i,j,bi,bj)
                locbarglbmean_sum = locbarglbmean_sum + RA(i,j,bi,bj)
              endif
            enddo
          enddo
        enddo
      enddo

      _GLOBAL_SUM_RL( locbarglbmean     , mythid )
      _GLOBAL_SUM_RL( locbarglbmean_sum , mythid )

      IF (  MASTER_CPU_THREAD(myThid) .AND.
     &     ( locmask(irec) .NE. 0. _d 0 ) .AND.
     &     ( locbarglbmean_sum .NE. 0. _d 0 ) ) THEN
          junk=locbarglbmean/locbarglbmean_sum - dataglbmean(irec)
          junkweight=locw
          objf_gencost(1,1,kgen) = objf_gencost(1,1,kgen)
     &        + junk*junk*junkweight
          num_gencost(1,1,kgen) = num_gencost(1,1,kgen)
     &        + 1. _d 0

        WRITE(msgBuf,'(A,i6,2e16.5)') gencost_name(kgen)(1:10),irec,
     &   locbarglbmean/locbarglbmean_sum, dataglbmean(irec)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      ENDIF

      endif ! if(locmask .ne. 0. _d 0)
      enddo

      endif !if(locw .NE. 0. _d 0) then
      endif !if (kgen.GT.0) then

      enddo !do k=1,NGENCOST

#endif /* ifdef ALLOW_GENCOST_1D */
#endif /* ifdef ALLOW_GENCOST_CONTRIBUTION */
#endif /* ifdef ALLOW_ECCO */

      end
