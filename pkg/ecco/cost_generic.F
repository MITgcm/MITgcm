C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_generic.F,v 1.17 2012/08/10 14:34:41 gforget Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_generic(
     &     nnzbar, localbarfile, localbar, xx_localbar_mean_dummy,
     &     nnzobs, localobsfile, mult_local,
     &     nrecloc, localstartdate, localperiod,
     &     ylocmask, localweight,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_local, num_local,
     &     myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_generic
c     ==================================================================
c
c     o Generic routine for evaluating time-dependent
c       cost function contribution
c
c     ==================================================================
c     SUBROUTINE cost_generic
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_CTRL
# include "ecco_cost.h"
# include "optim.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==

      integer nnzbar
      integer nnzobs
      integer nrecloc
      integer myiter
      integer mythid
      integer localstartdate(4)

      _RL localbar   (1-olx:snx+olx,1-oly:sny+oly,nnzbar,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL xx_localbar_mean_dummy
      _RL mult_local
      _RL mytime
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL objf_local(nsx,nsy)
      _RL num_local(nsx,nsy)

      character*(1) ylocmask
      character*(MAX_LEN_FNAM) localbarfile
      character*(MAX_LEN_FNAM) localobsfile

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer  il
      integer localrec
      integer obsrec

      logical doglobalread
      logical ladinit

      _RL spval
      parameter (spval = -9999. )
      _RL localwww
      _RL localcost
      _RL junk

      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL cmask (1-olx:snx+olx,1-oly:sny+oly,nnzobs)

      character*(128) fname1, fname2
      character*(MAX_LEN_MBUF) msgbuf

cnew(
      _RL daytime
      _RL diffsecs
      integer dayiter
      integer daydate(4)
      integer difftime(4)
      integer tempDate_1
      integer middate(4)
      integer yday, ymod
      integer md, dd, sd, ld, wd
      integer mody, modm
      integer beginmodel, beginlocal
      logical exst
cnew)

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

      localwww = 0. _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_local(bi,bj) = 0. _d 0
          num_local(bi,bj) = 0. _d 0
          do k = 1,nnzobs
            do j = jmin,jmax
              do i = imin,imax
                localobs(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

c--   Assign mask
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
            do j = 1-oly,sny+oly
              do i = 1-olx,snx+olx
      if ( ylocmask .EQ. 'C' .OR. ylocmask .EQ. 'c' ) then
         localmask(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
      elseif ( ylocmask .EQ. 'S' .OR. ylocmask .EQ. 's' ) then
         localmask(i,j,k,bi,bj) = maskS(i,j,k,bi,bj)
      elseif ( ylocmask .EQ. 'W' .OR. ylocmask .EQ. 'w' ) then
         localmask(i,j,k,bi,bj) = maskW(i,j,k,bi,bj)
      else
         STOP 'cost_generic: wrong ylocmask'
      endif
              enddo
            enddo
          enddo
        enddo
      enddo

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      write(fname1(1:128),'(80a)') ' '
      il=ilnblnk( localbarfile )
      write(fname1(1:128),'(2a,i10.10)')
     &     localbarfile(1:il),'.',optimcycle

cph      if ( .NOT. ( mult_local.EQ.0. .OR. localobsfile.EQ.' ' ) ) then
      if ( .NOT. ( localobsfile.EQ.' ' ) ) then

c--   Loop over records for the second time.
      do irec = 1, nrecloc

        if ( nnzbar .EQ. 1 ) then
           call active_read_xy( fname1, localbar, irec, doglobalread,
     &                      ladinit, optimcycle, mythid,
     &                      xx_localbar_mean_dummy )
        else
           call active_read_xyz( fname1, localbar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_localbar_mean_dummy )
        endif

cnew(
        if ( localperiod .EQ. 86400. ) then
c-- assume daily fields
           obsrec = irec
           daytime = FLOAT(secondsperday*(irec-1)) + modelstart
           dayiter = hoursperday*(irec-1) + modeliter0
           call cal_getdate( dayiter, daytime, daydate, mythid )
           call cal_convdate( daydate,yday,md,dd,sd,ld,wd,mythid )
           ymod = localstartdate(1)/10000
           do k=1,4
              middate(k)=0
           enddo
           tempDate_1 = yday*10000+100+1
           if ( ymod .GE. yday ) then
              call cal_FullDate( localstartdate(1), 0, middate, mythid)
           else
              call cal_FullDate( tempDate_1, 0, middate, mythid)
           endif
           call cal_TimePassed( middate, daydate, difftime, mythid )
           call cal_ToSeconds( difftime, diffsecs, mythid )
c           localrec = floor(diffsecs/localperiod) + 1
           localrec = int(diffsecs/localperiod) + 1
        else
c-- assume monthly fields
           beginlocal = localstartdate(1)/10000
           beginmodel = modelstartdate(1)/10000
           obsrec =
     &           ( beginmodel - beginlocal )*nmonthyear
     &         + ( mod(modelstartdate(1)/100,100)
     &            -mod(localstartdate(1)/100,100) )
     &         + irec
           mody   = modelstartdate(1)/10000
           modm   = modelstartdate(1)/100 - mody*100
           yday   = mody + INT((modm-1+irec-1)/12)
           localrec = 1 + MOD(modm-1+irec-1,12)
        endif

        il=ilnblnk(localobsfile)
        write(fname2(1:128),'(2a,i4)')
     &       localobsfile(1:il), '_', yday
        inquire( file=fname2, exist=exst )
        if ( (.NOT. exst).AND.( localperiod .NE. 86400. ) ) then
           write(fname2(1:128),'(a)') localobsfile(1:il)
           inquire( file=fname2, exist=exst )
#ifndef COST_GENERIC_ASSUME_CYCLIC
c assume we have one big file, one year after the other
           localrec = obsrec
c otherwise assume climatology, used for each year
#endif
        endif

        if ( (localrec .GT. 0).AND.(obsrec .GT. 0).AND.(exst) ) then
          call mdsreadfield( fname2, cost_iprec, cost_yftype, nnzobs,
     &         localobs, localrec, mythid )
        else
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nnzobs
                do j = jmin,jmax
                  do i = imin,imax
                     localobs(i,j,k,bi,bj) = spval
                  enddo
                enddo
              enddo
            enddo
          enddo
        endif
cnew)

        do bj = jtlo,jthi
          do bi = itlo,ithi

            localcost    = 0. _d 0

c--           Determine the mask on weights
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
               cmask(i,j,k) = cosphi(i,j,bi,bj)*localmask(i,j,k,bi,bj)
                if ( localobs(i,j,k,bi,bj) .lt. spminloc .or.
     &               localobs(i,j,k,bi,bj) .gt. spmaxloc .or.
     &               localobs(i,j,k,bi,bj) .eq. spzeroloc ) then
                   cmask(i,j,k) = 0. _d 0
                endif
              enddo
             enddo
            enddo
c--
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
                localwww  = localweight(i,j,k,bi,bj)*cmask(i,j,k)
                junk      = ( localbar(i,j,k,bi,bj) -
     &                        localobs(i,j,k,bi,bj) )
                localcost = localcost + junk*junk*localwww
                if ( localwww .ne. 0. )
     &               num_local(bi,bj) = num_local(bi,bj) + 1. _d 0
              enddo
             enddo
            enddo

            objf_local(bi,bj) = objf_local(bi,bj) + localcost

          enddo
        enddo

      enddo
c--   End of second loop over records.

c--   End of mult_local or localobsfile
      endif

      end
