C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_sstv4.F,v 1.7 2012/08/10 19:45:26 jmc Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


      subroutine cost_gencost_sstv4(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_gencost_sstv4
c     ==================================================================
c
c     o Evaluate cost function contributions of sea surface temperature.
c       (Daily Pointwise and then Large Scale)
c
c       started: Gael Forget, Oct-2009
c
c     ==================================================================
c     SUBROUTINE cost_gencost_sstv4
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
#include "cal.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#ifdef ALLOW_DAILYSST_COST_CONTRIBUTION
#ifdef ALLOW_SMOOTH
#ifdef ALLOW_GENCOST_CONTRIBUTION
#ifdef ALLOW_GENCOST_FREEFORM
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec,jrec,krec
      integer ilps
      integer gwunit

      logical doglobalread
      logical ladinit

      _RL anom_sst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL obs_sst (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL nb_sst (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL msk_sst (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL tmp_sst (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL spval

      integer num_var

      _RL junk,junkweight

      integer ndaysave
      _RL ndaysaveRL

      character*(80) fname
      character*(80) fname2

#ifdef ALLOW_ECCO_DEBUG
      character*(MAX_LEN_MBUF) msgBuf
      INTEGER ioUnit
#endif
      logical exst

      _RL daytime
      _RL diffsecs
      integer il, localrec
      integer dayiter
      integer daydate(4)
      integer difftime(4)
      integer tempDate_1
      integer middate(4)
      integer locstartdate(4)
      integer yday, ymod
      integer md, dd, sd, ld, wd
      integer mody, modm

      integer igen_amsre, igen_amsre_lsc

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

#ifdef ALLOW_ECCO_DEBUG
      ioUnit=standardMessageUnit
#endif

      call cal_FullDate(19920101,0,locstartdate,mythid)

c-- detect the relevant gencost indices
      igen_amsre=0
      igen_amsre_lsc=0
      do k=1,NGENCOST
        if (gencost_name(k).EQ.'sstv4-amsre') igen_amsre=k
        if (gencost_name(k).EQ.'sstv4-amsre-lsc') igen_amsre_lsc=k
      enddo

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      write(fname(1:80),'(80a)') ' '
      ilps=ilnblnk( sstbarfile )
      write(fname(1:80),'(2a,i10.10)')
     &     sstbarfile(1:ilps),'.',optimcycle

      spval = -9990.

cgf =======================================================
cgf PART 1: compute smooth SST cost term
cgf =======================================================


      ndaysave=7
      ndaysaveRL=ndaysave

      do irec = 1, ndaysrec-ndaysave+1, 7

         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
              anom_sst(i,j,bi,bj)  = 0. _d 0
              obs_sst(i,j,bi,bj)  = 0. _d 0
              nb_sst(i,j,bi,bj)  = 0. _d 0
              msk_sst(i,j,bi,bj)  = 0. _d 0
            enddo
           enddo
          enddo
         enddo

c PART 1.1: compute running sample average over ndaysave
c ------------------------------------------------------

         do jrec=1,ndaysave

           krec=irec+jrec-1

c get modeled sst:
             call active_read_xy( fname, sstbar, krec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_sstbar_mean_dummy )

c get observed sst:
             daytime = FLOAT(secondsperday*(krec-1)) + modelstart
             dayiter = hoursperday*(krec-1)+modeliter0
             call cal_getdate( dayiter, daytime, daydate, mythid )
             call cal_convdate( daydate,yday,md,dd,sd,ld,wd,mythid )
             ymod = locstartdate(1)/10000
             if ( ymod .GE. yday ) then
               middate(1)=1
               call cal_FullDate(locstartdate(1),0,middate,mythid)
             else
               middate(1)=1
               tempDate_1 = yday*10000+100+1
               call cal_FullDate( tempDate_1, 0, middate, mythid)
             endif
             call cal_TimePassed( middate, daydate, difftime, mythid )
             call cal_ToSeconds( difftime, diffsecs, mythid )
c             localrec = floor(diffsecs/86400.) + 1
             localrec = int(diffsecs/86400.) + 1

             il=ilnblnk(gencost_datafile(igen_amsre))
             write(fname2(1:80),'(2a,i4)')
     &         gencost_datafile(igen_amsre)(1:il), '_', yday
             inquire( file=fname2, exist=exst )

#ifdef ALLOW_ECCO_DEBUG
        WRITE(msgBuf,'(A,I4,A,I4,A,I10,A,1PE15.2)') 'sstv4 reading ',
     &      yday,' ',ymod,' ',localrec,' ',diffsecs
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c
        CALL CAL_PRINTDATE(middate,mythid)
        CALL CAL_PRINTDATE(daydate,mythid)
        CALL CAL_PRINTDATE(difftime,mythid)
#endif

             if ( ( localrec .GT. 0 ).AND.(diffsecs .GT. 0.d0) ) then
               call mdsreadfield( fname2, cost_iprec, cost_yftype, 1,
     &           tmp_sst, localrec, mythid )
             else
              do bj = jtlo,jthi
                do bi = itlo,ithi
                    do j = jmin,jmax
                      do i = imin,imax
                         tmp_sst(i,j,bi,bj) = spval
                      enddo
                    enddo
                enddo
              enddo
             endif

c accumulate obs and misfit:
             do bj = jtlo,jthi
              do bi = itlo,ithi
               do j = jmin,jmax
                do i = imin,imax
                 if ( (tmp_sst(i,j,bi,bj).GT.spval).AND.
     &                (maskc(i,j,1,bi,bj).EQ.1.) ) then
                  anom_sst(i,j,bi,bj)= anom_sst(i,j,bi,bj)+
     &               sstbar(i,j,bi,bj)-tmp_sst(i,j,bi,bj)
                  obs_sst(i,j,bi,bj)= obs_sst(i,j,bi,bj)+
     &               tmp_sst(i,j,bi,bj)
                  nb_sst(i,j,bi,bj)=nb_sst(i,j,bi,bj)+1. _d 0
                 endif
                enddo
               enddo
              enddo
             enddo

         enddo !do jrec=1,ndaysave

c average obs and misfit:
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
             if ( nb_sst(i,j,bi,bj) .NE. 0. ) then
              obs_sst(i,j,bi,bj) =
     &            obs_sst(i,j,bi,bj)/nb_sst(i,j,bi,bj)
              anom_sst(i,j,bi,bj) =
     &            anom_sst(i,j,bi,bj)/nb_sst(i,j,bi,bj)
              msk_sst(i,j,bi,bj) = 1. _d 0
             endif
            enddo
           enddo
          enddo
         enddo


c PART 1.2: smooth anom_sst in space
c ----------------------------------------

#ifdef ALLOW_GENCOST_SSTV4_OUTPUT
         write(fname2(1:80),'(1a)') 'sstdiff_raw'
         call mdswritefield(fname2,32,.false.,'RL',
     &     1,anom_sst,irec,1,mythid)

         write(fname2(1:80),'(1a)') 'sstobs_raw'
         call mdswritefield(fname2,32,.false.,'RL',
     &     1,obs_sst,irec,1,mythid)
#endif

         call smooth_hetero2d(anom_sst,maskc,
     &     gencost_scalefile(igen_amsre_lsc),300,mythid)

#ifdef ALLOW_GENCOST_SSTV4_OUTPUT
         call smooth_hetero2d(obs_sst,maskc,
     &     gencost_scalefile(igen_amsre_lsc),300,mythid)

         write(fname2(1:80),'(1a)') 'sstdiff_smooth'
         call mdswritefield(fname2,32,.false.,'RL',
     &     1,anom_sst,irec,1,mythid)

         write(fname2(1:80),'(1a)') 'sstobs_smooth'
         call mdswritefield(fname2,32,.false.,'RL',
     &     1,obs_sst,irec,1,mythid)
#endif

c PART 1.3: compute cost function term
c ------------------------------------

         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
             junk = anom_sst(i,j,bi,bj)
             junkweight = gencost_weight(i,j,bi,bj,igen_amsre_lsc)*
     &          maskc(i,j,1,bi,bj)
             objf_gencost(igen_amsre_lsc,bi,bj) =
     &          objf_gencost(igen_amsre_lsc,bi,bj)
     &            +junk*junk*junkweight/ndaysaveRL
             if ( (junkweight.GT.0.).AND.(nb_sst(i,j,bi,bj).GT.0.) )
     &          num_gencost(igen_amsre_lsc,bi,bj) =
     &          num_gencost(igen_amsre_lsc,bi,bj) + 1. _d 0 /ndaysaveRL
            enddo
           enddo
          enddo
         enddo

      enddo

cgf =======================================================
cgf PART 2: compute raw SST cost term
cgf =======================================================

      do irec = 1, ndaysrec

c get modeled sst:
        call active_read_xy( fname, sstbar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_sstbar_mean_dummy )

c get observed sst:
             daytime = FLOAT(secondsperday*(irec-1)) + modelstart
             dayiter = hoursperday*(irec-1)+modeliter0
             call cal_getdate( dayiter, daytime, daydate, mythid )
             call cal_convdate( daydate,yday,md,dd,sd,ld,wd,mythid )
             ymod = locstartdate(1)/10000
             if ( ymod .GE. yday ) then
               middate(1)=1
               call cal_FullDate(locstartdate(1),0,middate,mythid)
             else
               middate(1)=1
               tempDate_1 = yday*10000+100+1
               call cal_FullDate( tempDate_1, 0, middate, mythid)
             endif
             call cal_TimePassed( middate, daydate, difftime, mythid )
             call cal_ToSeconds( difftime, diffsecs, mythid )
c             localrec = floor(diffsecs/86400.) + 1
             localrec = int(diffsecs/86400.) + 1

             il=ilnblnk(gencost_datafile(igen_amsre))
             write(fname2(1:80),'(2a,i4)')
     &         gencost_datafile(igen_amsre)(1:il), '_', yday
             inquire( file=fname2, exist=exst )

#ifdef ALLOW_ECCO_DEBUG
        WRITE(msgBuf,'(A,I4,A,I4,A,I10,A,1PE15.2)') 'sstv4 reading ',
     &      yday,' ',ymod,' ',localrec,' ',diffsecs
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c
        CALL CAL_PRINTDATE(middate,mythid)
        CALL CAL_PRINTDATE(daydate,mythid)
        CALL CAL_PRINTDATE(difftime,mythid)
#endif

             if ( ( localrec .GT. 0 ).AND.(diffsecs .GT. 0.d0) ) then
               call mdsreadfield( fname2, cost_iprec, cost_yftype, 1,
     &           tmp_sst, localrec, mythid )
             else
              do bj = jtlo,jthi
                do bi = itlo,ithi
                    do j = jmin,jmax
                      do i = imin,imax
                         tmp_sst(i,j,bi,bj) = spval
                      enddo
                    enddo
                enddo
              enddo
             endif

c compute misfit:
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
             if ( (tmp_sst(i,j,bi,bj).GT.spval).AND.
     &            (maskc(i,j,1,bi,bj).EQ.1.) ) then
              anom_sst(i,j,bi,bj) =
     &               sstbar(i,j,bi,bj)-tmp_sst(i,j,bi,bj)
              msk_sst(i,j,bi,bj) = 1. _d 0
             else
              anom_sst(i,j,bi,bj) = 0. _d 0
              msk_sst(i,j,bi,bj) = 0. _d 0
             endif
            enddo
           enddo
          enddo
         enddo

#ifdef ALLOW_GENCOST_SSTV4_OUTPUT
         write(fname2(1:80),'(1a)') 'sstdiff_point'
         call mdswritefield(fname2,32,.false.,'RL',
     &     1,anom_sst,irec,1,mythid)
#endif

c compute cost:

         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
             junk = anom_sst(i,j,bi,bj)
             junkweight = gencost_weight(i,j,bi,bj,igen_amsre)*
     &          maskc(i,j,1,bi,bj)*msk_sst(i,j,bi,bj)
             objf_gencost(igen_amsre,bi,bj) =
     &          objf_gencost(igen_amsre,bi,bj)+junk*junk*junkweight
             if (junkweight.GT.0.)
     &          num_gencost(igen_amsre,bi,bj) =
     &          num_gencost(igen_amsre,bi,bj) + 1. _d 0
            enddo
           enddo
          enddo
         enddo

      enddo

#endif /* ifdef ALLOW_GENCOST_FREEFORM */
#endif /* ifdef ALLOW_GENCOST_CONTRIBUTION */
#endif /* ifdef ALLOW_SMOOTH */
#endif /* ifdef ALLOW_DAILYSST_COST_CONTRIBUTION */

      end
