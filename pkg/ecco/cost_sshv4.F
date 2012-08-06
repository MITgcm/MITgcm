C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_sshv4.F,v 1.9 2012/08/06 20:41:55 heimbach Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_sshv4(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_sshv4
c     ==================================================================
c
c     o Evaluate cost function contributions of sea surface height.
c
c        started: Gael Forget, Oct-2009
c
c        working assumption for the time mean dynamic topography (MDT) constraint:
c        the various SLA data sets (tp, ers, gfo) have been consistenty cross-referenced,
c        as done in the RADS data sets. We do not need to know the reference dynamic
c        topography (or SSH/Geoid). Simply it is assumed that the biases
c        between instruments have been taken care of. This is only a matter
c        for the MDT constraint, not for SLA constraints (see below).
c
cgf 1) there are a few hardcoded numbers that will eventually be put in common
cgf     blocks/namelists
cgf 2) there are a several refinements that should be considered, such as
cgf     modulating weights with respect to numbers of samples
c
c     ==================================================================
c     SUBROUTINE cost_sshv4
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

#ifdef ALLOW_SSH_COST_CONTRIBUTION
#ifdef ALLOW_SMOOTH
c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec,jrec,krec
      integer ilps
      integer gwunit

      logical doglobalread
      logical ladinit

      _RL offset,fac
      _RL offset_sum
      _RL psmean    ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL diagnosfld ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )

c for PART 1: re-reference MDT (tpmean) to the inferred SLA reference field
      _RL mean_slaobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_slaobs_NUM(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

c for PART 2: compute time mean differences over the model period
      _RL mean_slaobs2(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMssh_all(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMssh_all_NUM(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMssh_all_MSK(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL mean_psMtpobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMtpobs_NUM(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMtpobs_MSK(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL mean_psMersobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMersobs_NUM(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMersobs_MSK(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL mean_psMgfoobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMgfoobs_NUM(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mean_psMgfoobs_MSK(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

c for PART 4/5: compute smooth/raw anomalies
      _RL anom_psMslaobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL anom_slaobs (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL anom_psMslaobs_NUM (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL anom_psMtpobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL anom_psMtpobs_NUM (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL anom_tpobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL anom_psMersobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL anom_psMersobs_NUM(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL anom_ersobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL anom_psMgfoobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL anom_psMgfoobs_NUM(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL anom_gfoobs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      integer tpmean_y0,tpmean_y1,year,day
      integer num_var

      _RL junk,junkweight

      integer ndaysave
      _RL ndaysaveRL

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

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      write(fname(1:80),'(80a)') ' '
      ilps=ilnblnk( psbarfile )
      write(fname(1:80),'(2a,i10.10)')
     &     psbarfile(1:ilps),'.',optimcycle


cgf =======================================================
cgf PART 1:
cgf        x Get the MDT (tpmean) ... compute the sample mean
cgf        (mean_slaobs) of the SLA data (i.e. RADS for tp, ers, and gfo
cgf        together) over the time interval of the MDT ... subtract
cgf        mean_slaobs from tpmean.
cgf        x At this point, tpmean is the inferred SLA reference field.
cgf        x From there on, tpmean+sla will be directly comparable to
cgf        the model SSH (psbar).
cgf =======================================================

c--   Read mean field and mask
      call cost_ReadTopexMean( mythid )

c--   Compute mean_slaobs: sample mean SLA over the time period of tpmean.

c pavlis and ecco/rio
      tpmean_y0=1993
      tpmean_y1=2004
c maximenko
c      tpmean_y0=1992
c      tpmean_y1=2002
c rio
c      tpmean_y0=1993
c      tpmean_y1=1999

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              mean_slaobs(i,j,bi,bj)  = 0. _d 0
              mean_slaobs_NUM(i,j,bi,bj)  = 0. _d 0
          enddo
         enddo
        enddo
       enddo

      do year=tpmean_y0,tpmean_y1
       do day=1,366
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
      call cost_sla_read_yd( topexfile, topexstartdate,
     &                tpobs, tpmask,
     &                year, day, mythid )
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
      call cost_sla_read_yd( ersfile, ersstartdate,
     &                ersobs, ersmask,
     &                year, day, mythid )
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
      call cost_sla_read_yd( gfofile, gfostartdate,
     &                gfoobs, gfomask,
     &                year, day, mythid )
#endif
       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
      if ( tpmask(i,j,bi,bj)*tpmeanmask(i,j,bi,bj)*
     &    wsshv4(i,j,2,bi,bj) .NE. 0. ) then
          mean_slaobs(i,j,bi,bj)= mean_slaobs(i,j,bi,bj)+
     &  tpobs(i,j,bi,bj)
          mean_slaobs_NUM(i,j,bi,bj)= mean_slaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
      if ( ersmask(i,j,bi,bj)*tpmeanmask(i,j,bi,bj)*
     &    wsshv4(i,j,3,bi,bj) .NE. 0. ) then
          mean_slaobs(i,j,bi,bj)= mean_slaobs(i,j,bi,bj)+
     &  ersobs(i,j,bi,bj)
          mean_slaobs_NUM(i,j,bi,bj)= mean_slaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
      if ( gfomask(i,j,bi,bj)*tpmeanmask(i,j,bi,bj)*
     &    wsshv4(i,j,4,bi,bj) .NE. 0. ) then
          mean_slaobs(i,j,bi,bj)= mean_slaobs(i,j,bi,bj)+
     &  gfoobs(i,j,bi,bj)
          mean_slaobs_NUM(i,j,bi,bj)= mean_slaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
#endif
          enddo
         enddo
        enddo
       enddo

       enddo !do day=1,366
      enddo !do year=tpmean_y0,tpmean_y1

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
               if ( ( mean_slaobs_NUM(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ).AND.
     &              ( abs(YC(i,j,bi,bj)) .LE. 66. ).AND.
     &              ( tpmeanmask(i,j,bi,bj) .NE. 0. ) ) then
                  mean_slaobs(i,j,bi,bj) = mean_slaobs(i,j,bi,bj) /
     &                 mean_slaobs_NUM(i,j,bi,bj)
               else
                  mean_slaobs(i,j,bi,bj) = 0. _d 0
               endif
            enddo
          enddo
        enddo
      enddo


c--   smooth mean_slaobs:

      write(fname4test(1:80),'(1a)') 'sla2mdt_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,mean_slaobs,1,1,mythid)

      call smooth_hetero2d(mean_slaobs,maskc,
     &     sshv4cost_scalefile(1),300,mythid)

      write(fname4test(1:80),'(1a)') 'sla2mdt_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,mean_slaobs,1,1,mythid)

c--   re-reference tpmean:
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
               if ( ( tpmeanmask(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ) ) then
                  tpmean(i,j,bi,bj) = tpmean(i,j,bi,bj)
     &                 -mean_slaobs(i,j,bi,bj)
               endif
            enddo
          enddo
        enddo
      enddo


cgf =======================================================
cgf PART 2: compute sample means of psbar-slaobs over the
cgf          period that is covered by the model (i.e. psbar).
cgf          x for all SLA data sets together: mean_psMssh_all, mean_psMssh_all_MSK,
cgf             and offset will be used in PART 3 (MDT cost term).
cgf          x for each SLA data individually. mean_psMtpobs, mean_psMtpobs_MS, etc.
cgf             will be used in PART 4&5 (SLA cost terms).
cgf =======================================================

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax

              psmean(i,j,bi,bj)    = 0. _d 0
              mean_psMtpobs(i,j,bi,bj)  = 0. _d 0
              mean_psMersobs(i,j,bi,bj) = 0. _d 0
              mean_psMgfoobs(i,j,bi,bj) = 0. _d 0
              mean_psMssh_all(i,j,bi,bj) = 0. _d 0
              mean_slaobs2(i,j,bi,bj)  = 0. _d 0

              mean_psMtpobs_NUM(i,j,bi,bj)  = 0. _d 0
              mean_psMersobs_NUM(i,j,bi,bj) = 0. _d 0
              mean_psMgfoobs_NUM(i,j,bi,bj) = 0. _d 0
              mean_psMssh_all_NUM(i,j,bi,bj) = 0. _d 0

              mean_psMtpobs_MSK(i,j,bi,bj)  = 0. _d 0
              mean_psMersobs_MSK(i,j,bi,bj) = 0. _d 0
              mean_psMgfoobs_MSK(i,j,bi,bj) = 0. _d 0

            enddo
          enddo
        enddo
      enddo
      offset     = 0. _d 0
      offset_sum = 0. _d 0


      do irec = 1, ndaysrec

        call active_read_xy( fname, psbar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_psbar_mean_dummy )

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
      call cost_sla_read( topexfile, topexstartdate, topexperiod,
     &                topexintercept, topexslope,
     &                tpobs, tpmask,
     &                irec, mythid )
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
      call cost_sla_read( ersfile, ersstartdate, ersperiod,
     &                ersintercept, ersslope,
     &                ersobs, ersmask,
     &                irec, mythid )
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
      call cost_sla_read( gfofile, gfostartdate, gfoperiod,
     &                gfointercept, gfoslope,
     &                gfoobs, gfomask,
     &                irec, mythid )
#endif

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                psmean(i,j,bi,bj) = psmean(i,j,bi,bj) +
     &                psbar(i,j,bi,bj) / float(ndaysrec)
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
                if ( tpmask(i,j,bi,bj)*wsshv4(i,j,2,bi,bj)
     &               .NE. 0. ) then
                   mean_slaobs2(i,j,bi,bj)=
     &                 mean_slaobs2(i,j,bi,bj)+tpobs(i,j,bi,bj)
                   mean_psMtpobs(i,j,bi,bj) =
     &                 mean_psMtpobs(i,j,bi,bj) +
     &                 psbar(i,j,bi,bj)-tpobs(i,j,bi,bj)
                   mean_psMtpobs_NUM(i,j,bi,bj) =
     &                 mean_psMtpobs_NUM(i,j,bi,bj) + 1. _d 0
                endif
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
                if ( ersmask(i,j,bi,bj)*wsshv4(i,j,3,bi,bj)
     &               .NE. 0. ) then
                   mean_slaobs2(i,j,bi,bj)=
     &                 mean_slaobs2(i,j,bi,bj)+ersobs(i,j,bi,bj)
                   mean_psMersobs(i,j,bi,bj) =
     &                 mean_psMersobs(i,j,bi,bj) +
     &                 psbar(i,j,bi,bj)-ersobs(i,j,bi,bj)
                   mean_psMersobs_NUM(i,j,bi,bj) =
     &                 mean_psMersobs_NUM(i,j,bi,bj) + 1. _d 0
                endif
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
                if ( gfomask(i,j,bi,bj)*wsshv4(i,j,4,bi,bj)
     &               .NE. 0. ) then
                   mean_slaobs2(i,j,bi,bj)=
     &                 mean_slaobs2(i,j,bi,bj)+gfoobs(i,j,bi,bj)
                   mean_psMgfoobs(i,j,bi,bj) =
     &                 mean_psMgfoobs(i,j,bi,bj) +
     &                 psbar(i,j,bi,bj)-gfoobs(i,j,bi,bj)
                   mean_psMgfoobs_NUM(i,j,bi,bj) =
     &                 mean_psMgfoobs_NUM(i,j,bi,bj) + 1. _d 0
                endif
#endif
              enddo
            enddo
          enddo
        enddo

c--   END loop over records for the first time.
      enddo

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
               if ( mean_psMtpobs_NUM(i,j,bi,bj) .NE. 0. ) then
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) +
     &                 mean_psMtpobs(i,j,bi,bj)
                  mean_psMssh_all_NUM(i,j,bi,bj) =
     &                 mean_psMssh_all_NUM(i,j,bi,bj) +
     &                 mean_psMtpobs_NUM(i,j,bi,bj)
                  mean_psMtpobs(i,j,bi,bj) =
     &                 mean_psMtpobs(i,j,bi,bj) /
     &                 mean_psMtpobs_NUM(i,j,bi,bj)
                  mean_psMtpobs_MSK(i,j,bi,bj) = 1. _d 0
               endif
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
               if ( mean_psMersobs_NUM(i,j,bi,bj) .NE. 0. ) then
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) +
     &                 mean_psMersobs(i,j,bi,bj)
                  mean_psMssh_all_NUM(i,j,bi,bj) =
     &                 mean_psMssh_all_NUM(i,j,bi,bj) +
     &                 mean_psMersobs_NUM(i,j,bi,bj)
                  mean_psMersobs(i,j,bi,bj) =
     &                 mean_psMersobs(i,j,bi,bj) /
     &                 mean_psMersobs_NUM(i,j,bi,bj)
                  mean_psMersobs_MSK(i,j,bi,bj) = 1. _d 0
               endif
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
               if ( mean_psMgfoobs_NUM(i,j,bi,bj) .NE. 0. ) then
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) +
     &                 mean_psMgfoobs(i,j,bi,bj)
                  mean_psMssh_all_NUM(i,j,bi,bj) =
     &                 mean_psMssh_all_NUM(i,j,bi,bj) +
     &                 mean_psMgfoobs_NUM(i,j,bi,bj)
                  mean_psMgfoobs(i,j,bi,bj) =
     &                 mean_psMgfoobs(i,j,bi,bj) /
     &                 mean_psMgfoobs_NUM(i,j,bi,bj)
                  mean_psMgfoobs_MSK(i,j,bi,bj) = 1. _d 0
               endif
#endif
               if ( ( mean_psMssh_all_NUM(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ) .AND.
     &              ( abs(YC(i,j,bi,bj)) .LE. 66. ).AND.
     &              ( tpmeanmask(i,j,bi,bj) .NE. 0. ) ) then
                  mean_slaobs2(i,j,bi,bj) =
     &                 mean_slaobs2(i,j,bi,bj) /
     &                 mean_psMssh_all_NUM(i,j,bi,bj)
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) /
     &                 mean_psMssh_all_NUM(i,j,bi,bj)-tpmean(i,j,bi,bj)
                  mean_psMssh_all_MSK(i,j,bi,bj) = 1. _d 0
                  offset=offset+mean_psMssh_all(i,j,bi,bj)*
     &                 mean_psMssh_all_NUM(i,j,bi,bj)
                  offset_sum=offset_sum+mean_psMssh_all_NUM(i,j,bi,bj)
               else
                  mean_slaobs2(i,j,bi,bj) = 0.d0
                  mean_psMssh_all(i,j,bi,bj) = 0. _d 0
                  mean_psMssh_all_MSK(i,j,bi,bj) = 0. _d 0
               endif
              enddo
            enddo
          enddo
        enddo

c--   Do a global summation.
      _GLOBAL_SUM_RL( offset     , mythid )
      _GLOBAL_SUM_RL( offset_sum , mythid )

      if (offset_sum .eq. 0.0) then
        _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' sshv4: offset_sum = zero!'
        call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
        _END_MASTER( mythid )
c        stop   '  ... stopped in cost_ssh.'
      else
        _BEGIN_MASTER( mythid )
        write(msgbuf,'(a,d22.15)')
     &          ' sshv4: offset_sum = ',offset_sum
        call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
        _END_MASTER( mythid )
      endif

c--   Compute (average) offset
      offset = offset / offset_sum

c--   subtract offset from mean_psMssh_all and psmean
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax

               if ( (mean_psMssh_all_MSK(i,j,bi,bj) .NE. 0.) .AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ) .AND.
     &              ( abs(YC(i,j,bi,bj)) .LE. 66. ) ) then
c use the re-referencing approach
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) - offset
               elseif ( ( tpmeanmask(i,j,bi,bj) .NE. 0. ) .AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ) .AND.
     &              ( abs(YC(i,j,bi,bj)) .GT. 66. ) ) then
c use the simpler approach
                  mean_psMssh_all(i,j,bi,bj) =
     &             psmean(i,j,bi,bj) -tpmean(i,j,bi,bj) - offset
               else
                  mean_psMssh_all(i,j,bi,bj) = 0. _d 0
               endif

               if ( maskc(i,j,1,bi,bj) .NE. 0. )
     &            psmean(i,j,bi,bj)=psmean(i,j,bi,bj)-offset
            enddo
          enddo
        enddo
       enddo

c--    smooth mean_psMssh_all
      write(fname4test(1:80),'(1a)') 'mdtdiff_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     &    1,mean_psMssh_all,1,1,mythid)

      call smooth_hetero2d(mean_psMssh_all,maskc,
     &     sshv4cost_scalefile(1),300,mythid)

      write(fname4test(1:80),'(1a)') 'mdtdiff_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     &    1,mean_psMssh_all,1,1,mythid)

      write(fname4test(1:80),'(1a)') 'sla2model_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     &    1,mean_slaobs2,1,1,mythid)

      call smooth_hetero2d(mean_slaobs2,maskc,
     &     sshv4cost_scalefile(1),300,mythid)

      write(fname4test(1:80),'(1a)') 'sla2model_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     &    1,mean_slaobs2,1,1,mythid)

cgf at this point:
cgf     1) mean_psMssh_all is the sample mean <psbar-slaobs-tpmean-offset>,
cgf             to which a smoothing filter has been applied.
cgf     2) mean_psMtpobs is the (unsmoothed) sample mean <psbar-tpobs>.
cgf             And similarly for ers and gfo, each treated separately.

#ifdef ALLOW_PROFILES
      if ( usePROFILES ) then
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sny
            do i = 1,snx
              prof_etan_mean(i,j,bi,bj)=psmean(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
      _EXCH_XY_RL( prof_etan_mean, mythid )
      endif
#endif


cgf =======================================================
cgf PART 3: compute MDT cost term
cgf =======================================================


#ifdef ALLOW_SSH_MEAN_COST_CONTRIBUTION

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
       junk = mean_psMssh_all(i,j,bi,bj)
       objf_sshv4cost(1,bi,bj) = objf_sshv4cost(1,bi,bj) + junk*junk
     &      *wsshv4(i,j,1,bi,bj)*tpmeanmask(i,j,bi,bj)
       if ( wsshv4(i,j,1,bi,bj)*tpmeanmask(i,j,bi,bj) .ne. 0. )
     &      num_sshv4cost(1,bi,bj) = num_sshv4cost(1,bi,bj) + 1. _d 0
       diagnosfld(i,j,bi,bj) = junk*junk
     &      * wsshv4(i,j,1,bi,bj)*tpmeanmask(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo

      CALL WRITE_FLD_XY_RL( 'DiagnosSSHmean', ' ', diagnosfld,
     &                           optimcycle, mythid )

#endif /* ALLOW_SSH_MEAN_COST_CONTRIBUTION */



cgf =======================================================
cgf PART 4: compute smooth SLA cost term
cgf =======================================================


      ndaysave=35
      ndaysaveRL=ndaysave

      do irec = 1, ndaysrec-ndaysave+1, 7

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              anom_psMslaobs(i,j,bi,bj)  = 0. _d 0
              anom_slaobs(i,j,bi,bj)  = 0. _d 0
              anom_psMtpobs(i,j,bi,bj)  = 0. _d 0
              anom_psMersobs(i,j,bi,bj) = 0. _d 0
              anom_psMgfoobs(i,j,bi,bj) = 0. _d 0
              anom_psMslaobs_NUM(i,j,bi,bj)  = 0. _d 0
              anom_psMtpobs_NUM(i,j,bi,bj)  = 0. _d 0
              anom_psMersobs_NUM(i,j,bi,bj) = 0. _d 0
              anom_psMgfoobs_NUM(i,j,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo

c PART 4.1: compute running sample average over ndaysave
c ------------------------------------------------------

      do jrec=1,ndaysave

        krec=irec+jrec-1

        call active_read_xy( fname, psbar, krec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_psbar_mean_dummy )

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
      call cost_sla_read( topexfile, topexstartdate, topexperiod,
     &                topexintercept, topexslope,
     &                tpobs, tpmask,
     &                krec, mythid )
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
      call cost_sla_read( ersfile, ersstartdate, ersperiod,
     &                ersintercept, ersslope,
     &                ersobs, ersmask,
     &                krec, mythid )
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
      call cost_sla_read( gfofile, gfostartdate, gfoperiod,
     &                gfointercept, gfoslope,
     &                gfoobs, gfomask,
     &                krec, mythid )
#endif

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
      if ( tpmask(i,j,bi,bj)*mean_psMtpobs_MSK(i,j,bi,bj)
     &  .NE.0. ) then
           anom_psMtpobs(i,j,bi,bj)= anom_psMtpobs(i,j,bi,bj)+
     &        psbar(i,j,bi,bj)-tpobs(i,j,bi,bj)
     &        -mean_psMtpobs(i,j,bi,bj)
           anom_psMslaobs(i,j,bi,bj)= anom_psMslaobs(i,j,bi,bj)+
     &        psbar(i,j,bi,bj)-tpobs(i,j,bi,bj)
     &        -mean_psMtpobs(i,j,bi,bj)
           anom_slaobs(i,j,bi,bj)= anom_slaobs(i,j,bi,bj)+
     &        tpobs(i,j,bi,bj)
           anom_psMtpobs_NUM(i,j,bi,bj)=
     &        anom_psMtpobs_NUM(i,j,bi,bj)+1. _d 0
           anom_psMslaobs_NUM(i,j,bi,bj)=
     &        anom_psMslaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
      if ( ersmask(i,j,bi,bj)*mean_psMersobs_MSK(i,j,bi,bj)
     &  .NE.0. ) then
           anom_psMersobs(i,j,bi,bj)= anom_psMersobs(i,j,bi,bj)+
     &        psbar(i,j,bi,bj)-ersobs(i,j,bi,bj)
     &        -mean_psMersobs(i,j,bi,bj)
           anom_psMersobs_NUM(i,j,bi,bj)=
     &        anom_psMersobs_NUM(i,j,bi,bj)+1. _d 0
           anom_psMslaobs(i,j,bi,bj)= anom_psMslaobs(i,j,bi,bj)+
     &        psbar(i,j,bi,bj)-ersobs(i,j,bi,bj)
     &        -mean_psMersobs(i,j,bi,bj)
           anom_slaobs(i,j,bi,bj)= anom_slaobs(i,j,bi,bj)+
     &        ersobs(i,j,bi,bj)
           anom_psMslaobs_NUM(i,j,bi,bj)=
     &        anom_psMslaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
      if ( gfomask(i,j,bi,bj)*mean_psMgfoobs_MSK(i,j,bi,bj)
     &  .NE.0. ) then
           anom_psMgfoobs(i,j,bi,bj)= anom_psMgfoobs(i,j,bi,bj)+
     &        psbar(i,j,bi,bj)-gfoobs(i,j,bi,bj)
     &        -mean_psMgfoobs(i,j,bi,bj)
           anom_psMgfoobs_NUM(i,j,bi,bj)=
     &        anom_psMgfoobs_NUM(i,j,bi,bj)+1. _d 0
           anom_psMslaobs(i,j,bi,bj)= anom_psMslaobs(i,j,bi,bj)+
     &        psbar(i,j,bi,bj)-gfoobs(i,j,bi,bj)
     &        -mean_psMgfoobs(i,j,bi,bj)
           anom_slaobs(i,j,bi,bj)= anom_slaobs(i,j,bi,bj)+
     &        gfoobs(i,j,bi,bj)
           anom_psMslaobs_NUM(i,j,bi,bj)=
     &        anom_psMslaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
#endif
          enddo
         enddo
        enddo
       enddo

      enddo !do jrec=1,ndaysave

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
               if ( anom_psMtpobs_NUM(i,j,bi,bj) .NE. 0. ) then
                  anom_psMtpobs(i,j,bi,bj) =
     &                 anom_psMtpobs(i,j,bi,bj) /
     &                 anom_psMtpobs_NUM(i,j,bi,bj)
               endif
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
               if ( anom_psMersobs_NUM(i,j,bi,bj) .NE. 0. ) then
                  anom_psMersobs(i,j,bi,bj) =
     &                 anom_psMersobs(i,j,bi,bj) /
     &                 anom_psMersobs_NUM(i,j,bi,bj)
               endif
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
               if ( anom_psMgfoobs_NUM(i,j,bi,bj) .NE. 0. ) then
                  anom_psMgfoobs(i,j,bi,bj) =
     &                 anom_psMgfoobs(i,j,bi,bj) /
     &                 anom_psMgfoobs_NUM(i,j,bi,bj)
               endif
#endif
               if ( ( anom_psMslaobs_NUM(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ) ) then
                  anom_psMslaobs(i,j,bi,bj) =
     &                 anom_psMslaobs(i,j,bi,bj) /
     &                 anom_psMslaobs_NUM(i,j,bi,bj)
                  anom_slaobs(i,j,bi,bj) =
     &                 anom_slaobs(i,j,bi,bj) /
     &                 anom_psMslaobs_NUM(i,j,bi,bj)
               else
                  anom_psMslaobs(i,j,bi,bj) = 0. _d 0
                  anom_slaobs(i,j,bi,bj) = 0. _d 0
               endif
              enddo
            enddo
          enddo
        enddo

c PART 4.2: smooth anom_psMslaobs in space
c ----------------------------------------

#ifdef ALLOW_SSHV4_OUTPUT
      write(fname4test(1:80),'(1a)') 'sladiff_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_psMslaobs,irec,1,mythid)

      write(fname4test(1:80),'(1a)') 'slaobs_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_slaobs,irec,1,mythid)
#endif

      call smooth_hetero2d(anom_psMslaobs,maskc,
     &     sshv4cost_scalefile(5),300,mythid)

#ifdef ALLOW_SSHV4_OUTPUT
      call smooth_hetero2d(anom_slaobs,maskc,
     &     sshv4cost_scalefile(5),300,mythid)

      write(fname4test(1:80),'(1a)') 'sladiff_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_psMslaobs,irec,1,mythid)

      write(fname4test(1:80),'(1a)') 'slaobs_smooth'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_slaobs,irec,1,mythid)
#endif

c PART 4.3: compute cost function term
c ------------------------------------

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
# if (defined (ALLOW_SSH_GFOANOM_COST_CONTRIBUTION) || \
      defined (ALLOW_SSH_TPANOM_COST_CONTRIBUTION) || \
      defined (ALLOW_SSH_ERSANOM_COST_CONTRIBUTION))
          junk = anom_psMslaobs(i,j,bi,bj)
          objf_sshv4cost(5,bi,bj) = objf_sshv4cost(5,bi,bj)
     &    + junk*junk*wsshv4(i,j,5,bi,bj)*maskc(i,j,1,bi,bj)
     &        /ndaysaveRL
          if ( (wsshv4(i,j,5,bi,bj).GT.0.).AND.
     &         (anom_psMslaobs_NUM(i,j,bi,bj).GT.0.).AND.
     &         (maskc(i,j,1,bi,bj) .ne. 0.) )
     &         num_sshv4cost(5,bi,bj) = num_sshv4cost(5,bi,bj)
     &        + 1. _d 0 /ndaysaveRL
#endif
           enddo
         enddo
        enddo
       enddo

      enddo


cgf =======================================================
cgf PART 5: compute raw SLA cost term
cgf =======================================================


      do irec = 1, ndaysrec

        call active_read_xy( fname, psbar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_psbar_mean_dummy )

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
        call cost_readtopex( irec, mythid )
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
        call cost_readers( irec, mythid )
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
        call cost_readgfo( irec, mythid )
#endif

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              anom_psMtpobs(i,j,bi,bj)  = 0. _d 0
              anom_psMersobs(i,j,bi,bj) = 0. _d 0
              anom_psMgfoobs(i,j,bi,bj) = 0. _d 0
              anom_tpobs(i,j,bi,bj)  = 0. _d 0
              anom_ersobs(i,j,bi,bj) = 0. _d 0
              anom_gfoobs(i,j,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
      if ( tpmask(i,j,bi,bj)*mean_psMtpobs_MSK(i,j,bi,bj).NE.0. )
     & then
         anom_psMtpobs(i,j,bi,bj)=
     &       psbar(i,j,bi,bj) - tpobs(i,j,bi,bj)
     &       - mean_psMtpobs(i,j,bi,bj)
         anom_tpobs(i,j,bi,bj)=tpobs(i,j,bi,bj)
      endif
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
      if ( ersmask(i,j,bi,bj)*mean_psMersobs_MSK(i,j,bi,bj).NE.0. )
     & then
         anom_psMersobs(i,j,bi,bj)=
     &       psbar(i,j,bi,bj) - ersobs(i,j,bi,bj)
     &       - mean_psMersobs(i,j,bi,bj)
         anom_ersobs(i,j,bi,bj)=ersobs(i,j,bi,bj)
      endif
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
      if ( gfomask(i,j,bi,bj)*mean_psMgfoobs_MSK(i,j,bi,bj).NE.0. )
     & then
         anom_psMgfoobs(i,j,bi,bj)=
     &       psbar(i,j,bi,bj) - gfoobs(i,j,bi,bj)
     &       - mean_psMgfoobs(i,j,bi,bj)
         anom_gfoobs(i,j,bi,bj)=gfoobs(i,j,bi,bj)
      endif
#endif
          enddo
         enddo
        enddo
       enddo

#ifdef ALLOW_SSHV4_OUTPUT
      write(fname4test(1:80),'(1a)') 'sladiff_tp_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_psMtpobs,irec,1,mythid)
      write(fname4test(1:80),'(1a)') 'sladiff_ers_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_psMersobs,irec,1,mythid)
      write(fname4test(1:80),'(1a)') 'sladiff_gfo_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_psMgfoobs,irec,1,mythid)

      write(fname4test(1:80),'(1a)') 'slaobs_tp_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_tpobs,irec,1,mythid)
      write(fname4test(1:80),'(1a)') 'slaobs_ers_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_ersobs,irec,1,mythid)
      write(fname4test(1:80),'(1a)') 'slaobs_gfo_raw'
      call mdswritefield(fname4test,32,.false.,'RL',
     & 1,anom_gfoobs,irec,1,mythid)
#endif 

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
c--             The array psobs contains SSH anomalies.
                junkweight = mean_psMtpobs_MSK(i,j,bi,bj)
     &                      *wsshv4(i,j,2,bi,bj)
     &                      *tpmask(i,j,bi,bj)
     &                      *cosphi(i,j,bi,bj)
                junk = anom_psMtpobs(i,j,bi,bj)
                objf_sshv4cost(2,bi,bj) =
     &                objf_sshv4cost(2,bi,bj)
     &              + junk*junk*junkweight
                if ( junkweight .ne. 0. )
     &                num_sshv4cost(2,bi,bj) =
     &                num_sshv4cost(2,bi,bj) + 1. _d 0
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
c--             The array ersobs contains SSH anomalies.
                junkweight = mean_psMersobs_MSK(i,j,bi,bj)
     &                      *wsshv4(i,j,3,bi,bj)
     &                      *ersmask(i,j,bi,bj)
     &                      *cosphi(i,j,bi,bj)
                junk = anom_psMersobs(i,j,bi,bj)
                objf_sshv4cost(3,bi,bj) =
     &                objf_sshv4cost(3,bi,bj)
     &              + junk*junk*junkweight
                if ( junkweight .ne. 0. )
     &                num_sshv4cost(3,bi,bj) =
     &                num_sshv4cost(3,bi,bj) + 1. _d 0
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
c--             The array gfoobs contains SSH anomalies.
                junkweight = mean_psMgfoobs_MSK(i,j,bi,bj)
     &                      *wsshv4(i,j,4,bi,bj)
     &                      *gfomask(i,j,bi,bj)
     &                      *cosphi(i,j,bi,bj)
                junk = anom_psMgfoobs(i,j,bi,bj)
                objf_sshv4cost(4,bi,bj) =
     &                objf_sshv4cost(4,bi,bj)
     &              + junk*junk*junkweight
                if ( junkweight .ne. 0. )
     &                num_sshv4cost(4,bi,bj) =
     &                num_sshv4cost(4,bi,bj) + 1. _d 0
#endif
           enddo
         enddo
        enddo
       enddo

      enddo


cgf =======================================================
cgf PART 6: compute sum of MDT & smooth SLA & raw SLA cost terms
cgf =======================================================

c note: this is only for printing purposes, whereas objf_sshv4cost
c       will be used directly (with multipliers) as part of fc

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do num_var=1,NSSHV4COST
           objf_h(bi,bj) = objf_h(bi,bj)
     &                   + objf_sshv4cost(num_var,bi,bj)
           num_h(bi,bj) = num_h(bi,bj)
     &                   + num_sshv4cost(num_var,bi,bj)
          enddo
        enddo
      enddo

#endif /* ifdef ALLOW_SMOOTH */
#endif /* ifdef ALLOW_SSH_COST_CONTRIBUTION */

      end
