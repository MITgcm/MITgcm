C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_ssh_new.F,v 1.8 2012/08/06 20:41:55 heimbach Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ssh_new(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_ssh
c     ==================================================================
c
c     o Evaluate cost function contribution of sea surface height.
c       using of geoid error covariances requires regular model grid
c
c     started: Detlef Stammer, Ralf Giering Jul-1996
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     changed: Ralf Giering Ralf.Giering@FastOpt.de 12-Jun-2001
c
c              - totally rewrite for parallel processing
c
c     ==================================================================
c     SUBROUTINE cost_ssh
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

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
c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer ilps
      integer gwunit

      logical doglobalread
      logical ladinit

      _RL offset
      _RL costmean
      _RL offset_sum
      _RL psmean    ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL psmeantp  ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL psmeaners ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL psmeangfo ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL sumtp  ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL sumers ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL sumgfo ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )

      _RL wwwtp1  ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL wwwers1 ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL wwwgfo1 ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL wwwtp2  ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL wwwers2 ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL wwwgfo2 ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL junk

      character*(80) fname
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
      costmean   = 0. _d 0

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      write(fname(1:80),'(80a)') ' '
      ilps=ilnblnk( psbarfile )
      write(fname(1:80),'(2a,i10.10)')
     &     psbarfile(1:ilps),'.',optimcycle

c--   ============
c--   Mean values.
c--   ============

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              psmean(i,j,bi,bj)    = 0. _d 0
              psmeantp(i,j,bi,bj)  = 0. _d 0
              psmeaners(i,j,bi,bj) = 0. _d 0
              psmeangfo(i,j,bi,bj) = 0. _d 0
              sumtp(i,j,bi,bj)  = 0. _d 0
              sumers(i,j,bi,bj) = 0. _d 0
              sumgfo(i,j,bi,bj) = 0. _d 0
              wwwtp1(i,j,bi,bj)  = 0. _d 0
              wwwers1(i,j,bi,bj) = 0. _d 0
              wwwgfo1(i,j,bi,bj) = 0. _d 0
              wwwtp2(i,j,bi,bj)  = 0. _d 0
              wwwers2(i,j,bi,bj) = 0. _d 0
              wwwgfo2(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

c--   Read mean field and generate mask
      call cost_ReadTopexMean( mythid )

c--   Loop over records for the first time.
      do irec = 1, ndaysrec

c--     Compute the mean over all psbar records.
        call active_read_xy( fname, psbar, irec, doglobalread,
     &                       ladinit, optimcycle, mythid,
     &                       xx_psbar_mean_dummy )

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
cph these if would be more efficient, but need recomputations
c        if ( tpTimeMask(irec) .NE. 0. )
        call cost_readtopex( irec, mythid )
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
cph these if would be more efficient, but need recomputations
c        if ( ersTimeMask(irec) .NE. 0. )
        call cost_readers( irec, mythid )
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
cph these if would be more efficient, but need recomputations
c        if ( gfoTimeMask(irec) .NE. 0. )
        call cost_readgfo( irec, mythid )
#endif

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                psmean(i,j,bi,bj) = psmean(i,j,bi,bj) +
     &                psbar(i,j,bi,bj) / float(ndaysrec)
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
                if ( tpmask(i,j,bi,bj)*wtp(i,j,bi,bj)
     &               *tpTimeMask(irec) .NE. 0. ) then
                   psmeantp(i,j,bi,bj) = psmeantp(i,j,bi,bj) +
     &                 psbar(i,j,bi,bj)
                   sumtp(i,j,bi,bj) = sumtp(i,j,bi,bj) + 1. _d 0
                endif
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
                if ( ersmask(i,j,bi,bj)*wers(i,j,bi,bj)
     &               *ersTimeMask(irec) .NE. 0. ) then
                   psmeaners(i,j,bi,bj) = psmeaners(i,j,bi,bj) +
     &                 psbar(i,j,bi,bj)
                   sumers(i,j,bi,bj) = sumers(i,j,bi,bj) + 1. _d 0
                endif
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
                if ( gfomask(i,j,bi,bj)*wgfo(i,j,bi,bj)
     &               *gfoTimeMask(irec) .NE. 0. ) then
                   psmeangfo(i,j,bi,bj) = psmeangfo(i,j,bi,bj) +
     &                 psbar(i,j,bi,bj)
                   sumgfo(i,j,bi,bj) = sumgfo(i,j,bi,bj) + 1. _d 0
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
               if ( sumtp(i,j,bi,bj) .NE. 0. ) then
                  psmeantp(i,j,bi,bj) = psmeantp(i,j,bi,bj) /
     &                 sumtp(i,j,bi,bj)
                  wwwtp1(i,j,bi,bj) = 1. _d 0
               endif
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
               if ( sumers(i,j,bi,bj) .NE. 0. ) then
                  psmeaners(i,j,bi,bj) = psmeaners(i,j,bi,bj) /
     &                 sumers(i,j,bi,bj)
                  wwwers1(i,j,bi,bj) = 1. _d 0
               endif
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
               if ( sumgfo(i,j,bi,bj) .NE. 0. ) then
                  psmeangfo(i,j,bi,bj) = psmeangfo(i,j,bi,bj) /
     &                 sumgfo(i,j,bi,bj)
                  wwwgfo1(i,j,bi,bj) = 1. _d 0
               endif
#endif
              enddo
            enddo
          enddo
        enddo

c--   Compute and remove offset for current tile and sum over all
c--   tiles of this instance.
      offset     = 0. _d 0
      offset_sum = 0. _d 0

c--   Sum over this thread tiles.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sny
            do i = 1,snx
              offset     = offset +
     &                     tpmeanmask(i,j,bi,bj)*cosphi(i,j,bi,bj)*
     &                     (tpmean(i,j,bi,bj) - psmean(i,j,bi,bj))
              offset_sum = offset_sum +
     &                     tpmeanmask(i,j,bi,bj)*cosphi(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo

c--   Do a global summation.
      _GLOBAL_SUM_RL( offset     , mythid )
      _GLOBAL_SUM_RL( offset_sum , mythid )

      if (offset_sum .eq. 0.0) then
        _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' cost_ssh: offset_sum = zero!'
        call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
        _END_MASTER( mythid )
        stop   '  ... stopped in cost_ssh.'
      else
        _BEGIN_MASTER( mythid )
        write(msgbuf,'(a,d22.15)')
     &          ' cost_ssh: offset_sum = ',offset_sum
        call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
        _END_MASTER( mythid )
      endif

      offset = offset / offset_sum

#ifdef ALLOW_PROFILES
      if ( usePROFILES ) then
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sny
            do i = 1,snx
              prof_etan_mean(i,j,bi,bj)=offset+psmean(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
      _EXCH_XY_RL( prof_etan_mean, mythid )
      endif
#endif

#ifdef ALLOW_SSH_MEAN_COST_CONTRIBUTION
#ifndef ALLOW_SSH_TOT
c--   ==========
c--      Mean
c--   ==========
c--   compute mean ssh difference cost contribution
      call cost_ssh_mean(
     I                    psmean, offset
     O                  , costmean
     I                  , mythid
     &                  )


      objf_hmean = costmean
#endif /* ALLOW_SSH_TOT */
#endif /* ALLOW_SSH_MEAN_COST_CONTRIBUTION */

c--   ==========
c--   Anomalies.
c--   ==========

c--   Loop over records for the second time.
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

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
            do j = jmin,jmax
              do i = imin,imax
c--             The array psobs contains SSH anomalies.
                wwwtp2(i,j,bi,bj) = wwwtp1(i,j,bi,bj)
     &                             *wtp(i,j,bi,bj)
     &                             *tpmask(i,j,bi,bj)
     &                             *cosphi(i,j,bi,bj)
#ifndef ALLOW_SSH_TOT
                junk = ( psbar(i,j,bi,bj)
     &                   - psmeantp(i,j,bi,bj) - tpobs(i,j,bi,bj) )
                objf_tp(bi,bj) = objf_tp(bi,bj)
     &              + junk*junk*wwwtp2(i,j,bi,bj)
                if ( wwwtp2(i,j,bi,bj) .ne. 0. )
     &               num_tp(bi,bj) = num_tp(bi,bj) + 1. _d 0
#else
                if (tpmeanmask(i,j,bi,bj)*tpmask(i,j,bi,bj)
     &                  *wp(i,j,bi,bj)*wwwtp2(i,j,bi,bj) .ne.0.) then
                junk       = ( psbar(i,j,bi,bj) -
     &                 (tpobs(i,j,bi,bj)+tpmean(i,j,bi,bj)-offset) )
                objf_tp(bi,bj) = objf_tp(bi,bj)
     &              +junk*junk/( 1/wp(i,j,bi,bj)+1/wwwtp2(i,j,bi,bj) )
                num_tp(bi,bj) = num_tp(bi,bj) + 1. _d 0
                endif
#endif /* ALLOW_SSH_TOT */
              enddo
            enddo
#endif

#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
            do j = jmin,jmax
              do i = imin,imax
c--             The array ersobs contains SSH anomalies.
                wwwers2(i,j,bi,bj) = wwwers1(i,j,bi,bj)
     &                              *wers(i,j,bi,bj)
     &                              *ersmask(i,j,bi,bj)
     &                              *cosphi(i,j,bi,bj)
#ifndef ALLOW_SSH_TOT
                junk = ( psbar(i,j,bi,bj)
     &                   - psmeaners(i,j,bi,bj) - ersobs(i,j,bi,bj) )
                objf_ers(bi,bj) = objf_ers(bi,bj)
     &              + junk*junk*wwwers2(i,j,bi,bj)
                if ( wwwers2(i,j,bi,bj) .ne. 0. )
     &               num_ers(bi,bj) = num_ers(bi,bj) + 1. _d 0
#else
                if (tpmeanmask(i,j,bi,bj)*ersmask(i,j,bi,bj)
     &                  *wp(i,j,bi,bj)*wwwers2(i,j,bi,bj) .ne.0.) then
                junk       = ( psbar(i,j,bi,bj) -
     &                 (ersobs(i,j,bi,bj)+tpmean(i,j,bi,bj)-offset) )
                objf_ers(bi,bj) = objf_ers(bi,bj)
     &              +junk*junk/( 1/wp(i,j,bi,bj)+1/wwwers2(i,j,bi,bj) )
                num_ers(bi,bj) = num_ers(bi,bj) + 1. _d 0
                endif
#endif /* ALLOW_SSH_TOT */
              enddo
            enddo
#endif

#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
            do j = jmin,jmax
              do i = imin,imax
c--             The array gfoobs contains SSH anomalies.
                wwwgfo2(i,j,bi,bj) = wwwgfo1(i,j,bi,bj)
     &                        *wgfo(i,j,bi,bj)
     &                        *gfomask(i,j,bi,bj)
     &                        *cosphi(i,j,bi,bj)
#ifndef ALLOW_SSH_TOT
                junk = ( psbar(i,j,bi,bj)
     &                   - psmeangfo(i,j,bi,bj) - gfoobs(i,j,bi,bj) )
                objf_gfo(bi,bj) = objf_gfo(bi,bj)
     &              + junk*junk*wwwgfo2(i,j,bi,bj)
                if ( wwwgfo2(i,j,bi,bj) .ne. 0. )
     &               num_gfo(bi,bj) = num_gfo(bi,bj) + 1. _d 0
#else
                if (tpmeanmask(i,j,bi,bj)*gfomask(i,j,bi,bj)
     &                  *wp(i,j,bi,bj)*wwwgfo2(i,j,bi,bj) .ne.0.) then
                junk       = ( psbar(i,j,bi,bj) -
     &                 (gfoobs(i,j,bi,bj)+tpmean(i,j,bi,bj)-offset) )
                objf_gfo(bi,bj) = objf_gfo(bi,bj)
     &              +junk*junk/( 1/wp(i,j,bi,bj)+1/wwwgfo2(i,j,bi,bj) )
                num_gfo(bi,bj) = num_gfo(bi,bj) + 1. _d 0
                endif
#endif /* ALLOW_SSH_TOT */
              enddo
            enddo
#endif

          enddo
        enddo

      enddo
c--   End of second loop over records.

      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_h(bi,bj) = objf_h(bi,bj) +
     &        objf_tp(bi,bj) + objf_ers(bi,bj) + objf_gfo(bi,bj)
          num_h(bi,bj) = num_h(bi,bj) +
     &        num_tp(bi,bj) + num_ers(bi,bj) + num_gfo(bi,bj)
        enddo
      enddo


#endif /* ifdef ALLOW_SSH_COST_CONTRIBUTION */

      end
