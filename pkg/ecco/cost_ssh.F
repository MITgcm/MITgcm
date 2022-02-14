#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine cost_ssh(
     I                     myIter,
     I                     myTime,
     I                     myThid
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

#ifdef ALLOW_SSH_COST_CONTRIBUTION
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
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif
#endif

c     == routine arguments ==

      integer myIter
      _RL     myTime
      integer myThid

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

      logical doglobalread
      logical ladinit

      _RL offset
      _RL costmean
      _RL offset_sum
      _RL psmean ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL wwwtp  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy           )
      _RL wwwers ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy           )
      _RL wwwgfo ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy           )
      _RL junk

      character*(80) fname
      character*(MAX_LEN_MBUF) msgbuf

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--   Initialise local variables.
      costmean   = 0. _d 0

      do j = jmin, jmax
        do i = imin, imax
          wwwtp(i,j)  = 0. _d 0
          wwwers(i,j) = 0. _d 0
          wwwgfo(i,j) = 0. _d 0
        enddo
      enddo

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
              psmean(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

c--   Read mean field and generate mask
c--   Convert mean ssh from cm to m
#ifdef ALLOW_SSH_MEAN_COST_CONTRIBUTION
      CALL READ_REC_3D_RL( mdtdatfile, cost_iprec, 1,
     &                     mdt, 1, 1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              mdtmask(i,j,bi,bj)=maskC(i,j,1,bi,bj)
              if (mdt(i,j,bi,bj) .lt. -9990. _d 0) then
                mdtmask(i,j,bi,bj) = 0. _d 0
              endif
              if ( R_low(i,j,bi,bj) .GT. -200. ) then
                mdtmask(i,j,bi,bj) = 0. _d 0
              endif
              mdt(i,j,bi,bj) = mdt(i,j,bi,bj)*
     &                            mdtmask(i,j,bi,bj)*0.01 _d 0
            enddo
          enddo
        enddo
      enddo
#endif /* ALLOW_SSH_MEAN_COST_CONTRIBUTION */

c--   Loop over records for the first time.
      do irec = 1, ndaysrec

c--     Compute the mean over all psbar records.
        call active_read_xy( fname, psbar, irec, doglobalread,
     &                       ladinit, optimcycle, myThid,
     &                       xx_psbar_mean_dummy )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                psmean(i,j,bi,bj) = psmean(i,j,bi,bj) +
     &                psbar(i,j,bi,bj)/
     &                float(ndaysrec)
              enddo
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
          do j = 1,sNy
            do i = 1,sNx
              offset     = offset +
     &                     mdtmask(i,j,bi,bj)*cosphi(i,j,bi,bj)*
     &                     (mdt(i,j,bi,bj) - psmean(i,j,bi,bj))
              offset_sum = offset_sum +
     &                     mdtmask(i,j,bi,bj)*cosphi(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo

c--   Do a global summation.
      _GLOBAL_SUM_RL( offset     , myThid )
      _GLOBAL_SUM_RL( offset_sum , myThid )

      if (offset_sum .eq. 0.0) then
        _BEGIN_MASTER( myThid )
        write(msgbuf,'(a)') ' cost_ssh: offset_sum = zero!'
        call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , myThid)
        _END_MASTER( myThid )
cph        stop   '  ... stopped in cost_ssh.'
      else
        _BEGIN_MASTER( myThid )
        write(msgbuf,'(a,d22.15)')
     &          ' cost_ssh: offset_sum = ',offset_sum
        call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , myThid)
        _END_MASTER( myThid )
      endif

      offset = offset / offset_sum

#ifdef ALLOW_PROFILES
      if ( usePROFILES ) then
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sNy
            do i = 1,sNx
              prof_etan_mean(i,j,bi,bj)=offset+psmean(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
      _EXCH_XY_RL( prof_etan_mean, myThid )
      endif
#endif

#ifdef ALLOW_SSH_MEAN_COST_CONTRIBUTION
#ifndef ALLOW_SSH_TOT
c--   ==========
c--      Mean
c--   ==========
c--   compute mean ssh difference cost contribution
      objf_hmean = 0. _d 0
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              junk = psmean(i,j,bi,bj) - mdt(i,j,bi,bj) + offset
              objf_hmean = objf_hmean + junk*junk
     &                    * wp(i,j,bi,bj)*mdtmask(i,j,bi,bj)
              if ( wp(i,j,bi,bj)*mdtmask(i,j,bi,bj) .ne. 0. )
     &             num_hmean = num_hmean + 1.D0
            enddo
          enddo
        enddo
      enddo
      CALL GLOBAL_SUM_R8 (  objf_hmean,  myThid  )
      CALL GLOBAL_SUM_R8 (  num_hmean,  myThid  )
#endif /* ALLOW_SSH_TOT */
#endif /* ALLOW_SSH_MEAN_COST_CONTRIBUTION */

c--   ==========
c--   Anomalies.
c--   ==========

c--   Loop over records for the second time.
      do irec = 1, ndaysrec

        call active_read_xy( fname, psbar, irec, doglobalread,
     &                       ladinit, optimcycle, myThid,
     &                       xx_psbar_mean_dummy )

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
      call cost_sla_read( topexfile, topexstartdate, topexperiod,
     &                topexintercept, topexslope,
     &                tpobs, tpmask,
     &                irec, myThid )
#endif

#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
      call cost_sla_read( ersfile, ersstartdate, ersperiod,
     &                ersintercept, ersslope,
     &                ersobs, ersmask,
     &                irec, myThid )
#endif

#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
      call cost_sla_read( gfofile, gfostartdate, gfoperiod,
     &                gfointercept, gfoslope,
     &                gfoobs, gfomask,
     &                irec, myThid )
#endif

        do bj = jtlo,jthi
          do bi = itlo,ithi

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
            do j = jmin,jmax
              do i = imin,imax
c--             The array psobs contains SSH anomalies.
                wwwtp(i,j) = wtp(i,j,bi,bj) *cosphi(i,j,bi,bj)
#ifndef ALLOW_SSH_TOT
                junk       = ((psbar(i,j,bi,bj) - psmean(i,j,bi,bj)) -
     &                         tpobs(i,j,bi,bj))
     &                       *tpmask(i,j,bi,bj)
                objf_tp(bi,bj) = objf_tp(bi,bj)
     &              + junk*junk*wwwtp(i,j)
                if ( wwwtp(i,j)*junk .ne. 0. )
     &               num_tp(bi,bj) = num_tp(bi,bj) + 1. _d 0
#else
                if (mdtmask(i,j,bi,bj)*tpmask(i,j,bi,bj)
     &                  *wp(i,j,bi,bj)*wwwtp(i,j) .ne.0.) then
                junk       = ( psbar(i,j,bi,bj) -
     &                 (tpobs(i,j,bi,bj)+mdt(i,j,bi,bj)-offset) )
                objf_tp(bi,bj) = objf_tp(bi,bj)
     &              +junk*junk/( 1/wp(i,j,bi,bj)+1/wwwtp(i,j) )
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
                wwwers(i,j) = wers(i,j,bi,bj)*cosphi(i,j,bi,bj)
#ifndef ALLOW_SSH_TOT
                junk        = ((psbar(i,j,bi,bj) - psmean(i,j,bi,bj)) -
     &                         ersobs(i,j,bi,bj))
     &                        *ersmask(i,j,bi,bj)
                objf_ers(bi,bj) = objf_ers(bi,bj)
     &              + junk*junk*wwwers(i,j)
                if ( wwwers(i,j)*junk .ne. 0. )
     &               num_ers(bi,bj) = num_ers(bi,bj) + 1. _d 0
#else
                if (mdtmask(i,j,bi,bj)*ersmask(i,j,bi,bj)
     &                  *wp(i,j,bi,bj)*wwwers(i,j) .ne.0.) then
                junk       = ( psbar(i,j,bi,bj) -
     &                 (ersobs(i,j,bi,bj)+mdt(i,j,bi,bj)-offset) )
                objf_ers(bi,bj) = objf_ers(bi,bj)
     &              +junk*junk/( 1/wp(i,j,bi,bj)+1/wwwers(i,j) )
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
                wwwgfo(i,j) = wgfo(i,j,bi,bj)*cosphi(i,j,bi,bj)
#ifndef ALLOW_SSH_TOT
                junk        = ((psbar(i,j,bi,bj) - psmean(i,j,bi,bj)) -
     &                         gfoobs(i,j,bi,bj))
     &                        *gfomask(i,j,bi,bj)
                objf_gfo(bi,bj) = objf_gfo(bi,bj)
     &              + junk*junk*wwwgfo(i,j)
                if ( wwwgfo(i,j)*junk .ne. 0. )
     &               num_gfo(bi,bj) = num_gfo(bi,bj) + 1. _d 0
#else
                if (mdtmask(i,j,bi,bj)*gfomask(i,j,bi,bj)
     &                  *wp(i,j,bi,bj)*wwwgfo(i,j) .ne.0.) then
                junk       = ( psbar(i,j,bi,bj) -
     &                 (gfoobs(i,j,bi,bj)+mdt(i,j,bi,bj)-offset) )
                objf_gfo(bi,bj) = objf_gfo(bi,bj)
     &              +junk*junk/( 1/wp(i,j,bi,bj)+1/wwwgfo(i,j) )
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

      RETURN
      END
