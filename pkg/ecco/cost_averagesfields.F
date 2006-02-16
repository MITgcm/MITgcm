C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_averagesfields.F,v 1.8 2006/02/16 22:05:51 heimbach Exp $

#include "COST_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

      subroutine cost_averagesfields( mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_averagesfields
c     ==================================================================
c
c     o Compute time averages of etaN, theta, and salt. The counters
c       are explicitly calculated instead of being incremented. This
c       reduces dependencies. The latter is useful for the adjoint code
c       generation.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 24-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_averagesfields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"

#include "optim.h"
#include "ecco_cost.h"
#include "ctrl_dummy.h"
#ifdef ALLOW_EXF
# include "exf_fields.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE.h"
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==

      _RL     mytime
      integer mythid

c     == local variables ==

      integer myiter
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      logical first
      logical startofday
      logical startofmonth
      logical inday
      logical inmonth
      logical last
      logical endofday
      logical endofmonth

      integer ilps, ils,ilt

      character*(128) fnamepsbar
      character*(128) fnametbar
      character*(128) fnamesbar
      character*(128) fnameubar
      character*(128) fnamevbar
      character*(128) fnamewbar
      character*(128) fnametauxbar
      character*(128) fnametauybar
      character*(128) fnamehfluxbar
      character*(128) fnamesfluxbar

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

      myiter = niter0 + INT((mytime-starttime)/deltaTClock+0.5)
 
c--   Get the time flags and record numbers for the time averaging.

      call cost_averagesflags(
     I                    myiter,  mytime,     mythid,
     O                    first,   startofday, startofmonth,
     O                    inday,   inmonth,
     O                    last,    endofday,   endofmonth,
     O                    sum1day, dayrec,
     O                    sum1mon, monrec
     &                  )

#ifdef ALLOW_SSH_COST_CONTRIBUTION
      call cost_averagesgeneric(
     &     psbarfile, 
     &     psbar, etan, xx_psbar_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
#endif

#if (defined (ALLOW_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION) || \
     defined (ALLOW_ARGO_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
      call cost_averagesgeneric(
     &     tbarfile, 
     &     tbar, theta, xx_tbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, nr, mythid )
#else
#ifdef ALLOW_SST_COST_CONTRIBUTION
      call cost_averagesgeneric(
     &     tbarfile, 
     &     tbar, theta(1-Olx,1-Oly,1,1,1), xx_tbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, 1, mythid )
#endif
#endif

#if (defined (ALLOW_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION) || \
     defined (ALLOW_ARGO_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
      call cost_averagesgeneric(
     &     sbarfile, 
     &     sbar, salt, xx_sbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, nr, mythid )
#else
#ifdef ALLOW_SSS_COST_CONTRIBUTION
      call cost_averagesgeneric(
     &     sbarfile, 
     &     sbar, salt(1-Olx,1-Oly,1,1,1), xx_sbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, 1, mythid )
#endif  
#endif

#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
      call cost_averagesgeneric(
     &     wbarfile, 
     &     wbar, wvel, xx_wbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, nr, mythid )
#endif

#if (defined (ALLOW_DRIFTER_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
cph   There is a mismatch between the cost_drifer and the
cph   cost_obcs usage of ubar, vbar.
cph   cost_obcs refers to monthly means, cost_drifer to total mean.
cph   Needs to be updated for cost_obcs!!!.
c--   Next, do the averages for velocitty.
      if (first.or.startofmonth) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                  ubar(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)
                  vbar(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
      else if (last .or. endofmonth) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i = imin,imax
                  ubar(i,j,k,bi,bj)  = (ubar (i,j,k,bi,bj) +
     &                                  uVel(i,j,k,bi,bj)   )/
     &                                 float(sum1mon)
                  vbar(i,j,k,bi,bj)  = (vbar (i,j,k,bi,bj) +
     &                                  vVel(i,j,k,bi,bj)   )/
     &                                 float(sum1mon)
                enddo
              enddo
            enddo
          enddo
        enddo

c--     Save ubar and vbar.
        if (optimcycle .ge. 0) then
          ils=ilnblnk( ubarfile )
          write(fnameubar,'(2a,i10.10)') ubarfile(1:ils),'.',
     &                                             optimcycle
          write(fnamevbar,'(2a,i10.10)') vbarfile(1:ils),'.',
     &                                             optimcycle
        endif

        call active_write_xyz( fnameubar, ubar, monrec, optimcycle,
     &                         mythid, xx_ubar_mean_dummy)

        call active_write_xyz( fnamevbar, vbar, monrec, optimcycle,
     &                         mythid, xx_vbar_mean_dummy)

ce      , myiter, mytime )

      else if (       (       inmonth         ) .and.
     &          .not. (first .or. startofmonth) .and.
     &       .not. (last  .or. endofmonth  )      ) then
c--     Accumulate ubar and vbar.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                  ubar(i,j,k,bi,bj) = ubar (i,j,k,bi,bj) +
     &                                uVel (i,j,k,bi,bj)
                  vbar(i,j,k,bi,bj) = vbar (i,j,k,bi,bj) +
     &                                vVel (i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
      else
        stop   ' ... stopped in cost_averagesfields; ubar part.'
      endif
      
#endif

#ifdef ALLOW_SCAT_COST_CONTRIBUTION
c--   Next, do the averages for velocitty.
      if (first.or. startofmonth) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i =  imin,imax     
                  tauxbar(i,j,bi,bj) = ustress(i,j,bi,bj)
                  tauybar(i,j,bi,bj) = vstress(i,j,bi,bj)
                enddo
              enddo
          enddo
        enddo 
      else if (last .or. endofmonth) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i = imin,imax
                  tauxbar(i,j,bi,bj)  = (tauxbar (i,j,bi,bj) +
     &                                  ustress(i,j,bi,bj)   )/
     &                                 float(sum1mon)
                  tauybar(i,j,bi,bj)  = (tauybar (i,j,bi,bj) +
     &                                  vstress(i,j,bi,bj)   )/
     &                                 float(sum1mon)
                enddo
              enddo
          enddo
        enddo
     
c--     Save ubar and vbar.
        if (optimcycle .ge. 0) then
          ils=ilnblnk( tauxbarfile )
          write(fnametauxbar,'(2a,i10.10)') tauxbarfile(1:ils),'.',
     &                                             optimcycle
          ils=ilnblnk( tauybarfile )
          write(fnametauybar,'(2a,i10.10)') tauybarfile(1:ils),'.',
     &                                             optimcycle
        endif
     
        call active_write_xy( fnametauxbar, tauxbar, monrec, optimcycle,
     &                         mythid, xx_taux_mean_dummy)
     
        call active_write_xy( fnametauybar, tauybar, monrec, optimcycle,
     &                         mythid, xx_tauy_mean_dummy)
     

      else if ( .not. (first.or. startofmonth) .and.
     &          .not. (last .or. endofmonth)       ) then
c--     Accumulate ubar and vbar.
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i =  imin,imax
                  tauxbar(i,j,bi,bj) = tauxbar (i,j,bi,bj) +
     &                                ustress (i,j,bi,bj)
                  tauybar(i,j,bi,bj) = tauybar (i,j,bi,bj) +
     &                                vstress (i,j,bi,bj)
                enddo
              enddo
          enddo
        enddo
      else
        stop   ' ... stopped in cost_averagesfields; tauxbar part.'
      endif

     
#endif

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
c--   Next, do the averages for velocitty.
      if (first) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i =  imin,imax
                  hfluxbar(i,j,bi,bj)=hflux(i,j,bi,bj)
                enddo
              enddo
          enddo
        enddo
      else if (last) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i = imin,imax
                  hfluxbar(i,j,bi,bj)  = 
     &              ( hfluxbar (i,j,bi,bj) + hflux(i,j,bi,bj) )
     &              /float(nTimeSteps)
                enddo
              enddo
          enddo
        enddo

c--     Save hfluxbar 
        if (optimcycle .ge. 0) then
          ils=ilnblnk( hfluxbarfile )
          write(fnamehfluxbar,'(2a,i10.10)') hfluxbarfile(1:ils),'.',
     &                                             optimcycle
        endif
    
        call active_write_xy( fnamehfluxbar, hfluxbar, 1,
     &       optimcycle,   mythid, xx_hflux_mean_dummy)
    
      else if ( .not. (first) .and.
     &          .not. (last )       ) then
c--     Accumulate ubar and vbar.
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i =  imin,imax
                  hfluxbar(i,j,bi,bj) = 
     &              hfluxbar(i,j,bi,bj) + hflux(i,j,bi,bj)
                enddo
            enddo
          enddo 
       enddo
      else
        stop   ' ... stopped in cost_averagesfields; hfluxbar part.'
      endif

#endif

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
c--   Next, do the averages for velocitty.
      if (first ) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i =  imin,imax
                  sfluxbar(i,j,bi,bj) = sflux(i,j,bi,bj)
                enddo
              enddo
          enddo
        enddo
      else if (last) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i = imin,imax
                  sfluxbar(i,j,bi,bj)  = (sfluxbar (i,j,bi,bj) +
     &                                  sflux(i,j,bi,bj)   )/
     &                                 float(nTimeSteps)
                enddo
              enddo
          enddo
        enddo


c--     Save sfluxbar
        if (optimcycle .ge. 0) then
          ils=ilnblnk( sfluxbarfile )
          write(fnamesfluxbar,'(2a,i10.10)') sfluxbarfile(1:ils),'.',
     &                                             optimcycle
        endif

        call active_write_xy( fnamesfluxbar, sfluxbar, 1,
     &       optimcycle,  mythid, xx_sflux_mean_dummy)

      else if ( .not. (first) .and.
     &          .not. (last )       ) then
c--     Accumulate ubar and vbar.
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i =  imin,imax
                  sfluxbar(i,j,bi,bj) = sfluxbar (i,j,bi,bj) +
     &                                sflux (i,j,bi,bj)
                enddo
              enddo
          enddo
        enddo
      else
        print*
        stop   ' ... stopped in cost_averagesfields; sfluxbar part.'
      endif

#endif

#ifdef ALLOW_SEAICE

# ifdef ALLOW_SEAICE_COST_SMR_AREA
      call cost_averagesgeneric(
     &     smrareabarfile, 
     &     smrareabar, area, xx_smrareabar_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
# endif

#endif /* ALLOW_SEAICE */

      return
      end

