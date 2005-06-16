C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_cost_weights.F,v 1.10 2005/06/16 15:28:46 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine ecco_cost_weights( mythid )

c     ==================================================================
c     SUBROUTINE ecco_cost_weights
c     ==================================================================
c
c     o Read the weights used for the cost function evaluation.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu 02-May-2000
c
c              - corrected typo in mdsreadfield( sflux_errfile );
c                wp --> wsflux. Spotted by Patrick Heimbach.
c
c     ==================================================================
c     SUBROUTINE ecco_cost_weights
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ctrl.h"
#include "ecco_cost.h"

c     == routine arguments ==

      integer  mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer gwunit
      integer irec,nnz
      integer ilo,ihi
      integer iobcs

      _RL factor
      _RL wti(nr)
      _RL wsi(nr)
      _RL wui(nr)
      _RL wvi(nr)
      _RL whflux0
      _RL whflux0m
      _RL wsflux0
      _RL wsflux0m
      _RL wtau0
      _RL wtau0m
      _RL watemp0
      _RL waqh0
      _RL wwind0
      _RL ratio
      _RL dummy

c     == external ==

      integer  ifnblnk
      external ifnblnk
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

c--   Initialize background weights
      wtau0    = 0.
      whflux0  = 0.
      wsflux0  = 0.
      whflux0m = 0
      wsflux0m = 0.
      watemp0  = 0.
      waqh0    = 0.
      wwind0   = 0.

c--   Initialize variance (weight) fields.
      do k = 1,nr
         wti(k) = 0. _d 0
         wsi(k) = 0. _d 0
         wui(k) = 0. _d 0
         wvi(k) = 0. _d 0
      enddo
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              whflux  (i,j,bi,bj) = 0. _d 0
              whfluxm (i,j,bi,bj) = 0. _d 0
              wsflux  (i,j,bi,bj) = 0. _d 0
              wsfluxm (i,j,bi,bj) = 0. _d 0
              wtauu   (i,j,bi,bj) = 0. _d 0
              wtauum  (i,j,bi,bj) = 0. _d 0
              wtauv   (i,j,bi,bj) = 0. _d 0
              wtauvm  (i,j,bi,bj) = 0. _d 0
              watemp  (i,j,bi,bj) = 0. _d 0
              waqh    (i,j,bi,bj) = 0. _d 0
              wuwind  (i,j,bi,bj) = 0. _d 0
              wvwind  (i,j,bi,bj) = 0. _d 0
              wsst    (i,j,bi,bj) = 0. _d 0
              wsss    (i,j,bi,bj) = 0. _d 0
              wtp     (i,j,bi,bj) = 0. _d 0
              wers    (i,j,bi,bj) = 0. _d 0
              wp      (i,j,bi,bj) = 0. _d 0
              wudrift (i,j,bi,bj) = 0. _d 0
              wvdrift (i,j,bi,bj) = 0. _d 0
cph(
	      whflux2 (i,j,bi,bj) = 0. _d 0
	      wsflux2 (i,j,bi,bj) = 0. _d 0
	      wtauu2  (i,j,bi,bj) = 0. _d 0
	      wtauv2  (i,j,bi,bj) = 0. _d 0
cph)
            enddo
          enddo
        enddo
      enddo
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
            wtheta (k,bi,bj) = 0. _d 0
            wsalt  (k,bi,bj) = 0. _d 0
            wctdt  (k,bi,bj) = 0. _d 0
            wctds  (k,bi,bj) = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                wtheta2 (i,j,k,bi,bj) = 0. _d 0
                wsalt2  (i,j,k,bi,bj) = 0. _d 0
                wthetaLev (i,j,k,bi,bj) = 0. _d 0
                wsaltLev  (i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

#if (defined (ALLOW_OBCS_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_CONTROL))
      do iobcs = 1,nobcs
        do k = 1,Nr
#if (defined (ALLOW_OBCSN_CONTROL) || \
     defined (ALLOW_OBCSN_COST_CONTRIBUTION))
          wobcsn(k,iobcs) = 0. _d 0
#endif
#if (defined (ALLOW_OBCSS_CONTROL) || \
     defined (ALLOW_OBCSS_COST_CONTRIBUTION))
          wobcss(k,iobcs) = 0. _d 0
#endif
#if (defined (ALLOW_OBCSW_CONTROL) || \
     defined (ALLOW_OBCSW_COST_CONTRIBUTION))
          wobcsw(k,iobcs) = 0. _d 0
#endif
#if (defined (ALLOW_OBCSE_CONTROL) || \
     defined (ALLOW_OBCSE_COST_CONTRIBUTION))
          wobcse(k,iobcs) = 0. _d 0
#endif
        enddo
      enddo
#endif

c--   Build area weighting matrix used in the cost function
c--   contributions.

c--   Define frame.
      do j = jmin,jmax
        do i = imin,imax
c--       North/South and West/East edges set to zero.
          if ( (j .lt. 1) .or. (j .gt. sny) .or.
     &         (i .lt. 1) .or. (i .gt. snx)      ) then
            frame(i,j) = 0. _d 0
          else
            frame(i,j) = 1. _d 0
          endif
        enddo
      enddo

c--   First account for the grid used.
      if (usingCartesianGrid) then
        factor = 0. _d 0
      else if (usingSphericalPolarGrid) then
        factor = 1. _d 0
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
cds              cosphi(i,j,bi,bj) = cos(yc(i,j,bi,bj)*deg2rad*factor)*
cds     &                            frame(i,j)
              cosphi(i,j,bi,bj) = frame(i,j)
            enddo
          enddo
        enddo
      enddo

c--   Read error information and set up weight matrices.
      _BEGIN_MASTER(myThid)
        call mdsfindunit( gwunit, mythid )
        ilo = ifnblnk(data_errfile)
        ihi = ilnblnk(data_errfile)
        open(gwunit, file=data_errfile(ilo:ihi),
     &               status='old', form='formatted')

        read(gwunit,*) 
#if (defined (ALLOW_HFLUX_COST_CONTRIBUTION) || defined (ALLOW_HFLUX_CONTROL))
     &         whflux0
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION) || defined (ALLOW_ATEMP_CONTROL))
     &         watemp0
#endif
#if (defined (ALLOW_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SFLUX_CONTROL))
     &       , wsflux0
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION) || defined (ALLOW_AQH_CONTROL))
     &       , waqh0
#endif
#if (defined (ALLOW_USTRESS_COST_CONTRIBUTION) || defined (ALLOW_USTRESS_CONTROL))
     &       , wtau0
#elif (defined (ALLOW_UWIND_COST_CONTRIBUTION) || defined (ALLOW_UWIND_CONTROL))
     &       , wwind0
#endif
     &       , ratio
#if (defined (ALLOW_OBCS_COST_CONTRIBUTION) || defined (ALLOW_OBCS_CONTROL))
     &       , wbaro
#endif
        do k = 1,nr
          read(gwunit,*) wti(k), wsi(k)
#if (defined (ALLOW_OBCS_COST_CONTRIBUTION) || defined (ALLOW_OBCS_CONTROL))
     &               , wvi(k)
#endif
        end do
        close(gwunit)

        whflux0m = whflux0
        wsflux0m = wsflux0
        wtau0m   = wtau0
      _END_MASTER(myThid)

      _BARRIER

cph      jmin = 1
cph      jmax = sny
cph      imin = 1
cph      imax = snx

      do bj = jtlo,jthi
        do bi = itlo,ithi

          wsfluxmm(bi,bj) = 1.
          whfluxmm(bi,bj) = 1.

c--       The "classic" state estimation tool wastes memory here;
c--       as long as there is not more information available there
c--       is no need to add the zonal and meridional directions.
          do k = 1,nr
            wtheta(k,bi,bj)   = wti(k)
            wsalt (k,bi,bj)   = wsi(k)
            wcurrent(k,bi,bj) = wvi(k)
          enddo

          do k = 1,nr
#ifdef ALLOW_OBCSN_COST_CONTRIBUTION
            wobcsn(k,1) = wti(k)
            wobcsn(k,2) = wsi(k)
            wobcsn(k,3) = wti(k)*0.02
            wobcsn(k,4) = wti(k)*0.02
#endif
#ifdef ALLOW_OBCSS_COST_CONTRIBUTION
            wobcss(k,1) = wti(k)
            wobcss(k,2) = wsi(k)
            wobcss(k,3) = wti(k)*0.02
            wobcss(k,4) = wti(k)*0.02
#endif
#ifdef ALLOW_OBCSW_COST_CONTRIBUTION
            wobcsw(k,1) = wti(k)
            wobcsw(k,2) = wsi(k)
            wobcsw(k,3) = wti(k)*0.02
            wobcsw(k,4) = wti(k)*0.02
#endif
#ifdef ALLOW_OBCSE_COST_CONTRIBUTION
            wobcse(k,1) = wti(k)
            wobcse(k,2) = wsi(k)
            wobcse(k,3) = wti(k)*0.02
            wobcse(k,4) = wti(k)*0.02
#endif
          enddo
        enddo
      enddo

#if (defined (ALLOW_ARGO_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION)|| \
     defined (ALLOW_CTDSCLIM_COST_CONTRIBUTION))

      if ( salterrfile .NE. ' ' ) then
         call mdsreadfield( salterrfile, 32, 'RL', Nr, wsalt2, 1,
     &                      mythid)

         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
c--           Test for missing values.
              if (wsalt(k,bi,bj).eq.0 .and. 
     $            wsalt2(i,j,k,bi,bj).eq.0) then
                 wsalt2(i,j,k,bi,bj) = 0. _d 0
              else
                 wsalt2(i,j,k,bi,bj)=frame(i,j)/
     $                  ( wsalt2(i,j,k,bi,bj)*wsalt2(i,j,k,bi,bj) )
              endif
             enddo
            enddo
           enddo
          enddo
         enddo
      else
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
                wsalt2(i,j,k,bi,bj)=ratio/(wsalt(k,bi,bj)
     $               *wsalt(k,bi,bj)) *frame(i,j)
             enddo
            enddo
           enddo
          enddo
         enddo            
      endif
#endif

#if (defined (ALLOW_ARGO_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDTCLIM_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION))

      if ( temperrfile .NE. ' ' ) then
         call mdsreadfield( temperrfile, 32, 'RL', Nr, wtheta2, 1,
     &                      mythid)
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
c--           Test for missing values.
              if (wtheta(k,bi,bj).eq.0 .and. 
     $            wtheta2(i,j,k,bi,bj).eq.0) then
                 wtheta2(i,j,k,bi,bj) = 0. _d 0
              else
                 wtheta2(i,j,k,bi,bj)= frame(i,j)/
     $                  ( wtheta2(i,j,k,bi,bj)*wtheta2(i,j,k,bi,bj) )
              endif
             enddo
            enddo
           enddo
          enddo
         enddo
      else
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
              if (wtheta(k,bi,bj).eq.0 ) then
                 wtheta2(i,j,k,bi,bj) = 0. _d 0
              else                
                 wtheta2(i,j,k,bi,bj) = ratio/(wtheta(k,bi,bj)
     $                *wtheta(k,bi,bj))*frame(i,j)
              endif
             enddo
            enddo
           enddo
          enddo
         enddo            
      endif
#endif

      k = 1
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                wsst(i,j,bi,bj) = 0. _d 0
                wsss(i,j,bi,bj) = 0. _d 0
              else
cph                wsst(i,j,bi,bj) = wtheta(k,bi,bj)*10.
cph                wsss(i,j,bi,bj) = wsalt(k,bi,bj)*10.
cph       factor 5. by D Stammer
                wsst(i,j,bi,bj) = wtheta(k,bi,bj)*frame(i,j)
                wsss(i,j,bi,bj) = wsalt(k,bi,bj)*frame(i,j)
              endif
            enddo
          enddo
        enddo
      enddo
#ifdef ALLOW_EGM96_ERROR_DIAG
c--   Read egm-96 geoid covariance. Data in units of meters.
      nnz   =  1
      irec  =  1
      call mdsreadfield( geoid_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wp, irec, mythid )
c--   Set all tile edges to zero.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              wp(i,j,bi,bj) = wp(i,j,bi,bj)*frame(i,j)
cph-indonesian(
              if ( xC(i,j,bi,bj) .GT. 120. .AND.
     &             xC(i,j,bi,bj) .LT. 130. .AND.
     &             yC(i,j,bi,bj) .GT. -10. .AND.
     &             yC(i,j,bi,bj) .LT.  10. ) then
                 wp(i,j,bi,bj) = wp(i,j,bi,bj)*100.
              endif
cph-indonesian)
            enddo
          enddo
        enddo
      enddo
#else
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              wp(i,j,bi,bj) = frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SSH_COST_CONTRIBUTION
c--   Read T/P SSH anomaly rms field. Data in units of centimeters.
      nnz   =   1
      irec  =   1
      call mdsreadfield( ssh_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wtp, irec, mythid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Unit conversion to meters. ERS error is set to
c--           T/P error + 5cm
              if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                wtp (i,j,bi,bj) = 0. _d 0
                wers(i,j,bi,bj) = 0. _d 0
              else
                wtp (i,j,bi,bj) = ( wtp(i,j,bi,bj) * 0.01 * 0.5 )
     &                            *frame(i,j)
                wers(i,j,bi,bj) = ( wtp(i,j,bi,bj) + 0.05 )
     &                            *frame(i,j)
              endif
cph-indonesian(
              if ( xC(i,j,bi,bj) .GT. 120. .AND.
     &             xC(i,j,bi,bj) .LT. 130. .AND.
     &             yC(i,j,bi,bj) .GT. -10. .AND.
     &             yC(i,j,bi,bj) .LT.  10. ) then
                 wtp(i,j,bi,bj)  = wtp(i,j,bi,bj)*100.
                 wers(i,j,bi,bj) = wers(i,j,bi,bj)*100.
              endif
cph-indonesian)
            enddo
          enddo
        enddo
      enddo
#endif /* ALLOW_SSH_COST_CONTRIBUTION */

c--   Read zonal wind stress variance.
#if (defined (ALLOW_SCAT_COST_CONTRIBUTION))

      nnz   =   1
      irec  =   1
      call mdsreadfield( scatx_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wscatx, irec, mythid )
      call mdsreadfield( scaty_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wscaty, irec, mythid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wscatx(i,j,bi,bj) .lt. -9900.) then
                wscatx(i,j,bi,bj) = 0. _d 0
              endif
              wscatx(i,j,bi,bj) = wscatx(i,j,bi,bj)
              wscatx(i,j,bi,bj) = max(wscatx(i,j,bi,bj),wtau0)
              wscatx(i,j,bi,bj) = wscatx(i,j,bi,bj)*maskw(i,j,k,bi,bj)
     &                            *frame(i,j)
              if (wscaty(i,j,bi,bj) .lt. -9900.) then
                wscaty(i,j,bi,bj) = 0. _d 0
              endif
              wscaty(i,j,bi,bj) = wscaty(i,j,bi,bj)
              wscaty(i,j,bi,bj) = max(wscaty(i,j,bi,bj),wtau0)
              wscaty(i,j,bi,bj) = wscaty(i,j,bi,bj)*masks(i,j,k,bi,bj)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo

#endif

c--   Read zonal wind stress variance.
#if (defined (ALLOW_STRESS_MEAN_COST_CONTRIBUTION))
      nnz   =   1
      irec  =   1
cph      call mdsreadfield( tauum_errfile, cost_iprec, cost_yftype,
cph     &                   nnz, wtauum, irec, mythid )
cph      call mdsreadfield( tauvm_errfile, cost_iprec, cost_yftype,
cph     &                   nnz, wtauvm, irec, mythid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wtauum(i,j,bi,bj) .lt. -9900.) then
                wtauum(i,j,bi,bj) = 0. _d 0
              endif
              wtauum(i,j,bi,bj) = max(wtauum(i,j,bi,bj),wtau0m)
     &                            *frame(i,j)
c--           Test for missing values.
              if (wtauvm(i,j,bi,bj) .lt. -9900.) then
                wtauvm(i,j,bi,bj) = 0. _d 0
              endif
              wtauvm(i,j,bi,bj) = max(wtauvm(i,j,bi,bj),wtau0m)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_USTRESS_COST_CONTRIBUTION))
      nnz   =   1
ce      irec  =   2
ce(   due to Patrick's processing:
      irec  = 1
ce)
      call mdsreadfield( tauu_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wtauu, irec, mythid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wtauu(i,j,bi,bj) .lt. -9900.) then
                wtauu(i,j,bi,bj) = 0. _d 0
              endif
              wtauu(i,j,bi,bj) = wtauu(i,j,bi,bj)*0.1
              wtauu(i,j,bi,bj) = max(wtauu(i,j,bi,bj),wtau0)
              wtauu(i,j,bi,bj) = wtauu(i,j,bi,bj)*maskw(i,j,k,bi,bj)
     &                            *frame(i,j)
cph(
              wtauu2(i,j,bi,bj) = wtau0*maskW(i,j,k,bi,bj)*frame(i,j)
cph)
             enddo
          enddo
        enddo
      enddo

#elif (defined (ALLOW_UWIND_COST_CONTRIBUTION))

      nnz   =   1
ce      irec  =   2
ce(   due to Patrick's processing:
      irec  = 1
ce)
      if ( uwind_errfile .NE. ' ' ) then
         call mdsreadfield( uwind_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wuwind, irec, mythid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wuwind(i,j,bi,bj) .lt. -9900.) then
                wuwind(i,j,bi,bj) = 0. _d 0
              endif
              wuwind(i,j,bi,bj) = wuwind(i,j,bi,bj)
              wuwind(i,j,bi,bj) = max(wuwind(i,j,bi,bj),wwind0)
              wuwind(i,j,bi,bj) = wuwind(i,j,bi,bj)*maskc(i,j,k,bi,bj)
     &                            *frame(i,j)
             enddo
          enddo
        enddo
      enddo
#endif

c--   Read meridional wind stress variance.
#if (defined (ALLOW_VSTRESS_COST_CONTRIBUTION))
      nnz   =   1
ce      irec  =   2
ce(   due to Patrick's processing:
      irec  = 1
ce)

      call mdsreadfield( tauv_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wtauv, irec, mythid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wtauv(i,j,bi,bj) .lt. -9900.) then
                wtauv(i,j,bi,bj) = 0. _d 0
              endif
              wtauv(i,j,bi,bj) = wtauv(i,j,bi,bj)*0.1
              wtauv(i,j,bi,bj) = max(wtauv(i,j,bi,bj),wtau0)
              wtauv(i,j,bi,bj) = wtauv(i,j,bi,bj)*masks(i,j,k,bi,bj)
     &                            *frame(i,j)
cph(
              wtauv2(i,j,bi,bj) = wtau0*maskS(i,j,k,bi,bj)*frame(i,j)
cph)
            enddo
          enddo
        enddo
      enddo

#elif (defined (ALLOW_VWIND_COST_CONTRIBUTION))

      nnz   =   1
ce      irec  =   2
ce(   due to Patrick's processing:
      irec  = 1
ce)

      if ( vwind_errfile .NE. ' ' ) then
         call mdsreadfield( vwind_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wvwind, irec, mythid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wvwind(i,j,bi,bj) .lt. -9900.) then
                wvwind(i,j,bi,bj) = 0. _d 0
              endif
              wvwind(i,j,bi,bj) = wvwind(i,j,bi,bj)
              wvwind(i,j,bi,bj) = max(wvwind(i,j,bi,bj),wwind0)
              wvwind(i,j,bi,bj) = wvwind(i,j,bi,bj)*maskc(i,j,k,bi,bj)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_HFLUX_COST_CONTRIBUTION))
c--   Read heat flux flux variance.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick's processing:
      irec  = 1
ce)
      call mdsreadfield( hflux_errfile, cost_iprec, cost_yftype, nnz, 
     &                   whflux, irec, mythid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (whflux(i,j,bi,bj) .lt. -9900.) then
                whflux(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of W/m**2.
              whflux(i,j,bi,bj) = whflux(i,j,bi,bj)/3.
              whflux(i,j,bi,bj) = max(whflux(i,j,bi,bj),whflux0)
     &                            *frame(i,j)
              whfluxm(i,j,bi,bj) = max(whfluxm(i,j,bi,bj),whflux0m)
     &                            *frame(i,j)
cph(
              whflux2(i,j,bi,bj) = whflux0*frame(i,j)
cph)
            enddo
          enddo
        enddo
      enddo
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION))
c--   Read atmos. temp. variance.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick's processing:
      irec  = 1
ce)
      if ( atemp_errfile .NE. ' ' ) then
         call mdsreadfield( atemp_errfile, cost_iprec, cost_yftype, nnz, 
     &                   watemp, irec, mythid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (watemp(i,j,bi,bj) .lt. -9900.) then
                watemp(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of W/m**2.
              watemp(i,j,bi,bj) = watemp(i,j,bi,bj)
              watemp(i,j,bi,bj) = max(watemp(i,j,bi,bj),watemp0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo


#endif

#if (defined (ALLOW_SFLUX_COST_CONTRIBUTION))
c--   Read salt flux variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick's processing:
      irec  = 1
ce)
      call mdsreadfield( sflux_errfile, cost_iprec, cost_yftype, nnz, 
     &                   wsflux, irec, mythid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wsflux(i,j,bi,bj) .lt. -9900.) then
                wsflux(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wsflux(i,j,bi,bj) = wsflux(i,j,bi,bj) / 3.
              wsflux(i,j,bi,bj) = max(wsflux(i,j,bi,bj),wsflux0)
     &                            *frame(i,j)
              wsfluxm(i,j,bi,bj) = max(wsfluxm(i,j,bi,bj),wsflux0m)
     &                            *frame(i,j)
cph(
              wsflux2(i,j,bi,bj) = wsflux0*frame(i,j)
cph)
            enddo
          enddo
        enddo
      enddo
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION))
c--   Secific humid. variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick's processing:
      irec  = 1
ce)
      if ( aqh_errfile .NE. ' ' ) then
         call mdsreadfield( aqh_errfile, cost_iprec, cost_yftype, nnz, 
     &                   waqh, irec, mythid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (waqh(i,j,bi,bj) .lt. -9900.) then
                waqh(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              waqh(i,j,bi,bj) = waqh(i,j,bi,bj)
              waqh(i,j,bi,bj) = max(waqh(i,j,bi,bj),waqh0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

c--   Units have to be checked!
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              if (wtp(i,j,bi,bj) .ne. 0.) then
                wtp (i,j,bi,bj) = 1./wtp(i,j,bi,bj)/wtp(i,j,bi,bj)
              endif
              if (wers(i,j,bi,bj) .ne. 0.) then
                wers(i,j,bi,bj) = 1./wers(i,j,bi,bj)/wers(i,j,bi,bj)
              endif
              if (wsst(i,j,bi,bj) .ne. 0.) then
                wsst(i,j,bi,bj) = 1./wsst(i,j,bi,bj)/wsst(i,j,bi,bj)
              endif
              if (wsss(i,j,bi,bj) .ne. 0.) then
                wsss(i,j,bi,bj) = 1./wsss(i,j,bi,bj)/wsss(i,j,bi,bj)
              endif
              if (wp(i,j,bi,bj) .ne. 0.) then
                wp(i,j,bi,bj) = 1./wp(i,j,bi,bj)/wp(i,j,bi,bj)
              endif
              if (wtauu(i,j,bi,bj) .ne. 0.) then
                wtauu(i,j,bi,bj) = 1./wtauu(i,j,bi,bj)/wtauu(i,j,bi,bj)
              else
                wtauu(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauum(i,j,bi,bj) .ne. 0.) then
                wtauum(i,j,bi,bj) = 
     &            1./wtauum(i,j,bi,bj)/wtauum(i,j,bi,bj)
              else
                wtauum(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wscatx(i,j,bi,bj) .ne. 0.) then
                wscatx(i,j,bi,bj) = 
     &            1./wscatx(i,j,bi,bj)/wscatx(i,j,bi,bj)
              else
                wscatx(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauv(i,j,bi,bj) .ne. 0.) then
                wtauv(i,j,bi,bj) = 1./wtauv(i,j,bi,bj)/wtauv(i,j,bi,bj)
              else
                wtauv(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauvm(i,j,bi,bj) .ne. 0.) then
                wtauvm(i,j,bi,bj) =
     &           1./wtauvm(i,j,bi,bj)/wtauvm(i,j,bi,bj)
              else
                wtauvm(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wscaty(i,j,bi,bj) .ne. 0.) then
                wscaty(i,j,bi,bj) =
     &           1./wscaty(i,j,bi,bj)/wscaty(i,j,bi,bj)
              else
                wscaty(i,j,bi,bj) = 0.0 _d 0
              endif
              if (whflux(i,j,bi,bj) .ne. 0.) then
                whflux(i,j,bi,bj) =
     &                1./whflux(i,j,bi,bj)/whflux(i,j,bi,bj)
              else
                whflux(i,j,bi,bj) = 0.0 _d 0
              endif
              if (whfluxm(i,j,bi,bj) .ne. 0.) then
                whfluxm(i,j,bi,bj) = 
     &                1./whfluxm(i,j,bi,bj)/whfluxm(i,j,bi,bj)
              else    
                whfluxm(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wsflux(i,j,bi,bj) .ne. 0.) then
                wsflux(i,j,bi,bj) =
     &                1./wsflux(i,j,bi,bj)/wsflux(i,j,bi,bj)
              else
                wsflux(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wsfluxm(i,j,bi,bj) .ne. 0.) then
                wsfluxm(i,j,bi,bj) = 
     &                1./wsfluxm(i,j,bi,bj)/wsfluxm(i,j,bi,bj)
              else
                wsfluxm(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wuwind(i,j,bi,bj) .ne. 0.) then
                wuwind(i,j,bi,bj) = 
     &                1./wuwind(i,j,bi,bj)/wuwind(i,j,bi,bj)
              else
                wuwind(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wvwind(i,j,bi,bj) .ne. 0.) then
                wvwind(i,j,bi,bj) = 
     &                1./wvwind(i,j,bi,bj)/wvwind(i,j,bi,bj)
              else
                wvwind(i,j,bi,bj) = 0.0 _d 0
              endif
              if (watemp(i,j,bi,bj) .ne. 0.) then
                watemp(i,j,bi,bj) =
     &                1./watemp(i,j,bi,bj)/watemp(i,j,bi,bj)
              else
                watemp(i,j,bi,bj) = 0.0 _d 0
              endif
              if (waqh(i,j,bi,bj) .ne. 0.) then
                waqh(i,j,bi,bj) =
     &                1./waqh(i,j,bi,bj)/waqh(i,j,bi,bj)
              else
                waqh(i,j,bi,bj) = 0.0 _d 0
              endif

              if (wsfluxmm(bi,bj).ne.0.) 
     &             wsfluxmm(bi,bj) = 1./wsfluxmm(bi,bj)*wsfluxmm(bi,bj)
              if (whfluxmm(bi,bj).ne.0.) 
     &             whfluxmm(bi,bj) = 1./whfluxmm(bi,bj)*whfluxmm(bi,bj)

cph(
              if (whflux2(i,j,bi,bj) .ne. 0.) then
                 whflux2(i,j,bi,bj) =
     &                1./whflux2(i,j,bi,bj)/whflux2(i,j,bi,bj)
              else
                 whflux2(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wsflux2(i,j,bi,bj) .ne. 0.) then
                 wsflux2(i,j,bi,bj) =
     &                1./wsflux2(i,j,bi,bj)/wsflux2(i,j,bi,bj)
              else
                 wsflux2(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauu2(i,j,bi,bj) .ne. 0.) then
                 wtauu2(i,j,bi,bj) = 
     &                1./wtauu2(i,j,bi,bj)/wtauu2(i,j,bi,bj)
              else
                 wtauu2(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauv2(i,j,bi,bj) .ne. 0.) then
                 wtauv2(i,j,bi,bj) =
     &                1./wtauv2(i,j,bi,bj)/wtauv2(i,j,bi,bj)
              else
                 wtauv2(i,j,bi,bj) = 0.0 _d 0
              endif
cph)
            enddo
          enddo

          do k = 1,nr
            if (wtheta(k,bi,bj) .ne. 0.) then
              wtheta(k,bi,bj) = ratio/wtheta(k,bi,bj)/wtheta(k,bi,bj)
            else
              wtheta(k,bi,bj) = 0.0 _d 0
            endif
            if (wsalt(k,bi,bj) .ne. 0.) then
              wsalt(k,bi,bj) = ratio/wsalt(k,bi,bj)/wsalt(k,bi,bj)
            else
              wsalt(k,bi,bj) = 0.0 _d 0
            endif
          enddo

#ifdef ALLOW_OBCS_COST_CONTRIBUTION
          do iobcs = 1,nobcs
            do k = 1,nr
#ifdef ALLOW_OBCSN_COST_CONTRIBUTION
              if (wobcsn(k,iobcs) .ne. 0.) then
                 wobcsn(k,iobcs) = 
     &                ratio/wobcsn(k,iobcs)/wobcsn(k,iobcs)
              else
                 wobcsn(k,iobcs) = 0.0 _d 0
              endif
#endif
#ifdef ALLOW_OBCSS_COST_CONTRIBUTION
              if (wobcss(k,iobcs) .ne. 0.) then
                 wobcss(k,iobcs) = 
     &                ratio/wobcss(k,iobcs)/wobcss(k,iobcs)
              else
                 wobcss(k,iobcs) = 0.0 _d 0
              endif
#endif
#ifdef ALLOW_OBCSW_COST_CONTRIBUTION
              if (wobcsw(k,iobcs) .ne. 0.) then
                 wobcsw(k,iobcs) = 
     &                ratio/wobcsw(k,iobcs)/wobcsw(k,iobcs)
              else
                 wobcsw(k,iobcs) = 0.0 _d 0
              endif
#endif
#ifdef ALLOW_OBCSE_COST_CONTRIBUTION
              if (wobcse(k,iobcs) .ne. 0.) then
                 wobcse(k,iobcs) = 
     &                ratio/wobcse(k,iobcs)/wobcse(k,iobcs)
              else
                 wobcse(k,iobcs) = 0.0 _d 0
              endif
#endif
            enddo
          enddo
#endif

        enddo
      enddo

c
c     write masks and weights to files to be read by a master process
c
      call active_write_xyz_loc( 'maskCtrlC', maskC, 
     &     1, 0, mythid, dummy)
      call active_write_xyz_loc( 'maskCtrlW', maskW, 
     &     1, 0, mythid, dummy)
      call active_write_xyz_loc( 'maskCtrlS', maskS, 
     &     1, 0, mythid, dummy)

#if   (defined (ALLOW_HFLUX_COST_CONTRIBUTION))
      call active_write_xy_loc( 'whflux', whflux, 1, 0, mythid, dummy)
      call active_write_xy_loc( 'whflux2', whflux2, 1, 0, mythid, dummy)
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION))
      call active_write_xy_loc( 'watemp', watemp, 1, 0, mythid, dummy)
#endif

#if   (defined (ALLOW_SFLUX_COST_CONTRIBUTION))
      call active_write_xy_loc( 'wsflux', wsflux, 1, 0, mythid, dummy)
      call active_write_xy_loc( 'wsflux2', wsflux2, 1, 0, mythid, dummy)
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION))
      call active_write_xy_loc( 'waqh', waqh, 1, 0, mythid, dummy)
#endif

#if   (defined (ALLOW_USTRESS_COST_CONTRIBUTION))
      call active_write_xy_loc( 'wtauu', wtauu,   1, 0, mythid, dummy)
      call active_write_xy_loc( 'wtauu2', wtauu2,   1, 0, mythid, dummy)
#elif (defined (ALLOW_UWIND_COST_CONTRIBUTION))
      call active_write_xy_loc( 'wuwind', wuwind, 1, 0, mythid, dummy)
#endif

#if   (defined (ALLOW_VSTRESS_COST_CONTRIBUTION))
      call active_write_xy_loc( 'wtauv', wtauv,   1, 0, mythid, dummy)
      call active_write_xy_loc( 'wtauv2', wtauv2,   1, 0, mythid, dummy)
#elif (defined (ALLOW_VWIND_COST_CONTRIBUTION))
      call active_write_xy_loc( 'wvwind', wvwind, 1, 0, mythid, dummy)
#endif

#ifdef ALLOW_OBCSN_COST_CONTRIBUTION
#endif

      end
